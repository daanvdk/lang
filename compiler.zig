const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Pattern = @import("pattern.zig").Pattern;
const Instr = @import("instr.zig").Instr;
const Program = @import("program.zig").Program;
const Value = @import("value.zig").Value;

pub fn compile(allocator: std.mem.Allocator, expr: Expr) Compiler.Error!Program {
    var buffer = Buffer.init(allocator);
    defer buffer.deinit();

    var compiler = Compiler.init(&buffer);
    defer compiler.deinit();

    _ = try compiler.compileExpr(expr, .returned);

    const data = try buffer.content.toOwnedSlice();
    errdefer allocator.free(data);
    const instrs = try compiler.instrs.toOwnedSlice();
    errdefer allocator.free(instrs);
    const locations = try compiler.locations.toOwnedSlice();
    return .{ .instrs = instrs, .data = data, .locations = locations };
}

pub const Buffer = struct {
    content: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) Buffer {
        return .{
            .content = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Buffer) void {
        self.content.deinit();
    }

    fn add(self: *Buffer, content: []const u8) !usize {
        const index = self.content.items.len;
        try self.content.appendSlice(content);
        return index;
    }
};

pub const Compiler = struct {
    buffer: *Buffer,
    instrs: std.ArrayList(Instr),
    locations: std.ArrayList(Program.Location),
    stack: Stack,
    scope: std.StringHashMap(Info),
    caps: std.ArrayList(Cap),
    is_gen: bool = false,

    error_reason: union(enum) {
        unknown_name: []const u8,
    } = undefined,
    error_location: Instr.Location = undefined,

    pub const Error = std.mem.Allocator.Error || error{CompileError};

    pub fn init(buffer: *Buffer) Compiler {
        return .{
            .buffer = buffer,
            .instrs = std.ArrayList(Instr).init(buffer.content.allocator),
            .locations = std.ArrayList(Program.Location).init(buffer.content.allocator),
            .stack = Stack.init(buffer.content.allocator),
            .scope = std.StringHashMap(Info).init(buffer.content.allocator),
            .caps = std.ArrayList(Cap).init(buffer.content.allocator),
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.instrs.deinit();
        self.locations.deinit();
        self.stack.deinit();
        self.scope.deinit();
        self.caps.deinit();
    }

    pub fn printCompileError(self: *Compiler) void {
        switch (self.error_reason) {
            .unknown_name => |name| std.debug.print("unknown name: {s}\n", .{name}),
        }
    }

    pub fn compileExpr(self: *Compiler, expr: Expr, usage: Usage) Error!Info {
        switch (expr.data) {
            .global => |global| {
                try self.instrs.append(.{ .global = global });
                try self.compileUsage(usage);
                return .any;
            },
            .name => |name| {
                var info: Info = undefined;
                if (self.stack.get(name)) |local| {
                    try self.instrs.append(.{ .local = local.index });
                    info = local.info;
                } else if (self.scope.get(name)) |info_| {
                    try self.caps.append(.{
                        .index = self.instrs.items.len,
                        .name = name,
                    });
                    try self.instrs.append(.null);
                    info = info_;
                } else inline for (@typeInfo(Instr.Global).Enum.fields) |field| {
                    if (std.mem.eql(u8, name, field.name)) {
                        try self.instrs.append(.{ .global = @field(Instr.Global, field.name) });
                        info = .any;
                        break;
                    }
                } else {
                    self.error_reason = .{ .unknown_name = name };
                    self.error_location = expr.location;
                    return error.CompileError;
                }
                try self.compileUsage(usage);
                return info;
            },
            inline .num, .bool, .null => |value, tag| {
                if (usage != .ignored) {
                    try self.instrs.append(@unionInit(Instr, @tagName(tag), value));
                    try self.compileUsage(usage);
                }
                return .any;
            },
            .str => |content| {
                if (Value.toShort(content)) |short| {
                    try self.instrs.append(.{ .short_str = short });
                } else {
                    const index = try self.buffer.add(content);
                    try self.instrs.append(.{ .long_str = .{
                        .index = @truncate(index),
                        .len = @truncate(content.len),
                    } });
                }
                try self.compileUsage(usage);
                return .any;
            },
            inline .list, .lists => |value, tag| {
                const cols: []const Expr = switch (tag) {
                    .list => &.{expr},
                    .lists => value,
                    else => unreachable,
                };
                const start = self.stack.size();

                var i: usize = 0;
                while (i < cols.len and cols[i].data == .list) : (i += 1) {
                    for (cols[i].data.list) |item| {
                        _ = try self.compileExpr(item, .used);
                        try self.stack.push(null);
                    }
                }

                switch (cols.len - i) {
                    0 => try self.instrs.append(.nil),
                    1 => {
                        var index = self.instrs.items.len;
                        const curr = self.stack.size();
                        const info = try self.compileExpr(cols[i], .used);

                        if (!info.isList()) {
                            while (index < self.instrs.items.len) : (index += 1) {
                                switch (self.instrs.items[index]) {
                                    .local, .pop => |*j| {
                                        if (j.* >= curr) j.* += 1;
                                    },
                                    .lambda => |lambda| index += lambda.len,
                                    else => {},
                                }
                            }

                            try self.insertInstr(index, .{ .global = .list });
                            try self.instrs.append(.nil);
                            try self.instrs.append(.cons);
                            try self.instrs.append(.call);
                        }
                    },
                    else => {
                        _ = try self.compileExpr(.{
                            .data = .{ .call = &.{
                                .lhs = .{
                                    .data = .{ .global = .list },
                                    .location = .{},
                                },
                                .rhs = .{
                                    .data = .{ .list = cols[i..] },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        }, .used);
                    },
                }

                while (self.stack.size() > start) {
                    self.stack.pop();
                    try self.instrs.append(.cons);
                }

                try self.compileUsage(usage);
                return .list;
            },
            inline .dict, .dicts => |value, tag| {
                const cols: []const Expr = switch (tag) {
                    .dict => &.{expr},
                    .dicts => value,
                    else => unreachable,
                };
                var n = cols.len;
                while (n > 0 and cols[n - 1].data == .dict) n -= 1;

                switch (n) {
                    0 => try self.instrs.append(.empty_dict),
                    1 => {
                        var index = self.instrs.items.len;
                        const curr = self.stack.size();
                        const info = try self.compileExpr(cols[0], .used);

                        if (!info.isDict()) {
                            while (index < self.instrs.items.len) : (index += 1) {
                                switch (self.instrs.items[index]) {
                                    .local, .pop => |*j| {
                                        if (j.* >= curr) j.* += 1;
                                    },
                                    .lambda => |lambda| index += lambda.len,
                                    else => {},
                                }
                            }

                            try self.insertInstr(index, .{ .global = .dict });
                            try self.instrs.append(.nil);
                            try self.instrs.append(.cons);
                            try self.instrs.append(.call);
                        }
                    },
                    else => {
                        _ = try self.compileExpr(.{
                            .data = .{ .call = &.{
                                .lhs = .{
                                    .data = .{ .global = .dict },
                                    .location = .{},
                                },
                                .rhs = .{
                                    .data = .{ .list = cols[0..n] },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        }, .used);
                    },
                }

                try self.stack.push(null);
                for (cols[n..]) |col| {
                    for (col.data.dict) |pair| {
                        _ = try self.compileExpr(pair.key, .used);
                        try self.stack.push(null);
                        _ = try self.compileExpr(pair.value, .used);
                        self.stack.pop();
                        try self.instrs.append(.put_dict);
                    }
                }
                self.stack.pop();

                return .dict;
            },
            inline .lambda, .gen => |value, tag| {
                var compiler = Compiler.init(self.buffer);
                defer compiler.deinit();
                if (tag == .gen) compiler.is_gen = true;

                compiler.scope = try self.scope.clone();
                for (self.stack.entries.items) |maybe_entry| {
                    const entry = maybe_entry orelse continue;
                    try compiler.scope.put(entry.name, entry.info);
                }

                _ = try compiler.compileMatcher(switch (tag) {
                    .lambda => value.*,
                    .gen => .{
                        .pattern = .{
                            .data = .{ .list = &.{.{
                                .data = .{ .expr = &.{
                                    .data = .null,
                                    .location = .{},
                                } },
                                .location = .{},
                            }} },
                            .location = .{},
                        },
                        .expr = value.*,
                    },
                    else => unreachable,
                }, .list, null, .{}, .returned);

                var caps = std.StringHashMap(usize).init(self.instrs.allocator);
                defer caps.deinit();

                for (compiler.caps.items) |cap| {
                    const res = try caps.getOrPut(cap.name);
                    if (res.found_existing) continue;

                    _ = try self.compileExpr(.{
                        .data = .{ .name = cap.name },
                        .location = .{},
                    }, .used);
                    res.value_ptr.* = caps.count() - 1;
                }

                var index: usize = 0;
                while (index < compiler.instrs.items.len) : (index += 1) {
                    switch (compiler.instrs.items[index]) {
                        .local, .pop => |*i| i.* += caps.count(),
                        .lambda => |lambda| index += lambda.len,
                        else => {},
                    }
                }

                for (compiler.caps.items) |cap| {
                    compiler.instrs.items[cap.index] = .{ .local = caps.get(cap.name).? };
                }

                try self.instrs.append(.{ .lambda = .{
                    .caps = caps.count(),
                    .len = @truncate(compiler.instrs.items.len),
                } });

                try self.locations.ensureUnusedCapacity(compiler.locations.items.len);
                for (compiler.locations.items) |location| self.locations.appendAssumeCapacity(.{
                    .offset = location.offset + self.instrs.items.len,
                    .index = location.index,
                    .location = location.location,
                });
                try self.instrs.appendSlice(compiler.instrs.items);

                try self.compileUsage(usage);
                return .any;
            },
            inline .call, .get, .pow, .mul, .div, .add, .sub => |bin, tag| {
                _ = try self.compileExpr(bin.lhs, .used);
                try self.stack.push(null);
                _ = try self.compileExpr(bin.rhs, .used);
                self.stack.pop();
                if (tag == .call and usage == .returned and !self.is_gen) {
                    try self.instrs.append(.tail_call);
                } else {
                    try self.appendLocation(bin.lhs.location);
                    try self.appendLocation(bin.rhs.location);
                    try self.instrs.append(@field(Instr, @tagName(tag)));
                    try self.compileUsage(usage);
                }
                return .any;
            },
            inline .pos, .neg, .not => |expr_ptr, tag| {
                _ = try self.compileExpr(expr_ptr.*, .used);
                try self.appendLocation(expr_ptr.location);
                try self.instrs.append(@field(Instr, @tagName(tag)));
                try self.compileUsage(usage);
                return .any;
            },
            .cmp => |cmp_root| {
                _ = try self.compileExpr(cmp_root.lhs, .used);

                var to_end = std.ArrayList(usize).init(self.instrs.allocator);
                defer to_end.deinit();

                var lhs_location = cmp_root.lhs.location;

                for (cmp_root.cmps) |cmp| {
                    try self.stack.push(null);
                    _ = try self.compileExpr(cmp.rhs, .used);
                    self.stack.pop();
                    try self.instrs.append(.{ .local = self.stack.size() });
                    try self.instrs.append(.{ .pop = self.stack.size() });
                    try self.instrs.append(.{ .local = self.stack.size() });
                    try self.appendLocation(lhs_location);
                    try self.appendLocation(cmp.rhs.location);
                    try self.instrs.append(switch (cmp_root.last_op) {
                        inline else => |tag| @field(Instr, @tagName(tag)),
                    });

                    switch (usage) {
                        .returned => {
                            try self.instrs.append(.null);
                            const ret_start = self.instrs.items.len;
                            try self.instrs.append(.{ .bool = false });
                            try self.compileUsage(usage);
                            self.instrs.items[ret_start - 1] = .{ .jmp_if = self.instrs.items.len - ret_start };
                        },
                        .used => {
                            try self.instrs.append(.{ .jmp_if = 3 });
                            try self.instrs.append(.{ .pop = self.stack.size() });
                            try self.instrs.append(.{ .bool = false });
                            try to_end.append(self.instrs.items.len);
                            try self.instrs.append(.null);
                        },
                        .ignored => {
                            try self.instrs.append(.{ .jmp_if = 2 });
                            try self.instrs.append(.{ .pop = self.stack.size() });
                            try to_end.append(self.instrs.items.len);
                            try self.instrs.append(.null);
                        },
                    }

                    lhs_location = cmp.rhs.location;
                }

                _ = try self.compileExpr(cmp_root.last_rhs, .used);
                try self.appendLocation(lhs_location);
                try self.appendLocation(cmp_root.last_rhs.location);
                try self.instrs.append(switch (cmp_root.last_op) {
                    inline else => |tag| @field(Instr, @tagName(tag)),
                });
                try self.compileUsage(usage);

                for (to_end.items) |index| self.instrs.items[index] = .{ .jmp = self.instrs.items.len - 1 - index };

                return .any;
            },
            .@"and" => |bin| {
                var info = try self.compileExpr(bin.lhs, .used);
                switch (usage) {
                    .returned => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.null);

                        const ret_start = self.instrs.items.len;
                        try self.compileUsage(.returned);
                        self.instrs.items[ret_start - 1] = .{ .jmp_if = self.instrs.items.len - ret_start };

                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, usage));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, usage));

                        self.instrs.items[rhs_start - 1] = .{ .jmp = self.instrs.items.len - rhs_start };
                    },
                    .ignored => {
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        info = info.merge(try self.compileExpr(bin.rhs, usage));

                        self.instrs.items[rhs_start - 1] = .{ .jmp = self.instrs.items.len - rhs_start };
                    },
                }
                return info;
            },
            .@"or" => |bin| {
                var info = try self.compileExpr(bin.lhs, .used);
                switch (usage) {
                    .returned => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const ret_start = self.instrs.items.len;
                        try self.compileUsage(.returned);
                        self.instrs.items[ret_start - 1] = .{ .jmp = self.instrs.items.len - ret_start };

                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, usage));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, usage));

                        self.instrs.items[rhs_start - 1] = .{ .jmp_if = self.instrs.items.len - rhs_start };
                    },
                    .ignored => {
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        info = info.merge(try self.compileExpr(bin.rhs, usage));

                        self.instrs.items[rhs_start - 1] = .{ .jmp_if = self.instrs.items.len - rhs_start };
                    },
                }
                return info;
            },
            .match => |match| {
                var n: usize = 0;
                const no_match: ?Expr = while (n < match.matchers.len) : (n += 1) {
                    const matcher = match.matchers[n];
                    if (matcher.pattern.data == .ignore) break matcher.expr;
                } else null;

                if (n == 0) {
                    _ = try self.compileExpr(match.subject, .ignored);
                    if (no_match) |expr_| {
                        return try self.compileExpr(expr_, usage);
                    } else {
                        try self.instrs.append(.no_match);
                        return .all;
                    }
                }

                const subject_info = try self.compileExpr(match.subject, .used);
                var info = Info.all;

                var to_end = std.ArrayList(usize).init(self.instrs.allocator);
                defer to_end.deinit();

                for (0.., match.matchers[0..n]) |i, matcher| {
                    const has_next_matcher = i < n - 1;
                    const has_next = has_next_matcher or no_match != null;

                    if (has_next_matcher) {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.stack.push(null);
                    }

                    var to_next = std.ArrayList(usize).init(self.instrs.allocator);
                    defer to_next.deinit();
                    info = info.merge(try self.compileMatcher(
                        matcher,
                        subject_info,
                        if (has_next) &to_next else null,
                        match.subject.location,
                        usage,
                    ));

                    if (has_next_matcher) {
                        self.stack.pop();
                        if (usage != .returned) try self.instrs.append(.{ .pop = self.stack.size() });
                    }

                    if (has_next and usage != .returned) {
                        try to_end.append(self.instrs.items.len);
                        try self.instrs.append(.null);
                    }

                    for (to_next.items) |j| {
                        self.instrs.items[j] = .{ .jmp = self.instrs.items.len - j - 1 };
                    }
                }

                if (no_match) |expr_| {
                    info = info.merge(try self.compileExpr(expr_, usage));
                }

                for (to_end.items) |i| {
                    self.instrs.items[i] = .{ .jmp = self.instrs.items.len - i - 1 };
                }

                return info;
            },
            .@"if" => |if_| {
                _ = try self.compileExpr(if_.cond, .used);
                try self.instrs.append(.null);

                const else_index = self.instrs.items.len;
                const else_info = try self.compileExpr(if_.else_, usage);

                var then_index = self.instrs.items.len;
                const then_info = try self.compileExpr(if_.then, usage);

                const then_len = self.instrs.items.len - then_index;
                if (then_len > 0 and usage != .returned) {
                    try self.insertInstr(then_index, .{ .jmp = then_len });
                    then_index += 1;
                }

                const else_len = then_index - else_index;
                self.instrs.items[else_index - 1] = .{ .jmp_if = else_len };

                return then_info.merge(else_info);
            },
            .yield => |expr_ptr| {
                std.debug.assert(self.is_gen);
                _ = try self.compileExpr(expr_ptr.*, .used);
                try self.instrs.append(.yield);

                const start = self.stack.size();
                try self.compilePattern(.{
                    .data = .{ .list = &.{.{
                        .data = .{ .name = "value" },
                        .location = .{},
                    }} },
                    .location = .{},
                }, .list, null, start, expr_ptr.location);
                self.stack.entries.shrinkRetainingCapacity(start);

                try self.compileUsage(usage);
                return .any;
            },
            inline .@"for", .yield_all => |value, tag| {
                std.debug.assert(self.is_gen);

                try self.instrs.append(.{ .global = .send });
                try self.stack.push(null);
                _ = try self.compileExpr(switch (tag) {
                    .@"for" => value.subject,
                    .yield_all => value.*,
                    else => unreachable,
                }, .used);
                self.stack.pop();
                try self.instrs.append(.null);

                const loop_index = self.instrs.items.len;
                try self.instrs.append(.nil);
                try self.instrs.append(.cons);
                try self.instrs.append(.cons);

                if (tag == .yield_all and usage == .returned) {
                    try self.instrs.append(.tail_call);
                    return .any;
                }

                try self.instrs.append(.call);
                const start = self.stack.size();
                try self.compilePattern(.{
                    .data = .{ .list = &.{ .{
                        .data = .{ .name = "head" },
                        .location = .{},
                    }, .{
                        .data = .{ .name = "tail" },
                        .location = .{},
                    } } },
                    .location = .{},
                }, .any, null, start, .{});
                self.stack.entries.shrinkRetainingCapacity(start);
                try self.instrs.append(.{ .local = start + 1 });
                try self.instrs.append(.null);
                try self.instrs.append(.eq);
                try self.instrs.append(.null);

                const body_index = self.instrs.items.len;
                try self.instrs.append(.{ .global = .send });
                try self.instrs.append(.{ .local = start + 1 });
                try self.instrs.append(.{ .pop = start + 1 });
                try self.instrs.append(.{ .local = start });
                try self.instrs.append(.{ .pop = start });
                try self.stack.push(null);
                try self.stack.push(null);

                switch (tag) {
                    .@"for" => {
                        _ = try self.compileMatcher(value.matcher, .any, null, value.subject.location, .used);
                    },
                    .yield_all => {
                        try self.instrs.append(.yield);
                        try self.compilePattern(.{
                            .data = .{ .list = &.{.{
                                .data = .{ .name = "value" },
                                .location = .{},
                            }} },
                            .location = .{},
                        }, .any, null, start + 2, .{});
                        self.stack.entries.shrinkRetainingCapacity(start + 2);
                    },
                    else => unreachable,
                }

                self.stack.pop();
                self.stack.pop();
                try self.instrs.append(.null);

                const end_index = self.instrs.items.len;
                try self.instrs.append(.{ .pop = start + 1 });

                self.instrs.items[body_index - 1] = .{ .jmp_if = end_index - body_index };
                self.instrs.items[end_index - 1] = .{ .jmp_back = end_index - loop_index };

                try self.compileUsage(usage);
                return .any;
            },
            .@"return" => |expr_ptr| {
                _ = try self.compileExpr(expr_ptr.*, .returned);
                return .all;
            },
            .assert => |expr_ptr| {
                _ = try self.compileExpr(expr_ptr.*, .used);
                try self.instrs.append(.{ .jmp_if = 1 });
                try self.appendLocation(expr_ptr.location);
                try self.instrs.append(.no_match);
                return try self.compileExpr(.{
                    .data = .null,
                    .location = .{},
                }, usage);
            },
            .module => |stmts| {
                const allocator = self.instrs.allocator;

                // Collect what fns we have
                var fn_keys = std.StringHashMap([]const u8).init(allocator);
                defer {
                    var iter = fn_keys.valueIterator();
                    while (iter.next()) |fn_key| allocator.free(fn_key.*);
                    fn_keys.deinit();
                }

                for (stmts) |stmt| {
                    const fn_name = stmt.fn_name orelse continue;
                    const fn_key = try allocator.alloc(u8, fn_name.len + 1);
                    fn_key[0] = '@';
                    @memcpy(fn_key[1..], fn_name);
                    errdefer allocator.free(fn_key);
                    try fn_keys.put(fn_name, fn_key);
                }

                // Compile statements
                const start = self.stack.size();

                var mod_exprs = std.ArrayList(ModExpr).init(allocator);
                var mod_expr_indexes = std.StringHashMap(usize).init(allocator);
                defer {
                    for (mod_exprs.items) |mod_expr| mod_expr.expr.deinit(allocator);
                    mod_exprs.deinit();
                    mod_expr_indexes.deinit();
                }

                var pub_exprs = std.ArrayList(PubExpr).init(allocator);
                var pub_expr_indexes = std.StringHashMap(usize).init(allocator);
                defer {
                    pub_exprs.deinit();
                    pub_expr_indexes.deinit();
                }

                try self.instrs.append(.empty_dict);
                try self.stack.push(.{ .name = "@module", .info = .dict });

                var fn_index = self.instrs.items.len;

                for (stmts) |stmt| {
                    const offset = self.stack.size() - 1;

                    if (stmt.fn_name) |fn_name| {
                        try self.putMod(&mod_exprs, &mod_expr_indexes, fn_name, .{
                            .data = .{ .call = &.{
                                .lhs = .{
                                    .data = .{ .get = &.{
                                        .lhs = .{
                                            .data = .{ .name = "@module" },
                                            .location = .{},
                                        },
                                        .rhs = .{
                                            .data = .{ .str = fn_keys.get(fn_name).? },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .rhs = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "@module" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        });
                        if (stmt.is_pub) try putPub(&pub_exprs, &pub_expr_indexes, fn_name, null);
                    } else {
                        var subject = try stmt.subject.clone(allocator);
                        defer subject.deinit(allocator);

                        var i = mod_exprs.items.len;
                        while (i > 0) {
                            i -= 1;
                            const mod_expr = mod_exprs.items[i];
                            if (fn_keys.get(mod_expr.name) == null or !subject.usesName(mod_expr.name)) continue;

                            const matchers = try allocator.alloc(Expr.Matcher, 1);
                            errdefer allocator.free(matchers);
                            matchers[0] = .{
                                .pattern = .{
                                    .data = .{ .name = mod_expr.name },
                                    .location = .{},
                                },
                                .expr = subject,
                            };

                            const match = try allocator.create(Expr.Match);
                            errdefer allocator.destroy(match);
                            match.* = .{
                                .subject = try mod_expr.expr.clone(allocator),
                                .matchers = matchers,
                            };

                            subject = .{
                                .data = .{ .match = match },
                                .location = .{},
                            };
                        }

                        const info = try self.compileExpr(subject, .used);
                        try self.compilePattern(stmt.pattern, info, null, offset + 1, .{});

                        if (offset < self.stack.size() - 1) {
                            try self.instrs.append(.{ .local = offset });
                            try self.instrs.append(.{ .pop = offset });
                            const entry = self.stack.entries.orderedRemove(offset);
                            try self.stack.push(entry);
                        }

                        for (offset.., self.stack.entries.items[offset .. self.stack.size() - 1]) |index, maybe_entry| {
                            const entry = maybe_entry orelse continue;

                            _ = try self.compileExpr(.{
                                .data = .{ .str = entry.name },
                                .location = .{},
                            }, .used);
                            try self.instrs.append(.{ .local = index });
                            try self.instrs.append(.put_dict);

                            try self.putMod(&mod_exprs, &mod_expr_indexes, entry.name, .{
                                .data = .{ .get = &.{
                                    .lhs = .{
                                        .data = .{ .name = "@module" },
                                        .location = .{},
                                    },
                                    .rhs = .{
                                        .data = .{ .str = entry.name },
                                        .location = .{},
                                    },
                                } },
                                .location = .{},
                            });
                            if (stmt.is_pub) try putPub(&pub_exprs, &pub_expr_indexes, entry.name, index);
                        }
                    }
                }

                // Create the pub dict
                try self.instrs.append(.empty_dict);
                try self.stack.push(null);
                for (pub_exprs.items) |pub_expr| {
                    _ = try self.compileExpr(.{
                        .data = .{ .str = pub_expr.name },
                        .location = .{},
                    }, .used);
                    if (pub_expr.index) |local| {
                        try self.instrs.append(.{ .local = local });
                    } else {
                        try self.stack.push(null);
                        _ = try self.compileExpr(.{
                            .data = .{ .call = &.{
                                .lhs = .{
                                    .data = .{ .get = &.{
                                        .lhs = .{
                                            .data = .{ .name = "@module" },
                                            .location = .{},
                                        },
                                        .rhs = .{
                                            .data = .{ .str = fn_keys.get(pub_expr.name).? },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .rhs = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "@module" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        }, .used);
                        self.stack.pop();
                    }
                    try self.instrs.append(.put_dict);
                }
                self.stack.pop();

                while (self.stack.size() > start) {
                    self.stack.pop();
                    if (usage != .returned) try self.instrs.append(.{ .pop = self.stack.size() });
                }

                // Compile fns
                for (stmts) |stmt| {
                    const fn_name = stmt.fn_name orelse continue;
                    const fn_key = fn_keys.get(fn_name).?;

                    var body = try (Expr{
                        .data = .{ .match = &.{
                            .subject = .{
                                .data = .{ .name = "@args" },
                                .location = .{},
                            },
                            .matchers = &.{.{
                                .pattern = stmt.pattern,
                                .expr = stmt.subject,
                            }},
                        } },
                        .location = .{},
                    }).clone(allocator);
                    defer body.deinit(allocator);

                    // Wrap with all things used from the module
                    var i = mod_exprs.items.len;
                    while (i > 0) {
                        i -= 1;
                        const entry = mod_exprs.items[i];
                        if (!body.usesName(entry.name)) continue;

                        const matchers = try allocator.alloc(Expr.Matcher, 1);
                        errdefer allocator.free(matchers);
                        matchers[0] = .{
                            .pattern = .{
                                .data = .{ .name = entry.name },
                                .location = .{},
                            },
                            .expr = body,
                        };

                        const match = try allocator.create(Expr.Match);
                        errdefer allocator.destroy(match);
                        match.subject = try entry.expr.clone(allocator);
                        match.matchers = matchers;

                        body = .{
                            .data = .{ .match = match },
                            .location = .{},
                        };
                    }

                    // Compile fn
                    const fn_start = self.instrs.items.len;
                    try self.stack.push(.{ .name = "@module", .info = .dict });
                    _ = try self.compileExpr(.{
                        .data = .{ .str = fn_key },
                        .location = .{},
                    }, .used);
                    try self.stack.push(null);
                    _ = try self.compileExpr(.{
                        .data = .{ .lambda = &.{
                            .pattern = .{
                                .data = .{ .list = &.{
                                    .{
                                        .data = .{ .name = "@module" },
                                        .location = .{},
                                    },
                                } },
                                .location = .{},
                            },
                            .expr = .{
                                .data = .{ .lambda = &.{
                                    .pattern = .{
                                        .data = .{ .name = "@args" },
                                        .location = .{},
                                    },
                                    .expr = body,
                                } },
                                .location = .{},
                            },
                        } },
                        .location = .{},
                    }, .used);
                    self.stack.pop();
                    self.stack.pop();
                    try self.instrs.append(.put_dict);

                    // Move fn to the front
                    const fn_len = self.instrs.items.len - fn_start;
                    try self.moveInstrs(fn_start, fn_index, fn_len);
                    fn_index += fn_len;
                }

                try self.compileUsage(usage);
                return .dict;
            },
        }
    }

    fn putMod(
        self: *Compiler,
        mod_exprs: *std.ArrayList(ModExpr),
        mod_expr_indexes: *std.StringHashMap(usize),
        name: []const u8,
        expr: Expr,
    ) !void {
        const allocator = self.instrs.allocator;
        const mod_expr = try expr.clone(allocator);
        if (mod_expr_indexes.get(name)) |i| {
            mod_exprs.items[i].expr.deinit(allocator);
            mod_exprs.items[i].expr = mod_expr;
        } else {
            errdefer mod_expr.deinit(self.instrs.allocator);
            try mod_expr_indexes.put(name, mod_exprs.items.len);
            try mod_exprs.append(.{ .name = name, .expr = mod_expr });
        }
    }

    fn putPub(
        pub_exprs: *std.ArrayList(PubExpr),
        pub_expr_indexes: *std.StringHashMap(usize),
        name: []const u8,
        index: ?usize,
    ) !void {
        if (pub_expr_indexes.get(name)) |i| {
            pub_exprs.items[i].index = index;
        } else {
            try pub_expr_indexes.put(name, pub_exprs.items.len);
            try pub_exprs.append(.{ .name = name, .index = index });
        }
    }

    fn compileUsage(self: *Compiler, usage: Usage) Error!void {
        switch (usage) {
            .returned => {
                if (self.is_gen) {
                    try self.instrs.append(.null);
                    try self.instrs.append(.nil);
                    try self.instrs.append(.cons);
                    try self.instrs.append(.cons);
                }
                try self.instrs.append(.ret);
            },
            .used => {},
            .ignored => try self.instrs.append(.{ .pop = self.stack.size() }),
        }
    }

    fn compilePattern(
        self: *Compiler,
        pattern: Pattern,
        info: Info,
        to_next: ?*std.ArrayList(usize),
        start: usize,
        location: Instr.Location,
    ) Error!void {
        switch (pattern.data) {
            .name => |name| {
                if (self.stack.getFromIndex(start, name)) |local| {
                    try self.instrs.append(.{ .local = local.index });
                    try self.instrs.append(.eq);
                    try self.compileNoMatch(true, to_next, start, 0, location);
                } else {
                    try self.stack.push(.{ .name = name, .info = info });
                }
            },
            .ignore => {
                try self.instrs.append(.{ .pop = self.stack.size() });
            },
            .expr => |expr| {
                const old_is_gen = self.is_gen;
                defer self.is_gen = old_is_gen;
                self.is_gen = false;

                try self.stack.push(null);
                _ = try self.compileExpr(expr.*, .used);
                self.stack.pop();
                try self.instrs.append(.eq);
                try self.compileNoMatch(true, to_next, start, 0, location);
            },
            .guard => |guard| {
                try self.compilePattern(guard.pattern, info, to_next, start, location);
                _ = try self.compileExpr(guard.cond, .used);
                try self.compileNoMatch(true, to_next, start, 0, location);
            },
            inline .list, .lists => |value, tag| {
                const cols: []const Pattern = switch (tag) {
                    .list => &.{pattern},
                    .lists => value,
                    else => unreachable,
                };

                if (!info.isList()) {
                    try self.instrs.append(.{ .global = .is_list });
                    try self.instrs.append(.{ .local = self.stack.size() });
                    try self.instrs.append(.nil);
                    try self.instrs.append(.cons);
                    try self.instrs.append(.call);
                    try self.compileNoMatch(true, to_next, start, 1, location);
                }

                var i: usize = 0;
                while (i < cols.len and cols[i].data == .list) : (i += 1) {
                    for (cols[i].data.list) |item| {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.compileNoMatch(true, to_next, start, 1, location);
                        try self.instrs.append(.decons);

                        if (item.data == .name and self.stack.getFromIndex(start, item.data.name) == null) {
                            try self.stack.push(.{ .name = item.data.name, .info = .any });
                        } else if (item.data == .ignore) {
                            try self.instrs.append(.{ .pop = self.stack.size() });
                        } else {
                            const curr = self.stack.size();
                            try self.instrs.append(.{ .local = curr });
                            try self.instrs.append(.{ .pop = curr });

                            try self.stack.push(null);
                            try self.compilePattern(item, .any, to_next, start, location);
                            _ = self.stack.entries.orderedRemove(start);

                            if (self.stack.size() != curr) {
                                try self.instrs.append(.{ .local = curr });
                                try self.instrs.append(.{ .pop = curr });
                            }
                        }
                    }
                }

                switch (cols.len - i) {
                    0 => try self.compileNoMatch(false, to_next, start, 0, location),
                    1 => try self.compilePattern(cols[i], .list, to_next, start, location),
                    else => unreachable,
                }
            },
            inline .dict, .dicts => |value, tag| {
                const cols: []const Pattern = switch (tag) {
                    .dict => &.{pattern},
                    .dicts => value,
                    else => unreachable,
                };

                if (!info.isDict()) {
                    try self.instrs.append(.{ .global = .is_dict });
                    try self.instrs.append(.{ .local = self.stack.size() });
                    try self.instrs.append(.nil);
                    try self.instrs.append(.cons);
                    try self.instrs.append(.call);
                    try self.compileNoMatch(true, to_next, start, 1, location);
                }

                var i: usize = 0;
                while (i < cols.len and cols[i].data == .dict) : (i += 1) {
                    for (cols[i].data.dict) |item| {
                        try self.stack.push(null);
                        _ = try self.compileExpr(item.key, .used);
                        self.stack.pop();

                        try self.instrs.append(.{ .local = self.stack.size() + 1 });
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.in);
                        try self.compileNoMatch(true, to_next, start, 2, location);
                        try self.instrs.append(.pop_dict);

                        if (item.value.data == .name and self.stack.getFromIndex(start, item.value.data.name) == null) {
                            try self.stack.push(.{ .name = item.value.data.name, .info = .any });
                        } else if (item.value.data == .ignore) {
                            try self.instrs.append(.{ .pop = self.stack.size() });
                        } else {
                            const curr = self.stack.size();
                            try self.instrs.append(.{ .local = curr });
                            try self.instrs.append(.{ .pop = curr });

                            try self.stack.push(null);
                            try self.compilePattern(item.value, .any, to_next, start, location);
                            _ = self.stack.entries.orderedRemove(start);

                            if (self.stack.size() != curr) {
                                try self.instrs.append(.{ .local = curr });
                                try self.instrs.append(.{ .pop = curr });
                            }
                        }
                    }
                }

                switch (cols.len - i) {
                    0 => try self.compileNoMatch(false, to_next, start, 0, location),
                    1 => try self.compilePattern(cols[i], .dict, to_next, start, location),
                    else => unreachable,
                }
            },
        }
    }

    fn compileNoMatch(
        self: *Compiler,
        expect: bool,
        to_next: ?*std.ArrayList(usize),
        start: usize,
        extra: usize,
        location: Instr.Location,
    ) Error!void {
        var end = self.stack.size() + extra;
        const jmp = end - start + 1;

        if (expect) {
            try self.instrs.append(.{ .jmp_if = jmp });
        } else {
            try self.instrs.append(.{ .jmp_if = 1 });
            try self.instrs.append(.{ .jmp = jmp });
        }

        while (end > start) {
            end -= 1;
            try self.instrs.append(.{ .pop = end });
        }

        if (to_next) |to_next_| try to_next_.append(self.instrs.items.len);
        try self.appendLocation(location);
        try self.instrs.append(.no_match);
    }

    fn compileMatcher(
        self: *Compiler,
        matcher: Expr.Matcher,
        subject_info: Info,
        to_next: ?*std.ArrayList(usize),
        location: Instr.Location,
        usage: Usage,
    ) Error!Info {
        const start = self.stack.size();

        try self.compilePattern(matcher.pattern, subject_info, to_next, start, location);
        const info = try self.compileExpr(matcher.expr, usage);

        while (self.stack.size() > start) {
            self.stack.pop();
            if (usage != .returned) try self.instrs.append(.{ .pop = self.stack.size() });
        }

        return info;
    }

    fn appendLocation(self: *Compiler, location: Instr.Location) !void {
        if (location.index == 0 and location.len == 0) return;

        const locations = self.locations.items;
        const offset = self.instrs.items.len;

        var index: usize = 0;
        while (index < locations.len and locations[locations.len - 1 - index].offset == offset) index += 1;

        try self.locations.append(.{
            .offset = offset,
            .index = index,
            .location = location,
        });
    }

    fn insertInstr(self: *Compiler, index: usize, instr: Instr) !void {
        try self.instrs.insert(index, instr);

        var i = self.caps.items.len;
        while (i > 0) {
            i -= 1;
            const cap = &self.caps.items[i];
            if (cap.index < index) break;
            cap.index += 1;
        }

        i = self.locations.items.len;
        while (i > 0) {
            i -= 1;
            const location = &self.locations.items[i];
            if (location.index < index) break;
            location.index += 1;
        }
    }

    fn moveInstrs(self: *Compiler, old_index: usize, new_index: usize, len: usize) !void {
        const allocator = self.instrs.allocator;
        try moveItems(Instr, allocator, self.instrs.items, old_index, new_index, len);
        try moveIndexedItems(Cap, allocator, self.caps.items, "index", old_index, new_index, len);
        try moveIndexedItems(Program.Location, allocator, self.locations.items, "offset", old_index, new_index, len);
    }

    fn moveItems(comptime T: type, allocator: std.mem.Allocator, items: []T, old_index: usize, new_index: usize, len: usize) !void {
        if (old_index == new_index or len == 0) return;

        const data = try allocator.dupe(T, items[old_index .. old_index + len]);
        defer allocator.free(data);

        if (old_index < new_index) {
            std.mem.copyForwards(
                T,
                items[old_index..new_index],
                items[old_index + len .. new_index + len],
            );
        } else {
            std.mem.copyBackwards(
                T,
                items[new_index + len .. old_index + len],
                items[new_index..old_index],
            );
        }
        @memcpy(items[new_index .. new_index + len], data);
    }

    fn moveIndexedItems(comptime T: type, allocator: std.mem.Allocator, items: []T, comptime field: []const u8, old_index: usize, new_index: usize, len: usize) !void {
        const item_old_index = findIndex(T, items, field, old_index);
        const item_new_index = findIndex(T, items, field, new_index);
        const item_len = findIndex(T, items, field, old_index + len) - item_old_index;

        try moveItems(T, allocator, items, item_old_index, item_new_index, item_len);

        if (old_index < new_index) {
            for (items[item_old_index..item_new_index]) |*item| {
                @field(item, field) -= len;
            }
            for (items[item_new_index .. item_new_index + item_len]) |*item| {
                @field(item, field) += new_index - old_index;
            }
        } else {
            for (items[item_new_index .. item_new_index + item_len]) |*item| {
                @field(item, field) -= old_index - new_index;
            }
            for (items[item_new_index + item_len .. item_old_index + item_len]) |*item| {
                @field(item, field) += len;
            }
        }
    }

    fn findIndex(comptime T: type, items: []T, comptime field: []const u8, instr_index: usize) usize {
        var min_index: usize = 0;
        var max_index = items.len;

        while (min_index != max_index) {
            const mid_index = (min_index + max_index) / 2;
            if (instr_index <= @field(items[mid_index], field)) {
                max_index = mid_index;
            } else {
                min_index = mid_index + 1;
            }
        }

        return min_index;
    }
};

const Usage = union(enum) { returned, used, ignored };

const Info = enum {
    any,
    list,
    dict,
    all,

    fn merge(self: Info, other: Info) Info {
        if (self == other or self == .all) return other;
        if (other == .all) return self;
        return .any;
    }

    fn isList(self: Info) bool {
        return self == .list or self == .all;
    }

    fn isDict(self: Info) bool {
        return self == .dict or self == .all;
    }
};

const Stack = struct {
    entries: std.ArrayList(?Entry),

    fn init(allocator: std.mem.Allocator) Stack {
        return .{
            .entries = std.ArrayList(?Entry).init(allocator),
        };
    }

    fn deinit(self: *Stack) void {
        self.entries.deinit();
    }

    inline fn size(self: *Stack) usize {
        return self.entries.items.len;
    }

    fn push(self: *Stack, entry: ?Entry) !void {
        try self.entries.append(entry);
    }

    fn pop(self: *Stack) void {
        _ = self.entries.pop();
    }

    fn get(self: *Stack, name: []const u8) ?GetResult {
        return self.getFromIndex(0, name);
    }

    fn getFromIndex(self: *Stack, min_index: usize, name: []const u8) ?GetResult {
        var index = self.size();
        while (index > min_index) {
            index -= 1;
            const entry = self.entries.items[index] orelse continue;
            if (std.mem.eql(u8, entry.name, name)) {
                return .{ .index = index, .info = entry.info };
            }
        }
        return null;
    }

    const Entry = struct {
        name: []const u8,
        info: Info,
    };

    const GetResult = struct {
        index: usize,
        info: Info,
    };
};

const Cap = struct {
    index: usize,
    name: []const u8,
};

const ModExpr = struct {
    name: []const u8,
    expr: Expr,
};

const PubExpr = struct {
    name: []const u8,
    index: ?usize,
};

test "compile num 1" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .num = 1337 },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1337 },
        .ret,
    }, program.instrs);
}

test "compile num 2" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .num = 45.67 },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 45.67 },
        .ret,
    }, program.instrs);
}

test "compile bool 1" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .bool = true },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = true },
        .ret,
    }, program.instrs);
}

test "compile bool 2" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .bool = false },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = false },
        .ret,
    }, program.instrs);
}

test "compile null" {
    const program = try compile(std.testing.allocator, .{
        .data = .null,
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .null,
        .ret,
    }, program.instrs);
}

test "compile operators" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .add = &.{
            .lhs = .{
                .data = .{ .mul = &.{
                    .lhs = .{
                        .data = .{ .num = 1 },
                        .location = .{ .index = 0, .len = 1 },
                    },
                    .rhs = .{
                        .data = .{ .num = 2 },
                        .location = .{ .index = 4, .len = 1 },
                    },
                } },
                .location = .{ .index = 0, .len = 5 },
            },
            .rhs = .{
                .data = .{ .div = &.{
                    .lhs = .{
                        .data = .{ .num = 3 },
                        .location = .{ .index = 8, .len = 1 },
                    },
                    .rhs = .{
                        .data = .{ .neg = &.{
                            .data = .{ .num = 4 },
                            .location = .{ .index = 13, .len = 1 },
                        } },
                        .location = .{ .index = 12, .len = 2 },
                    },
                } },
                .location = .{ .index = 8, .len = 6 },
            },
        } },
        .location = .{ .index = 0, .len = 14 },
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1 },
        .{ .num = 2 },
        .mul,
        .{ .num = 3 },
        .{ .num = 4 },
        .neg,
        .div,
        .add,
        .ret,
    }, program.instrs);

    try std.testing.expectEqualSlices(Program.Location, &.{
        .{ .offset = 2, .index = 0, .location = .{ .index = 0, .len = 1 } },
        .{ .offset = 2, .index = 1, .location = .{ .index = 4, .len = 1 } },
        .{ .offset = 5, .index = 0, .location = .{ .index = 13, .len = 1 } },
        .{ .offset = 6, .index = 0, .location = .{ .index = 8, .len = 1 } },
        .{ .offset = 6, .index = 1, .location = .{ .index = 12, .len = 2 } },
        .{ .offset = 7, .index = 0, .location = .{ .index = 0, .len = 5 } },
        .{ .offset = 7, .index = 1, .location = .{ .index = 8, .len = 6 } },
    }, program.locations);
}

test "compile var" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .num = 4 },
                .location = .{},
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "x" },
                    .location = .{},
                },
                .expr = .{
                    .data = .{ .mul = &.{
                        .lhs = .{
                            .data = .{ .name = "x" },
                            .location = .{},
                        },
                        .rhs = .{
                            .data = .{ .name = "x" },
                            .location = .{},
                        },
                    } },
                    .location = .{},
                },
            }},
        } },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 4 },
        .{ .local = 0 },
        .{ .local = 0 },
        .mul,
        .ret,
    }, program.instrs);
}

test "compile lambda" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .lambda = &.{
                    .pattern = .{
                        .data = .{ .list = &.{
                            .{
                                .data = .{ .name = "x" },
                                .location = .{},
                            },
                        } },
                        .location = .{},
                    },
                    .expr = .{
                        .data = .{ .mul = &.{
                            .lhs = .{
                                .data = .{ .name = "x" },
                                .location = .{},
                            },
                            .rhs = .{
                                .data = .{ .name = "x" },
                                .location = .{},
                            },
                        } },
                        .location = .{},
                    },
                } },
                .location = .{},
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "sqr" },
                    .location = .{},
                },
                .expr = .{
                    .data = .{ .call = &.{
                        .lhs = .{
                            .data = .{ .name = "sqr" },
                            .location = .{},
                        },
                        .rhs = .{
                            .data = .{ .list = &.{
                                .{
                                    .data = .{ .num = 4 },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        },
                    } },
                    .location = .{},
                },
            }},
        } },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .lambda = .{ .caps = 0, .len = 13 } },
        .{ .local = 0 },
        .{ .jmp_if = 2 },
        .{ .pop = 0 },
        .no_match,
        .decons,
        .{ .jmp_if = 1 },
        .{ .jmp = 2 },
        .{ .pop = 0 },
        .no_match,
        .{ .local = 0 },
        .{ .local = 0 },
        .mul,
        .ret,
        .{ .local = 0 },
        .{ .num = 4 },
        .nil,
        .cons,
        .tail_call,
    }, program.instrs);
}

test "compile closure" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .num = 2 },
                .location = .{},
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "n" },
                    .location = .{},
                },
                .expr = .{
                    .data = .{ .match = &.{
                        .subject = .{
                            .data = .{ .lambda = &.{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "x" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .expr = .{
                                    .data = .{ .mul = &.{
                                        .lhs = .{
                                            .data = .{ .name = "x" },
                                            .location = .{},
                                        },
                                        .rhs = .{
                                            .data = .{ .name = "n" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                            } },
                            .location = .{},
                        },
                        .matchers = &.{.{
                            .pattern = .{
                                .data = .{ .name = "times_n" },
                                .location = .{},
                            },
                            .expr = .{
                                .data = .{ .call = &.{
                                    .lhs = .{
                                        .data = .{ .name = "times_n" },
                                        .location = .{},
                                    },
                                    .rhs = .{
                                        .data = .{ .list = &.{
                                            .{
                                                .data = .{ .num = 4 },
                                                .location = .{},
                                            },
                                        } },
                                        .location = .{},
                                    },
                                } },
                                .location = .{},
                            },
                        }},
                    } },
                    .location = .{},
                },
            }},
        } },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 2 },
        .{ .local = 0 },
        .{ .lambda = .{ .caps = 1, .len = 13 } },
        .{ .local = 1 },
        .{ .jmp_if = 2 },
        .{ .pop = 1 },
        .no_match,
        .decons,
        .{ .jmp_if = 1 },
        .{ .jmp = 2 },
        .{ .pop = 1 },
        .no_match,
        .{ .local = 1 },
        .{ .local = 0 },
        .mul,
        .ret,
        .{ .local = 1 },
        .{ .num = 4 },
        .nil,
        .cons,
        .tail_call,
    }, program.instrs);
}

test "compile dict" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .dict = &.{
                    .{
                        .key = .{
                            .data = .{ .str = "foo" },
                            .location = .{},
                        },
                        .value = .{
                            .data = .{ .num = 1 },
                            .location = .{},
                        },
                    },
                    .{
                        .key = .{
                            .data = .{ .str = "bar" },
                            .location = .{},
                        },
                        .value = .{
                            .data = .{ .num = 2 },
                            .location = .{},
                        },
                    },
                    .{
                        .key = .{
                            .data = .{ .str = "baz" },
                            .location = .{},
                        },
                        .value = .{
                            .data = .{ .num = 3 },
                            .location = .{},
                        },
                    },
                } },
                .location = .{},
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .dicts = &.{
                        .{
                            .data = .{ .dict = &.{
                                .{
                                    .key = .{
                                        .data = .{ .str = "foo" },
                                        .location = .{},
                                    },
                                    .value = .{
                                        .data = .{ .name = "foo" },
                                        .location = .{},
                                    },
                                },
                            } },
                            .location = .{},
                        },
                        .{
                            .data = .ignore,
                            .location = .{},
                        },
                    } },
                    .location = .{},
                },
                .expr = .{
                    .data = .{ .name = "foo" },
                    .location = .{},
                },
            }},
        } },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .empty_dict,
        .{ .short_str = Value.toShort("foo").? },
        .{ .num = 1 },
        .put_dict,
        .{ .short_str = Value.toShort("bar").? },
        .{ .num = 2 },
        .put_dict,
        .{ .short_str = Value.toShort("baz").? },
        .{ .num = 3 },
        .put_dict,
        .{ .short_str = Value.toShort("foo").? },
        .{ .local = 1 },
        .{ .local = 0 },
        .in,
        .{ .jmp_if = 3 },
        .{ .pop = 1 },
        .{ .pop = 0 },
        .no_match,
        .pop_dict,
        .{ .pop = 1 },
        .{ .local = 0 },
        .ret,
    }, program.instrs);
}

test "compile module" {
    const program = try compile(std.testing.allocator, .{
        .data = .{ .module = &.{
            .{
                .fn_name = null,
                .is_pub = false,
                .pattern = .{
                    .data = .{ .name = "A" },
                    .location = .{},
                },
                .subject = .{
                    .data = .{ .num = 0 },
                    .location = .{},
                },
            },
            .{
                .fn_name = null,
                .is_pub = false,
                .pattern = .{
                    .data = .{ .name = "B" },
                    .location = .{},
                },
                .subject = .{
                    .data = .{ .num = 1 },
                    .location = .{},
                },
            },
            .{
                .fn_name = "fib",
                .is_pub = true,
                .pattern = .{
                    .data = .{ .lists = &.{
                        .{
                            .data = .{ .name = "args" },
                            .location = .{},
                        },
                    } },
                    .location = .{},
                },
                .subject = .{
                    .data = .{ .match = &.{
                        .subject = .{
                            .data = .{ .name = "args" },
                            .location = .{},
                        },
                        .matchers = &.{
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .expr = &.{
                                                .data = .{ .num = 0 },
                                                .location = .{},
                                            } },
                                            .location = .{},
                                        },
                                        .{
                                            .data = .{ .name = "a" },
                                            .location = .{},
                                        },
                                        .{
                                            .data = .ignore,
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .expr = .{
                                    .data = .{ .name = "a" },
                                    .location = .{},
                                },
                            },
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "n" },
                                            .location = .{},
                                        },
                                        .{
                                            .data = .{ .name = "a" },
                                            .location = .{},
                                        },
                                        .{
                                            .data = .{ .name = "b" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .expr = .{
                                    .data = .{ .call = &.{
                                        .lhs = .{
                                            .data = .{ .name = "fib" },
                                            .location = .{},
                                        },
                                        .rhs = .{
                                            .data = .{ .list = &.{
                                                .{
                                                    .data = .{ .sub = &.{
                                                        .lhs = .{
                                                            .data = .{ .name = "n" },
                                                            .location = .{},
                                                        },
                                                        .rhs = .{
                                                            .data = .{ .num = 1 },
                                                            .location = .{},
                                                        },
                                                    } },
                                                    .location = .{},
                                                },
                                                .{
                                                    .data = .{ .name = "b" },
                                                    .location = .{},
                                                },
                                                .{
                                                    .data = .{ .add = &.{
                                                        .lhs = .{
                                                            .data = .{ .name = "a" },
                                                            .location = .{},
                                                        },
                                                        .rhs = .{
                                                            .data = .{ .name = "b" },
                                                            .location = .{},
                                                        },
                                                    } },
                                                    .location = .{},
                                                },
                                            } },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                            },
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "n" },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                                .expr = .{
                                    .data = .{ .call = &.{
                                        .lhs = .{
                                            .data = .{ .name = "fib" },
                                            .location = .{},
                                        },
                                        .rhs = .{
                                            .data = .{ .list = &.{
                                                .{
                                                    .data = .{ .name = "n" },
                                                    .location = .{},
                                                },
                                                .{
                                                    .data = .{ .name = "A" },
                                                    .location = .{},
                                                },
                                                .{
                                                    .data = .{ .name = "B" },
                                                    .location = .{},
                                                },
                                            } },
                                            .location = .{},
                                        },
                                    } },
                                    .location = .{},
                                },
                            },
                        },
                    } },
                    .location = .{},
                },
            },
        } },
        .location = .{},
    });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .empty_dict,
        // @fib
        .{ .short_str = Value.toShort("@fib").? },
        .{ .lambda = .{ .caps = 0, .len = 114 } },
        .{ .local = 0 },
        .{ .jmp_if = 2 },
        .{ .pop = 0 },
        .no_match,
        .decons,
        .{ .jmp_if = 1 },
        .{ .jmp = 2 },
        .{ .pop = 0 },
        .no_match,
        .{ .local = 0 },
        .{ .lambda = .{ .caps = 1, .len = 102 } },
        // A
        .{ .local = 0 },
        .{ .short_str = Value.toShort("A").? },
        .get,
        // B
        .{ .local = 0 },
        .{ .short_str = Value.toShort("B").? },
        .get,
        // fib
        .{ .local = 0 },
        .{ .short_str = Value.toShort("@fib").? },
        .get,
        .{ .local = 0 },
        .nil,
        .cons,
        .call,
        .{ .local = 1 },
        // start match
        .{ .local = 5 },
        // [0, a, _]
        .{ .local = 6 },
        .{ .local = 7 },
        .{ .jmp_if = 2 },
        .{ .pop = 7 },
        .{ .jmp = 26 },
        .decons,
        .{ .local = 7 },
        .{ .pop = 7 },
        .{ .num = 0 },
        .eq,
        .{ .jmp_if = 2 },
        .{ .pop = 7 },
        .{ .jmp = 18 },
        .{ .local = 7 },
        .{ .jmp_if = 2 },
        .{ .pop = 7 },
        .{ .jmp = 14 },
        .decons,
        .{ .local = 8 },
        .{ .jmp_if = 3 },
        .{ .pop = 8 },
        .{ .pop = 7 },
        .{ .jmp = 8 },
        .decons,
        .{ .pop = 8 },
        .{ .jmp_if = 1 },
        .{ .jmp = 2 },
        .{ .pop = 7 },
        .{ .jmp = 2 },
        .{ .local = 7 },
        .ret,
        // [n, a, b]
        .{ .local = 6 },
        .{ .local = 7 },
        .{ .jmp_if = 2 },
        .{ .pop = 7 },
        .{ .jmp = 33 },
        .decons,
        .{ .local = 8 },
        .{ .jmp_if = 3 },
        .{ .pop = 8 },
        .{ .pop = 7 },
        .{ .jmp = 27 },
        .decons,
        .{ .local = 9 },
        .{ .jmp_if = 4 },
        .{ .pop = 9 },
        .{ .pop = 8 },
        .{ .pop = 7 },
        .{ .jmp = 20 },
        .decons,
        .{ .jmp_if = 1 },
        .{ .jmp = 4 },
        .{ .pop = 9 },
        .{ .pop = 8 },
        .{ .pop = 7 },
        .{ .jmp = 13 },
        // @module @args A B fib args subject n a b
        .{ .local = 4 },
        .{ .local = 7 },
        .{ .num = 1 },
        .sub,
        .{ .local = 9 },
        .{ .local = 8 },
        .{ .local = 9 },
        .add,
        .nil,
        .cons,
        .cons,
        .cons,
        .tail_call,
        // [n]
        .{ .local = 6 },
        .{ .jmp_if = 2 },
        .{ .pop = 6 },
        .no_match,
        .decons,
        .{ .jmp_if = 1 },
        .{ .jmp = 2 },
        .{ .pop = 6 },
        .no_match,
        .{ .local = 4 },
        .{ .local = 6 },
        .{ .local = 2 },
        .{ .local = 3 },
        .nil,
        .cons,
        .cons,
        .cons,
        .tail_call,
        // end match
        .ret,
        .put_dict,

        // A
        .{ .num = 0 },

        .{ .local = 0 },
        .{ .pop = 0 },
        .{ .short_str = Value.toShort("A").? },
        .{ .local = 0 },
        .put_dict,

        // B
        .{ .num = 1 },

        .{ .local = 1 },
        .{ .pop = 1 },
        .{ .short_str = Value.toShort("B").? },
        .{ .local = 1 },
        .put_dict,

        // pub
        .empty_dict,

        .{ .short_str = Value.toShort("fib").? },
        .{ .local = 2 },
        .{ .short_str = Value.toShort("@fib").? },
        .get,
        .{ .local = 2 },
        .nil,
        .cons,
        .call,
        .put_dict,

        .ret,
    }, program.instrs);
}
