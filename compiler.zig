const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Pattern = @import("pattern.zig").Pattern;
const Instr = @import("instr.zig").Instr;
const Program = @import("program.zig").Program;

pub fn compile(allocator: std.mem.Allocator, expr: Expr) Compiler.Error!Program {
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    _ = try compiler.compileExpr(expr, .returned);

    const instrs = try compiler.instrs.toOwnedSlice();
    return .{ .instrs = instrs };
}

const Compiler = struct {
    instrs: std.ArrayList(Instr),
    stack: Stack,

    const Error = std.mem.Allocator.Error || error{CompileError};

    fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .instrs = std.ArrayList(Instr).init(allocator),
            .stack = Stack.init(allocator),
        };
    }

    fn deinit(self: *Compiler) void {
        self.instrs.deinit();
        self.stack.deinit();
    }

    fn compileExpr(self: *Compiler, expr: Expr, usage: Usage) Error!Info {
        switch (expr) {
            .name => |name| {
                var info: Info = undefined;
                if (self.stack.get(name)) |local| {
                    try self.instrs.append(.{ .local = local.index });
                    info = local.info;
                } else {
                    std.debug.print("unknown name: {s}\n", .{name});
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
            inline .pow, .mul, .div, .add, .sub, .eq, .ne, .lt, .le, .gt, .ge => |bin, tag| {
                _ = try self.compileExpr(bin.lhs, .used);
                try self.stack.push(null);
                _ = try self.compileExpr(bin.rhs, .used);
                self.stack.pop();
                try self.instrs.append(@field(Instr, @tagName(tag)));
                try self.compileUsage(usage);
                return .any;
            },
            inline .pos, .neg, .not => |expr_ptr, tag| {
                _ = try self.compileExpr(expr_ptr.*, .used);
                try self.instrs.append(@field(Instr, @tagName(tag)));
                try self.compileUsage(usage);
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
                        info = info.merge(try self.compileExpr(bin.rhs, .returned));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .used));

                        self.instrs.items[rhs_start - 1] = .{ .jmp = self.instrs.items.len - rhs_start };
                    },
                    .ignored => {
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        info = info.merge(try self.compileExpr(bin.rhs, .ignored));

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
                        info = info.merge(try self.compileExpr(bin.rhs, .returned));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .used));

                        self.instrs.items[rhs_start - 1] = .{ .jmp_if = self.instrs.items.len - rhs_start };
                    },
                    .ignored => {
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        info = info.merge(try self.compileExpr(bin.rhs, .ignored));

                        self.instrs.items[rhs_start - 1] = .{ .jmp_if = self.instrs.items.len - rhs_start };
                    },
                }
                return info;
            },
            .match => |match| {
                var n: usize = 0;
                const no_match: ?Expr = while (n < match.matchers.len) : (n += 1) {
                    const matcher = match.matchers[n];
                    if (matcher.pattern == .ignore) break matcher.expr;
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
                    const has_next = i < n - 1;

                    if (has_next) {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.stack.push(null);
                    }

                    const start = self.stack.size();
                    const pattern_start = self.instrs.items.len;
                    try self.compilePattern(matcher.pattern, subject_info, start);
                    const pattern_end = self.instrs.items.len;
                    info = info.merge(try self.compileExpr(matcher.expr, usage));

                    while (self.stack.size() > start) {
                        self.stack.pop();
                        if (usage != .returned) try self.instrs.append(.{ .pop = self.stack.size() });
                    }

                    if (has_next) {
                        self.stack.pop();
                        if (usage != .returned) try self.instrs.append(.{ .pop = self.stack.size() });
                    }

                    if ((has_next or no_match != null) and usage != .returned) {
                        try to_end.append(self.instrs.items.len);
                        try self.instrs.append(.null);
                    }

                    for (pattern_start..pattern_end) |j| {
                        switch (self.instrs.items[j]) {
                            .no_match => self.instrs.items[j] = .{ .jmp = self.instrs.items.len - j - 1 },
                            else => {},
                        }
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
        }
    }

    fn compileUsage(self: *Compiler, usage: Usage) Error!void {
        switch (usage) {
            .returned => try self.instrs.append(.ret),
            .used => {},
            .ignored => try self.instrs.append(.{ .pop = self.stack.size() }),
        }
    }

    fn compilePattern(self: *Compiler, pattern: Pattern, info: Info, start: usize) Error!void {
        switch (pattern) {
            .name => |name| {
                if (self.stack.getFromIndex(start, name)) |local| {
                    try self.instrs.append(.{ .local = local.index });
                    try self.instrs.append(.eq);
                    try self.compileNoMatch(true, start, 0);
                } else {
                    try self.stack.push(.{ .name = name, .info = info });
                }
            },
            .ignore => {
                try self.instrs.append(.{ .pop = self.stack.size() });
            },
        }
    }

    fn compileNoMatch(self: *Compiler, expect: bool, start: usize, extra: usize) Error!void {
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

        try self.instrs.append(.no_match);
    }
};

const Usage = enum { returned, used, ignored };
const Info = enum {
    any,
    all,

    fn merge(self: Info, other: Info) Info {
        if (self == other or self == .all) return other;
        if (other == .all) return self;
        return .any;
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

test "compile num 1" {
    const program = try compile(std.testing.allocator, .{ .num = 1337 });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1337 },
        .ret,
    }, program.instrs);
}

test "compile num 2" {
    const program = try compile(std.testing.allocator, .{ .num = 45.67 });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 45.67 },
        .ret,
    }, program.instrs);
}

test "compile bool 1" {
    const program = try compile(std.testing.allocator, .{ .bool = true });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = true },
        .ret,
    }, program.instrs);
}

test "compile bool 2" {
    const program = try compile(std.testing.allocator, .{ .bool = false });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = false },
        .ret,
    }, program.instrs);
}

test "compile null" {
    const program = try compile(std.testing.allocator, .null);
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .null,
        .ret,
    }, program.instrs);
}

test "compile operators" {
    const program = try compile(std.testing.allocator, .{ .add = &.{
        .lhs = .{ .mul = &.{
            .lhs = .{ .num = 1 },
            .rhs = .{ .num = 2 },
        } },
        .rhs = .{ .div = &.{
            .lhs = .{ .num = 3 },
            .rhs = .{ .neg = &.{ .num = 4 } },
        } },
    } });
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
}

test "compile var" {
    const program = try compile(std.testing.allocator, .{ .match = &.{
        .subject = .{ .num = 4 },
        .matchers = &.{.{
            .pattern = .{ .name = "x" },
            .expr = .{ .mul = &.{
                .lhs = .{ .name = "x" },
                .rhs = .{ .name = "x" },
            } },
        }},
    } });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 4 },
        .{ .local = 0 },
        .{ .local = 0 },
        .mul,
        .ret,
    }, program.instrs);
}
