const std = @import("std");

const Instr = @import("instr.zig").Instr;
const Runner = @import("runner.zig").Runner;

pub const Value = union(enum) {
    num: f64,
    bool: bool,
    null,
    str: [8]u8,
    nil,
    builtin: *const fn (*Runner, ?*Cons) Runner.Error!Value,
    obj: *Obj,

    pub fn eql(self: Value, other: Value) bool {
        if (self.toStr()) |self_content| {
            const other_content = other.toStr() orelse return false;
            return std.mem.eql(u8, self_content, other_content);
        }

        return switch (self) {
            .num => |value| other == .num and value == other.num,
            .bool => |value| other == .bool and value == other.bool,
            .null => other == .null,
            .str => unreachable,
            .nil => other == .nil,
            .builtin => |value| other == .builtin and value == other.builtin,
            .obj => |obj| other == .obj and (obj == other.obj or
                (obj.type == other.obj.type and switch (obj.type) {
                .str => unreachable,
                inline else => |obj_type| obj_type.detailed(obj).eql(obj_type.detailed(other.obj)),
            })),
        };
    }

    pub fn truthy(self: Value) bool {
        return switch (self) {
            .num => |value| value != 0,
            .bool => |value| value,
            .null => false,
            .str => |value| value[0] != 0,
            .nil => false,
            .builtin => false,
            .obj => |obj| switch (obj.type) {
                inline else => |obj_type| obj_type.detailed(obj).truthy(),
            },
        };
    }

    pub fn mark(self: Value) void {
        switch (self) {
            .obj => |obj| obj.mark(),
            else => {},
        }
    }

    pub fn toStr(self: *const Value) ?[]const u8 {
        return switch (self.*) {
            .str => |*short| fromShort(short),
            .obj => |obj| switch (obj.type) {
                .str => ObjType.str.detailed(obj).content,
                else => null,
            },
            else => null,
        };
    }

    pub fn writeStr(content: []const u8, writer: anytype) !void {
        try writer.print("\"", .{});
        for (content) |char| {
            switch (char) {
                '\"' => try writer.print("\\\"", .{}),
                '\\' => try writer.print("\\\\", .{}),
                '\n' => try writer.print("\\n", .{}),
                '\t' => try writer.print("\\t", .{}),
                '\r' => try writer.print("\\r", .{}),
                else => {
                    if (std.ascii.isPrint(char)) {
                        try writer.print("{c}", .{char});
                    } else {
                        try writer.print("\\x{X}", .{char});
                    }
                },
            }
        }
        try writer.print("\"", .{});
    }

    pub fn fromShort(short: *const [8]u8) []const u8 {
        var len: usize = 0;
        while (len < 8 and short[len] != 0) len += 1;
        return short[0..len];
    }

    pub fn toShort(content: []const u8) ?[8]u8 {
        if (content.len > 8) return null;
        var short: [8]u8 = undefined;
        inline for (0..8) |i| {
            if (i >= content.len) {
                short[i] = 0;
            } else if (content[i] == 0) {
                return null;
            } else {
                short[i] = content[i];
            }
        }
        return short;
    }

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .num => |value| try writer.print("{d}", .{value}),
            .bool => |value| {
                if (value) {
                    try writer.print("true", .{});
                } else {
                    try writer.print("false", .{});
                }
            },
            .null => try writer.print("null", .{}),
            .str => |*short| try writeStr(fromShort(short), writer),
            .nil => try writer.print("[]", .{}),
            .builtin => try writer.print("<builtin>", .{}),
            .obj => |obj| switch (obj.type) {
                inline else => |obj_type| try obj_type.detailed(obj).write(writer),
            },
        }
    }

    pub fn hash(self: Value) u64 {
        var hasher = Hasher{};
        if (self == .str) {
            hasher.add(@intFromEnum(Value.obj));
            hasher.add(@intFromEnum(ObjType.str));
            for (fromShort(&self.str)) |char| hasher.add(char);
        } else {
            hasher.add(@intFromEnum(self));
            switch (self) {
                .num => |value| hasher.add(@bitCast(value)),
                .bool => |value| hasher.add(if (value) 1 else 0),
                .null => {},
                .str => unreachable,
                .nil => {},
                .builtin => |value| hasher.add(@intFromPtr(value)),
                .obj => |obj| hasher.add(obj.hash),
            }
        }
        return hasher.hash;
    }

    pub const Obj = struct {
        type: ObjType,
        prev: ?*Obj,
        seen: bool,
        hash: u64,

        pub fn toValue(self: *Obj) Value {
            return .{ .obj = self };
        }

        pub fn mark(self: *Obj) void {
            if (self.seen) return;
            self.seen = true;
            switch (self.type) {
                inline else => |obj_type| obj_type.detailed(self).mark(),
            }
        }
    };

    pub const ObjType = enum {
        program,
        cons,
        dict,
        lambda,
        str,

        pub fn Detail(comptime self: ObjType) type {
            return switch (self) {
                .program => Program,
                .cons => Cons,
                .dict => Dict,
                .lambda => Lambda,
                .str => Str,
            };
        }

        pub fn detailed(comptime self: ObjType, obj: *Obj) *self.Detail() {
            return @fieldParentPtr("obj", obj);
        }
    };

    pub const Program = struct {
        obj: Obj = undefined,
        path: []const u8,
        instrs: []const Instr,
        data: []const u8,

        pub inline fn eql(_: *Program, _: *Program) bool {
            return false;
        }

        pub inline fn truthy(_: *Program) bool {
            return true;
        }

        pub fn allocated(self: *Program) usize {
            return @sizeOf(u8) * self.path.len + @sizeOf(Instr) * self.instrs.len + @sizeOf(u8) * self.data.len;
        }

        pub fn mark(_: *Program) void {}

        pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
            allocator.free(self.path);
            allocator.free(self.instrs);
            allocator.free(self.data);
        }

        pub fn write(_: *Program, writer: anytype) !void {
            try writer.print("<program>", .{});
        }

        pub fn hash(self: *const Program, hasher: *Hasher) void {
            hasher.add(@intFromPtr(self.instrs.ptr));
            hasher.add(@intFromPtr(self.data.ptr));
        }
    };

    pub const Cons = struct {
        obj: Obj = undefined,
        head: Value,
        tail: ?*Cons,

        pub fn eql(self: *Cons, other: *Cons) bool {
            var self_ = self;
            var other_ = other;
            while (true) {
                if (!self_.head.eql(other_.head)) return false;
                self_ = self_.tail orelse return other_.tail == null;
                other_ = other_.tail orelse return false;
            }
        }

        pub inline fn truthy(_: *Cons) bool {
            return true;
        }

        pub fn allocated(_: *Cons) usize {
            return 0;
        }

        pub fn mark(self: *Cons) void {
            self.head.mark();
            if (self.tail) |tail| tail.obj.mark();
        }

        pub fn deinit(_: *Cons, _: std.mem.Allocator) void {}

        pub fn write(self: *Cons, writer: anytype) !void {
            try writer.print("[{}", .{self.head});
            var tail = self.tail;
            while (tail) |cons| {
                try writer.print(", {}", .{cons.head});
                tail = cons.tail;
            }
            try writer.print("]", .{});
        }

        pub inline fn toValue(self: ?*Cons) Value {
            const cons = self orelse return .nil;
            return cons.obj.toValue();
        }

        pub fn expectOne(self: ?*Cons) !Value {
            var iter_ = iter(self);
            return try iter_.expectOne();
        }

        pub fn expectN(self: ?*Cons, comptime n: usize) ![n]Value {
            var iter_ = iter(self);
            return try iter_.expectN(n);
        }

        pub fn iter(self: ?*Cons) Iter {
            return .{ .tail = self };
        }

        pub fn hash(self: *const Cons, hasher: *Hasher) void {
            hasher.add(self.head.hash());
            if (self.tail) |cons| {
                hasher.add(1);
                hasher.add(cons.obj.hash);
            } else {
                hasher.add(0);
            }
        }

        pub const Iter = struct {
            tail: ?*Cons,

            pub fn next(self: *Iter) ?Value {
                const cons = self.tail orelse return null;
                self.tail = cons.tail;
                return cons.head;
            }

            pub fn expectNext(self: *Iter) !Value {
                return self.next() orelse error.RunError;
            }

            pub fn expectEnd(self: *Iter) !void {
                if (self.tail != null) return error.RunError;
            }

            pub fn expectOne(self: *Iter) !Value {
                const value = try self.expectNext();
                try self.expectEnd();
                return value;
            }

            pub fn expectN(self: *Iter, comptime n: usize) ![n]Value {
                var values: [n]Value = undefined;
                inline for (0..n) |i| values[i] = try self.expectNext();
                try self.expectEnd();
                return values;
            }
        };
    };

    pub const Dict = struct {
        pub const bits = 4;
        pub const len = 1 << bits;
        pub const mask = len - 1;
        pub const levels = (64 / bits) + (if (64 % bits == 0) 0 else 1);
        pub const empty_data = [_]?*Obj{null} ** len;

        obj: Obj = undefined,
        data: [len]?*Obj,

        pub fn eql(self: *Dict, other: *Dict) bool {
            for (self.data, other.data) |maybe_self_obj, maybe_other_obj| {
                const self_obj = maybe_self_obj orelse {
                    if (maybe_other_obj != null) return false;
                    continue;
                };
                const other_obj = maybe_other_obj orelse return false;

                switch (self_obj.type) {
                    .dict => {
                        const self_cons = ObjType.dict.detailed(self_obj);
                        const other_cons = ObjType.dict.detailed(other_obj);
                        if (!self_cons.eql(other_cons)) return false;
                    },
                    .cons => {
                        var self_cons = ObjType.cons.detailed(self_obj);
                        var other_cons = ObjType.cons.detailed(other_obj);

                        // we cant use normal equals since we dont want the
                        // ordering to matter

                        // First we check if all items in self are in other
                        // and we count how many items are in self
                        var items: usize = 0;
                        while (true) {
                            const item = self_cons.head;
                            var other_cons_ = other_cons;
                            const item_in_other = while (true) {
                                if (other_cons_.head.eql(item)) break true;
                                other_cons_ = other_cons_.tail orelse break false;
                            };
                            if (!item_in_other) return false;

                            items += 1;
                            self_cons = self_cons.tail orelse break;
                        }

                        // Now we check other has the same amount of items
                        while (items > 0) {
                            items -= 1;
                            other_cons = other_cons.tail orelse break;
                        } else return false;
                        if (items > 0) return false;
                    },
                    else => unreachable,
                }
            }

            return true;
        }

        pub fn truthy(self: *Dict) bool {
            for (self.data) |maybe_obj| {
                if (maybe_obj != null) return true;
            }
            return false;
        }

        pub fn allocated(_: *Dict) usize {
            return 0;
        }

        pub fn mark(self: *Dict) void {
            for (self.data) |maybe_obj| {
                const obj = maybe_obj orelse continue;
                obj.mark();
            }
        }

        pub fn deinit(_: *Dict, _: std.mem.Allocator) void {}

        pub fn write(self: *Dict, writer: anytype) !void {
            try writer.print("{{", .{});

            var dict_iter = self.iter();
            var first = true;
            while (dict_iter.next()) |item| {
                if (first) {
                    first = false;
                } else {
                    try writer.print(", ", .{});
                }
                try writer.print("{} = {}", .{ item[0], item[1] });
            }

            try writer.print("}}", .{});
        }

        pub fn get(self: *Dict, key: Value) ?Value {
            var curr: DictOrCons = .{ .dict = self };

            const key_hash = key.hash();
            inline for (0..levels) |level| {
                const obj = curr.dict.data[index(key_hash, level)] orelse return null;
                const field = comptime if (level == levels - 1) "cons" else "dict";
                curr = @unionInit(DictOrCons, field, @fieldParentPtr("obj", obj));
            }

            while (true) {
                const item = ObjType.cons.detailed(curr.cons.head.obj);
                if (item.head.eql(key)) return item.tail.?.head;
                curr = .{ .cons = curr.cons.tail orelse return null };
            }
        }

        pub fn put(self: *Dict, runner: *Runner, key: Value, value: Value) Runner.Error!*Dict {
            var dicts: [levels - 1]?*Dict = undefined;
            var cons: ?*Cons = undefined;

            const key_hash = key.hash();
            inline for (0..levels) |level| {
                const maybe_dict: ?*Dict = if (level == 0) self else dicts[level - 1];
                const maybe_obj: ?*Obj = if (maybe_dict) |dict| dict.data[index(key_hash, level)] else null;
                if (level == levels - 1) {
                    cons = if (maybe_obj) |obj| @fieldParentPtr("obj", obj) else null;
                } else {
                    dicts[level] = if (maybe_obj) |obj| @fieldParentPtr("obj", obj) else null;
                }
            }

            var curr: DictOrCons = .{ .cons = try putCons(runner, cons, key, value) };

            inline for (0..levels) |rev_level| {
                const level = comptime levels - 1 - rev_level;

                var data: [len]?*Obj = undefined;
                if (level == 0) {
                    data = self.data;
                } else if (dicts[level - 1]) |dict| {
                    data = dict.data;
                } else {
                    data = empty_data;
                }

                data[index(key_hash, level)] = if (rev_level == 0) &curr.cons.obj else &curr.dict.obj;
                curr = .{ .dict = try runner.create(.dict, .{ .data = data }) };
            }

            return curr.dict;
        }

        pub fn putCons(runner: *Runner, tail: ?*Cons, key: Value, value: Value) Runner.Error!*Cons {
            if (try replaceCons(runner, tail, key, value)) |cons| return cons;
            const item = try runner.createListValue(&.{ key, value });
            return try runner.create(.cons, .{ .head = item, .tail = tail });
        }

        pub fn replaceCons(runner: *Runner, tail: ?*Cons, key: Value, value: Value) Runner.Error!?*Cons {
            const cons = tail orelse return null;
            const item: *Cons = @fieldParentPtr("obj", cons.head.obj);
            if (item.head.eql(key)) {
                return try runner.create(.cons, .{
                    .head = try runner.createListValue(&.{ key, value }),
                    .tail = cons.tail,
                });
            } else if (try replaceCons(runner, cons.tail, key, value)) |replaced_tail| {
                return try runner.create(.cons, .{
                    .head = cons.head,
                    .tail = replaced_tail,
                });
            } else {
                return null;
            }
        }

        pub fn pop(self: *Dict, runner: *Runner, key: Value) Runner.Error!DictPop {
            var dicts: [levels - 1]*Dict = undefined;
            var cons: *Cons = undefined;

            const key_hash = key.hash();
            inline for (0..levels) |level| {
                const dict = if (level == 0) self else dicts[level - 1];
                const obj = dict.data[index(key_hash, level)] orelse return error.RunError;
                if (level == levels - 1) {
                    cons = @fieldParentPtr("obj", obj);
                } else {
                    dicts[level] = @fieldParentPtr("obj", obj);
                }
            }

            var value: Value = undefined;
            var curr: ?DictOrCons = undefined;
            {
                const cons_pop = try popCons(runner, cons, key);
                value = cons_pop.value;
                curr = if (cons_pop.cons) |cons_| .{ .cons = cons_ } else null;
            }

            inline for (0..levels) |rev_level| {
                const level = comptime levels - 1 - rev_level;

                var data = if (level == 0) self.data else dicts[level - 1].data;

                if (curr) |curr_| {
                    data[index(key_hash, level)] = if (rev_level == 0) &curr_.cons.obj else &curr_.dict.obj;
                } else {
                    data[index(key_hash, level)] = null;
                }

                for (data) |obj| {
                    if (obj != null) {
                        curr = .{ .dict = try runner.create(.dict, .{ .data = data }) };
                        break;
                    }
                } else {
                    curr = null;
                }
            }

            return .{
                .value = value,
                .dict = if (curr) |curr_| curr_.dict else try runner.create(.dict, .{ .data = empty_data }),
            };
        }

        fn popCons(runner: *Runner, cons: *Cons, key: Value) Runner.Error!ConsPop {
            const item: *Cons = @fieldParentPtr("obj", cons.head.obj);
            if (item.head.eql(key)) {
                return .{
                    .value = item.tail.?.head,
                    .cons = cons.tail,
                };
            } else {
                const cons_pop = try popCons(runner, cons.tail orelse return error.RunError, key);
                return .{
                    .value = cons_pop.value,
                    .cons = try runner.create(.cons, .{ .head = cons.head, .tail = cons_pop.cons }),
                };
            }
        }

        inline fn index(key_hash: u64, level: usize) usize {
            return (key_hash >> @truncate(bits * level)) & mask;
        }

        const DictOrCons = union {
            dict: *Dict,
            cons: *Cons,
        };

        const DictPop = struct {
            value: Value,
            dict: *Dict,
        };

        const ConsPop = struct {
            value: Value,
            cons: ?*Cons,
        };

        pub fn next(self: *const Dict, maybe_key: ?Value) ?*Cons {
            const key = maybe_key orelse return self.firstItem(0);

            var dicts: [levels - 1]*Dict = undefined;
            var cons: *Cons = undefined;

            const key_hash = key.hash();
            inline for (0..levels) |level| {
                const dict = if (level == 0) self else dicts[level - 1];
                const obj = dict.data[index(key_hash, level)] orelse return null;
                if (level == levels - 1) {
                    cons = @fieldParentPtr("obj", obj);
                } else {
                    dicts[level] = @fieldParentPtr("obj", obj);
                }
            }

            while (true) {
                const item: *Cons = @fieldParentPtr("obj", cons.head.obj);
                if (item.head.eql(key)) {
                    cons = cons.tail orelse break;
                    return @fieldParentPtr("obj", cons.head.obj);
                } else {
                    cons = cons.tail orelse return null;
                }
            }

            var level: usize = levels;
            while (level > 0) {
                level -= 1;
                const dict = if (level == 0) self else dicts[level - 1];
                const obj = dict.firstObj(index(key_hash, level) + 1) orelse continue;
                if (level == levels - 1) {
                    const cons_: *Cons = @fieldParentPtr("obj", obj);
                    return @fieldParentPtr("obj", cons_.head.obj);
                } else {
                    const dict_: *Dict = @fieldParentPtr("obj", obj);
                    return dict_.firstItem(level).?;
                }
            }

            return null;
        }

        fn firstItem(self: *const Dict, init_level: usize) ?*Cons {
            var obj = self.firstObj(0) orelse return null;
            for (init_level + 1..levels) |_| {
                const dict: *Dict = @fieldParentPtr("obj", obj);
                obj = dict.firstObj(0).?;
            }
            const cons: *Cons = @fieldParentPtr("obj", obj);
            return @fieldParentPtr("obj", cons.head.obj);
        }

        fn firstObj(self: *const Dict, init_index: usize) ?*Obj {
            for (self.data[init_index..]) |maybe_obj| {
                if (maybe_obj) |obj| return obj;
            }
            return null;
        }

        pub fn iter(self: *const Dict) Iter {
            return .{ .dict = self };
        }

        const Iter = struct {
            dict: *const Dict,
            key: ?Value = null,

            fn next(self: *Iter) ?[2]Value {
                const item = self.dict.next(self.key) orelse return null;
                const key = item.head;
                const value = item.tail.?.head;
                self.key = key;
                return .{ key, value };
            }
        };

        pub fn hash(self: *const Dict, hasher: *Hasher) void {
            for (self.data) |maybe_obj| {
                if (maybe_obj) |obj| {
                    hasher.add(1);
                    hasher.add(obj.hash);
                } else {
                    hasher.add(0);
                }
            }
        }
    };

    pub const Lambda = struct {
        obj: Obj = undefined,
        program: *Program,
        offset: usize,
        stack: []const Value,

        pub inline fn eql(_: *Lambda, _: *Lambda) bool {
            return false;
        }

        pub inline fn truthy(_: *Lambda) bool {
            return true;
        }

        pub fn allocated(self: *Lambda) usize {
            return @sizeOf(Value) * self.stack.len;
        }

        pub fn mark(self: *Lambda) void {
            self.program.obj.mark();
            for (self.stack) |value| value.mark();
        }

        pub fn deinit(self: *Lambda, allocator: std.mem.Allocator) void {
            allocator.free(self.stack);
        }

        pub fn write(_: *Lambda, writer: anytype) !void {
            try writer.print("<lambda>", .{});
        }

        pub fn hash(self: *const Lambda, hasher: *Hasher) void {
            hasher.add(self.program.obj.hash);
            hasher.add(self.offset);
            for (self.stack) |value| hasher.add(value.hash());
        }
    };

    pub const Str = struct {
        obj: Obj = undefined,
        content: []const u8,
        source: Source,

        pub inline fn eql(self: *Str, other: *Str) bool {
            return std.mem.eql(u8, self.content, other.content);
        }

        pub inline fn truthy(self: *Str) bool {
            return self.content.len > 0;
        }

        pub fn allocated(self: *Str) usize {
            return switch (self.source) {
                .alloc => @sizeOf(u8) * self.content.len,
                else => 0,
            };
        }

        pub fn mark(self: *Str) void {
            switch (self.source) {
                inline .slice, .data => |value| value.obj.mark(),
                else => {},
            }
        }

        pub fn deinit(self: *Str, allocator: std.mem.Allocator) void {
            if (self.source == .alloc) allocator.free(self.content);
        }

        pub fn write(self: *Str, writer: anytype) !void {
            try writeStr(self.content, writer);
        }

        pub fn hash(self: *const Str, hasher: *Hasher) void {
            for (self.content) |char| hasher.add(char);
        }

        pub const Source = union(enum) {
            data: *Program,
            alloc,
            slice: *Str,
        };
    };

    pub const Hasher = struct {
        const prime = 1099511628211;
        const offset = 14695981039346656037;

        hash: u64 = prime,

        pub fn add(self: *Hasher, value: u64) void {
            self.hash ^= value;
            self.hash *|= offset;
        }
    };
};
