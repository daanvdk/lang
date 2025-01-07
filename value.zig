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

    pub const Obj = struct {
        type: ObjType,
        prev: ?*Obj,
        seen: bool,

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
        lambda,
        str,

        pub fn Detail(comptime self: ObjType) type {
            return switch (self) {
                .program => Program,
                .cons => Cons,
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
        instrs: []const Instr,
        data: []const u8,

        pub inline fn eql(_: *Program, _: *Program) bool {
            return false;
        }

        pub inline fn truthy(_: *Program) bool {
            return true;
        }

        pub fn allocated(self: *Program) usize {
            return @sizeOf(Instr) * self.instrs.len + @sizeOf(u8) * self.data.len;
        }

        pub fn mark(_: *Program) void {}

        pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
            allocator.free(self.instrs);
            allocator.free(self.data);
        }

        pub fn write(_: *Program, writer: anytype) !void {
            try writer.print("<program>", .{});
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

        pub const Source = union(enum) {
            data: *Program,
            alloc,
            slice: *Str,
        };
    };
};
