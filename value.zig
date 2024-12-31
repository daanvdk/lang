const std = @import("std");

const Instr = @import("instr.zig").Instr;
const Runner = @import("runner.zig").Runner;

pub const Value = union(enum) {
    num: f64,
    bool: bool,
    null,
    nil,
    builtin: *const fn (*Runner, ?*Cons) Runner.Error!Value,
    obj: *Obj,

    pub fn eql(self: Value, other: Value) bool {
        return switch (self) {
            .num => |value| other == .num and value == other.num,
            .bool => |value| other == .bool and value == other.bool,
            .null => other == .null,
            .nil => other == .nil,
            .builtin => |value| other == .builtin and value == other.builtin,
            .obj => |obj| other == .obj and (obj == other.obj or
                (obj.type == other.obj.type and switch (obj.type) {
                inline else => |obj_type| obj_type.detailed(obj).eql(obj_type.detailed(other.obj)),
            })),
        };
    }

    pub fn truthy(self: Value) bool {
        return switch (self) {
            .num => |value| value != 0,
            .bool => |value| value,
            .null => false,
            .nil => false,
            .builtin => false,
            .obj => |obj| switch (obj.type) {
                inline else => |obj_type| obj_type.detailed(obj).truthy(),
            },
        };
    }

    pub const Obj = struct {
        type: ObjType,
        prev: ?*Obj,

        pub fn toValue(self: *Obj) Value {
            return .{ .obj = self };
        }
    };

    pub const ObjType = enum {
        program,
        cons,
        lambda,

        pub fn Detail(comptime self: ObjType) type {
            return switch (self) {
                .program => Program,
                .cons => Cons,
                .lambda => Lambda,
            };
        }

        pub fn detailed(comptime self: ObjType, obj: *Obj) *self.Detail() {
            return @fieldParentPtr("obj", obj);
        }
    };

    pub const Program = struct {
        obj: Obj = undefined,
        instrs: []const Instr,

        pub inline fn eql(_: *Program, _: *Program) bool {
            return false;
        }

        pub inline fn truthy(_: *Program) bool {
            return true;
        }

        pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
            allocator.free(self.instrs);
        }
    };

    pub const Cons = struct {
        obj: Obj = undefined,
        head: Value,
        tail: ?*Cons,

        pub inline fn eql(self: *Cons, other: *Cons) bool {
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

        pub fn deinit(_: Cons, _: std.mem.Allocator) void {}

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

        pub fn deinit(self: Lambda, allocator: std.mem.Allocator) void {
            allocator.free(self.stack);
        }
    };
};
