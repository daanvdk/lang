const std = @import("std");

const Instr = @import("instr.zig").Instr;

pub const Value = union(enum) {
    num: f64,
    bool: bool,
    null,
    obj: *Obj,

    pub fn eql(self: Value, other: Value) bool {
        return switch (self) {
            .num => |value| other == .num and value == other.num,
            .bool => |value| other == .bool and value == other.bool,
            .null => other == .null,
            .obj => |obj| other == .obj and obj.type == other.obj.type and switch (obj.type) {
                inline else => |obj_type| obj_type.detailed(obj).eql(obj_type.detailed(other.obj)),
            },
        };
    }

    pub fn truthy(self: Value) bool {
        return switch (self) {
            .num => |value| value != 0,
            .bool => |value| value,
            .null => false,
            .obj => |obj| switch (obj.type) {
                inline else => |obj_type| obj_type.detailed(obj).truthy(),
            },
        };
    }

    pub const Obj = struct {
        type: ObjType,
        prev: ?*Obj,
    };

    pub const ObjType = enum {
        program,

        pub fn Detail(comptime self: ObjType) type {
            return switch (self) {
                .program => Program,
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
};
