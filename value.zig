const std = @import("std");

const Instr = @import("instr.zig").Instr;

pub const Value = union(enum) {
    num: f64,
    obj: *Obj,

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

        pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
            allocator.free(self.instrs);
        }
    };
};
