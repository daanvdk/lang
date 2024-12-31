const std = @import("std");

const Instr = @import("instr.zig").Instr;

pub const Program = struct {
    instrs: []const Instr,

    pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
        allocator.free(self.instrs);
    }
};
