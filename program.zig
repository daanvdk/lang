const std = @import("std");

const Instr = @import("instr.zig").Instr;

pub const Program = struct {
    instrs: []const Instr,
    data: []const u8,

    pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
        allocator.free(self.instrs);
        allocator.free(self.data);
    }
};
