const std = @import("std");

pub const Pattern = union(enum) {
    name: []const u8,
    ignore,

    pub fn deinit(self: Pattern, allocator: std.mem.Allocator) void {
        _ = allocator;
        switch (self) {
            .name, .ignore => {},
        }
    }
};
