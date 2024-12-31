const std = @import("std");

pub const Expr = union(enum) {
    num: f64,

    pub fn deinit(self: Expr, allocator: std.mem.Allocator) void {
        _ = allocator;
        switch (self) {
            .num => {},
        }
    }
};
