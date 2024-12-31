const std = @import("std");

pub const Expr = union(enum) {
    num: f64,
    bool: bool,
    null,

    pow: *const Bin,
    pos: *const Expr,
    neg: *const Expr,
    mul: *const Bin,
    div: *const Bin,
    add: *const Bin,
    sub: *const Bin,

    eq: *const Bin,
    ne: *const Bin,
    lt: *const Bin,
    le: *const Bin,
    gt: *const Bin,
    ge: *const Bin,

    not: *const Expr,
    @"and": *const Bin,
    @"or": *const Bin,

    pub fn deinit(self: Expr, allocator: std.mem.Allocator) void {
        switch (self) {
            .num, .bool, .null => {},
            inline .pow, .pos, .neg, .mul, .div, .add, .sub, .eq, .ne, .lt, .le, .gt, .ge, .not, .@"and", .@"or" => |value| {
                value.deinit(allocator);
                allocator.destroy(value);
            },
        }
    }

    pub const Bin = struct {
        lhs: Expr,
        rhs: Expr,

        pub fn deinit(self: Bin, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            self.rhs.deinit(allocator);
        }
    };
};
