const std = @import("std");

const Pattern = @import("pattern.zig").Pattern;

pub const Expr = union(enum) {
    name: []const u8,
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

    match: *const Match,

    pub fn deinit(self: Expr, allocator: std.mem.Allocator) void {
        switch (self) {
            .name, .num, .bool, .null => {},
            inline .pow, .pos, .neg, .mul, .div, .add, .sub, .eq, .ne, .lt, .le, .gt, .ge, .not, .@"and", .@"or", .match => |value| {
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

    pub const Match = struct {
        subject: Expr,
        matchers: []const Matcher,

        pub fn deinit(self: Match, allocator: std.mem.Allocator) void {
            self.subject.deinit(allocator);
            for (self.matchers) |matcher| matcher.deinit(allocator);
            allocator.free(self.matchers);
        }
    };

    pub const Matcher = struct {
        pattern: Pattern,
        expr: Expr,

        pub fn deinit(self: Matcher, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            self.expr.deinit(allocator);
        }
    };
};
