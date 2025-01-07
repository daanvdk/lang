const std = @import("std");

const Pattern = @import("pattern.zig").Pattern;
const Instr = @import("instr.zig").Instr;

pub const Expr = union(enum) {
    global: Instr.Global,
    name: []const u8,
    num: f64,
    bool: bool,
    null,
    str: []const u8,

    list: []const Expr,
    lists: []const Expr,
    lambda: *const Matcher,

    call: *const Bin,

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
    @"if": *const If,
    @"for": *const For,
    gen: *const Expr,
    yield: *const Expr,
    yield_all: *const Expr,
    @"return": *const Expr,

    pub fn deinit(self: Expr, allocator: std.mem.Allocator) void {
        switch (self) {
            .global, .name, .num, .bool, .null => {},
            .str => |content| allocator.free(content),
            .list, .lists => |items| {
                for (items) |item| item.deinit(allocator);
                allocator.free(items);
            },
            inline else => |value| {
                value.deinit(allocator);
                allocator.destroy(value);
            },
        }
    }

    pub fn clone(self: Expr, allocator: std.mem.Allocator) std.mem.Allocator.Error!Expr {
        switch (self) {
            .global, .name, .num, .bool, .null => return self,
            .str => |content| return .{ .str = try allocator.dupe(u8, content) },
            inline .list, .lists => |items, tag| {
                const copies = try allocator.alloc(Expr, items.len);
                var i: usize = 0;
                errdefer {
                    allocator.free(copies);
                    for (copies[0..i]) |item| item.deinit(allocator);
                }
                while (i < items.len) : (i += 1) {
                    copies[i] = try items[i].clone(allocator);
                }
                return @unionInit(Expr, @tagName(tag), copies);
            },
            inline else => |value, tag| {
                const copy = try allocator.create(@TypeOf(value.*));
                errdefer allocator.destroy(copy);
                copy.* = try value.clone(allocator);
                return @unionInit(Expr, @tagName(tag), copy);
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

        pub fn clone(self: Bin, allocator: std.mem.Allocator) std.mem.Allocator.Error!Bin {
            var copy: Bin = undefined;
            copy.lhs = try self.lhs.clone(allocator);
            errdefer copy.lhs.deinit(allocator);
            copy.rhs = try self.rhs.clone(allocator);
            return copy;
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

        pub fn clone(self: Match, allocator: std.mem.Allocator) std.mem.Allocator.Error!Match {
            const subject = try self.subject.clone(allocator);
            errdefer subject.deinit(allocator);

            const matchers = try allocator.alloc(Matcher, self.matchers.len);
            var i: usize = 0;
            errdefer {
                allocator.free(matchers);
                for (matchers[0..i]) |matcher| matcher.deinit(allocator);
            }
            while (i < matchers.len) : (i += 1) {
                matchers[i] = try self.matchers[i].clone(allocator);
            }

            return .{ .subject = subject, .matchers = matchers };
        }
    };

    pub const Matcher = struct {
        pattern: Pattern,
        expr: Expr,

        pub fn deinit(self: Matcher, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            self.expr.deinit(allocator);
        }

        pub fn clone(self: Matcher, allocator: std.mem.Allocator) std.mem.Allocator.Error!Matcher {
            var copy: Matcher = undefined;
            copy.pattern = try self.pattern.clone(allocator);
            errdefer copy.pattern.deinit(allocator);
            copy.expr = try self.expr.clone(allocator);
            return copy;
        }
    };

    pub const If = struct {
        cond: Expr,
        then: Expr,
        else_: Expr,

        pub fn deinit(self: If, allocator: std.mem.Allocator) void {
            self.cond.deinit(allocator);
            self.then.deinit(allocator);
            self.else_.deinit(allocator);
        }

        pub fn clone(self: If, allocator: std.mem.Allocator) std.mem.Allocator.Error!If {
            var copy: If = undefined;
            copy.cond = try self.cond.clone(allocator);
            errdefer copy.cond.deinit(allocator);
            copy.then = try self.then.clone(allocator);
            errdefer copy.then.deinit(allocator);
            copy.else_ = try self.else_.clone(allocator);
            return copy;
        }
    };

    pub const For = struct {
        subject: Expr,
        matcher: Matcher,

        pub fn deinit(self: For, allocator: std.mem.Allocator) void {
            self.subject.deinit(allocator);
            self.matcher.deinit(allocator);
        }

        pub fn clone(self: For, allocator: std.mem.Allocator) std.mem.Allocator.Error!For {
            var copy: For = undefined;
            copy.subject = try self.subject.clone(allocator);
            errdefer copy.subject.deinit(allocator);
            copy.matcher = try self.matcher.clone(allocator);
            return copy;
        }
    };
};
