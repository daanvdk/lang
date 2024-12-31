const std = @import("std");

const Expr = @import("expr.zig").Expr;

pub const Pattern = union(enum) {
    name: []const u8,
    ignore,
    expr: *const Expr,

    list: []const Pattern,
    lists: []const Pattern,

    pub fn deinit(self: Pattern, allocator: std.mem.Allocator) void {
        switch (self) {
            .name, .ignore => {},
            .expr => |expr| {
                expr.deinit(allocator);
                allocator.destroy(expr);
            },
            .list, .lists => |items| {
                for (items) |item| item.deinit(allocator);
                allocator.free(items);
            },
        }
    }

    pub fn clone(self: Pattern, allocator: std.mem.Allocator) std.mem.Allocator.Error!Pattern {
        switch (self) {
            .name, .ignore => return self,
            .expr => |expr| {
                const copy = try allocator.create(Expr);
                errdefer allocator.destroy(copy);
                copy.* = try expr.clone(allocator);
                return .{ .expr = copy };
            },
            inline .list, .lists => |items, tag| {
                const copies = try allocator.alloc(Pattern, items.len);
                var i: usize = 0;
                errdefer {
                    allocator.free(copies);
                    for (copies[0..i]) |item| item.deinit(allocator);
                }
                while (i < items.len) : (i += 1) {
                    copies[i] = try items[i].clone(allocator);
                }
                return @unionInit(Pattern, @tagName(tag), copies);
            },
        }
    }
};
