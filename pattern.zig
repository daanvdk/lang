const std = @import("std");

const Expr = @import("expr.zig").Expr;

pub const Pattern = union(enum) {
    name: []const u8,
    ignore,
    expr: *const Expr,
    guard: *const Guard,

    list: []const Pattern,
    lists: []const Pattern,
    dict: []const Pattern.Pair,
    dicts: []const Pattern,

    pub fn deinit(self: Pattern, allocator: std.mem.Allocator) void {
        switch (self) {
            .name, .ignore => {},
            inline .list, .lists, .dict, .dicts => |items| {
                for (items) |item| item.deinit(allocator);
                allocator.free(items);
            },
            inline else => |value| {
                value.deinit(allocator);
                allocator.destroy(value);
            },
        }
    }

    pub fn clone(self: Pattern, allocator: std.mem.Allocator) std.mem.Allocator.Error!Pattern {
        switch (self) {
            .name, .ignore => return self,
            inline .list, .lists, .dict, .dicts => |items, tag| {
                const copies = try allocator.alloc(@TypeOf(items[0]), items.len);
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
            inline else => |value, tag| {
                const copy = try allocator.create(@TypeOf(value.*));
                errdefer allocator.destroy(copy);
                copy.* = try value.clone(allocator);
                return @unionInit(Pattern, @tagName(tag), copy);
            },
        }
    }

    pub fn usesName(self: Pattern, name: []const u8) ?bool {
        return switch (self) {
            .name => |name_| if (std.mem.eql(u8, name_, name)) false else null,
            .ignore => null,
            inline .list, .lists, .dict, .dicts => |items| for (items) |item| {
                if (item.usesName(name)) |uses| break uses;
            } else null,
            .expr => |expr| if (expr.usesName(name)) true else null,
            inline else => |value| value.usesName(name),
        };
    }

    pub const Guard = struct {
        pattern: Pattern,
        cond: Expr,

        pub fn deinit(self: Guard, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            self.cond.deinit(allocator);
        }

        pub fn clone(self: Guard, allocator: std.mem.Allocator) std.mem.Allocator.Error!Guard {
            var copy: Guard = undefined;
            copy.pattern = try self.pattern.clone(allocator);
            errdefer copy.pattern.deinit(allocator);
            copy.cond = try self.cond.clone(allocator);
            return copy;
        }

        pub fn usesName(self: Guard, name: []const u8) ?bool {
            if (self.pattern.usesName(name)) |uses| return uses;
            if (self.cond.usesName(name)) return true;
            return null;
        }
    };

    pub const Pair = struct {
        key: Expr,
        value: Pattern,

        pub fn deinit(self: Pair, allocator: std.mem.Allocator) void {
            self.key.deinit(allocator);
            self.value.deinit(allocator);
        }

        pub fn clone(self: Pair, allocator: std.mem.Allocator) std.mem.Allocator.Error!Pair {
            var copy: Pair = undefined;
            copy.key = try self.key.clone(allocator);
            errdefer copy.key.deinit(allocator);
            copy.value = try self.value.clone(allocator);
            return copy;
        }

        pub fn usesName(self: Pair, name: []const u8) ?bool {
            if (self.key.usesName(name)) return true;
            return self.value.usesName(name);
        }
    };
};
