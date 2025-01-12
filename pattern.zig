const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Instr = @import("instr.zig").Instr;

pub const Pattern = struct {
    data: Data,
    location: Instr.Location,

    pub const Data = union(enum) {
        name: []const u8,
        ignore,
        expr: *const Expr,
        guard: *const Guard,

        list: []const Pattern,
        lists: []const Pattern,
        dict: []const Pattern.Pair,
        dicts: []const Pattern,
        str: *const Str,
    };

    pub fn deinit(self: Pattern, allocator: std.mem.Allocator) void {
        switch (self.data) {
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
        var copy = self;
        copy.data = switch (self.data) {
            .name, .ignore => self.data,
            inline .list, .lists, .dict, .dicts => |items, tag| data: {
                const copies = try allocator.alloc(@TypeOf(items[0]), items.len);
                var i: usize = 0;
                errdefer {
                    allocator.free(copies);
                    for (copies[0..i]) |item| item.deinit(allocator);
                }
                while (i < items.len) : (i += 1) {
                    copies[i] = try items[i].clone(allocator);
                }
                break :data @unionInit(Pattern.Data, @tagName(tag), copies);
            },
            inline else => |value, tag| data: {
                const copy_ = try allocator.create(@TypeOf(value.*));
                errdefer allocator.destroy(copy_);
                copy_.* = try value.clone(allocator);
                break :data @unionInit(Pattern.Data, @tagName(tag), copy_);
            },
        };
        return copy;
    }

    pub fn usesName(self: Pattern, name: []const u8) ?bool {
        return switch (self.data) {
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

    pub const Str = struct {
        prefix: []const u8,
        splits: []const StrSplit,
        last_pattern: Pattern,
        suffix: []const u8,

        pub fn deinit(self: Str, allocator: std.mem.Allocator) void {
            allocator.free(self.prefix);
            for (self.splits) |split| split.deinit(allocator);
            allocator.free(self.splits);
            self.last_pattern.deinit(allocator);
            allocator.free(self.suffix);
        }

        pub fn clone(self: Str, allocator: std.mem.Allocator) std.mem.Allocator.Error!Str {
            var copy: Str = undefined;

            copy.prefix = try allocator.dupe(u8, self.prefix);
            errdefer allocator.free(copy.prefix);

            const splits = try allocator.alloc(StrSplit, self.splits.len);
            var i: usize = 0;
            errdefer {
                allocator.free(splits);
                for (splits[0..i]) |item| item.deinit(allocator);
            }
            while (i < splits.len) : (i += 1) {
                splits[i] = try self.splits[i].clone(allocator);
            }
            copy.splits = splits;

            copy.last_pattern = try self.last_pattern.clone(allocator);
            errdefer copy.last_pattern.deinit(allocator);

            copy.suffix = try allocator.dupe(u8, self.suffix);

            return copy;
        }

        pub fn usesName(self: Str, name: []const u8) ?bool {
            for (self.splits) |split| if (split.usesName(name)) |uses| return uses;
            return self.last_pattern.usesName(name);
        }
    };

    pub const StrSplit = struct {
        pattern: Pattern,
        separator: []const u8,

        pub fn deinit(self: StrSplit, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            allocator.free(self.separator);
        }

        pub fn clone(self: StrSplit, allocator: std.mem.Allocator) std.mem.Allocator.Error!StrSplit {
            var copy: StrSplit = undefined;
            copy.pattern = try self.pattern.clone(allocator);
            errdefer copy.pattern.deinit(allocator);
            copy.separator = try allocator.dupe(u8, self.separator);
            return copy;
        }

        pub fn usesName(self: StrSplit, name: []const u8) ?bool {
            return self.pattern.usesName(name);
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
