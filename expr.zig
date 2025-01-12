const std = @import("std");

const Pattern = @import("pattern.zig").Pattern;
const Instr = @import("instr.zig").Instr;

pub const Expr = struct {
    data: Data,
    location: Instr.Location,

    pub const Data = union(enum) {
        global: Instr.Global,
        name: []const u8,
        num: f64,
        bool: bool,
        null,
        str: []const u8,

        list: []const Expr,
        lists: []const Expr,
        dict: []const Pair,
        dicts: []const Expr,
        lambda: *const Matcher,

        call: *const Bin,
        get: *const Bin,
        slice: *const Tri,

        pow: *const Bin,
        pos: *const Expr,
        neg: *const Expr,
        mul: *const Bin,
        div: *const Bin,
        add: *const Bin,
        sub: *const Bin,

        cmp: *const CmpRoot,

        not: *const Expr,
        @"and": *const Bin,
        @"or": *const Bin,

        match: *const Match,
        @"if": *const Tri,
        @"for": *const For,
        gen: *const Expr,
        yield: *const Expr,
        yield_all: *const Expr,
        @"return": *const Expr,
        assert: *const Expr,

        module: []const Stmt,
    };

    pub fn deinit(self: Expr, allocator: std.mem.Allocator) void {
        switch (self.data) {
            .global, .name, .num, .bool, .null => {},
            .str => |content| allocator.free(content),
            inline .list, .lists, .dict, .dicts, .module => |items| {
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
        var copy = self;
        copy.data = switch (self.data) {
            .global, .name, .num, .bool, .null => self.data,
            .str => |content| .{ .str = try allocator.dupe(u8, content) },
            inline .list, .lists, .dict, .dicts, .module => |items, tag| data: {
                const copies = try allocator.alloc(@TypeOf(items[0]), items.len);
                var i: usize = 0;
                errdefer {
                    allocator.free(copies);
                    for (copies[0..i]) |item| item.deinit(allocator);
                }
                while (i < items.len) : (i += 1) {
                    copies[i] = try items[i].clone(allocator);
                }
                break :data @unionInit(Data, @tagName(tag), copies);
            },
            inline else => |value, tag| data: {
                const copy_ = try allocator.create(@TypeOf(value.*));
                errdefer allocator.destroy(copy_);
                copy_.* = try value.clone(allocator);
                break :data @unionInit(Data, @tagName(tag), copy_);
            },
        };
        return copy;
    }

    pub fn usesName(self: Expr, name: []const u8) bool {
        return switch (self.data) {
            .global, .num, .bool, .null, .str => false,
            .name => |name_| std.mem.eql(u8, name_, name),
            inline .list, .lists, .dict, .dicts => |items| for (items) |item| {
                if (item.usesName(name)) break true;
            } else false,
            .module => |stmts| uses: {
                const is_in_module = for (stmts) |stmt| {
                    if (stmt.fn_name) |fn_name| {
                        if (std.mem.eql(u8, name, fn_name)) break true;
                    } else {
                        if (!(stmt.pattern.usesName(name) orelse true)) break true;
                    }
                } else false;

                if (!is_in_module) {
                    for (stmts) |stmt| {
                        if (stmt.fn_name != null and (stmt.pattern.usesName(name) orelse stmt.subject.usesName(name))) break :uses true;
                    }
                }

                for (stmts) |stmt| {
                    if (stmt.fn_name) |fn_name| {
                        if (std.mem.eql(u8, name, fn_name)) break :uses false;
                    } else {
                        if (stmt.subject.usesName(name)) break :uses true;
                        if (stmt.pattern.usesName(name)) |uses| break :uses uses;
                    }
                }

                break :uses false;
            },
            inline else => |value| value.usesName(name),
        };
    }

    pub const Bin = struct {
        lhs: Expr,
        rhs: Expr,

        pub fn deinit(self: Bin, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            self.rhs.deinit(allocator);
        }

        pub fn clone(self: Bin, allocator: std.mem.Allocator) std.mem.Allocator.Error!Bin {
            var copy: Bin = self;
            copy.lhs = try self.lhs.clone(allocator);
            errdefer copy.lhs.deinit(allocator);
            copy.rhs = try self.rhs.clone(allocator);
            return copy;
        }

        pub fn usesName(self: Bin, name: []const u8) bool {
            return self.lhs.usesName(name) or self.rhs.usesName(name);
        }
    };

    pub const CmpRoot = struct {
        lhs: Expr,
        cmps: []const Cmp,
        last_op: Cmp.Op,
        last_rhs: Expr,

        pub fn deinit(self: CmpRoot, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            for (self.cmps) |op| op.deinit(allocator);
            allocator.free(self.cmps);
            self.last_rhs.deinit(allocator);
        }

        pub fn clone(self: CmpRoot, allocator: std.mem.Allocator) std.mem.Allocator.Error!CmpRoot {
            var copy = self;

            copy.lhs = try self.lhs.clone(allocator);
            errdefer copy.lhs.deinit(allocator);

            const cmps = try allocator.alloc(Cmp, self.cmps.len);
            var i: usize = 0;
            errdefer {
                allocator.free(cmps);
                for (cmps[0..i]) |cmp| cmp.deinit(allocator);
            }
            while (i < cmps.len) : (i += 1) {
                cmps[i] = try self.cmps[i].clone(allocator);
            }
            copy.cmps = cmps;

            copy.last_rhs = try self.last_rhs.clone(allocator);

            return copy;
        }

        pub fn usesName(self: CmpRoot, name: []const u8) bool {
            if (self.lhs.usesName(name)) return true;
            for (self.cmps) |op| if (op.usesName(name)) return true;
            return false;
        }
    };

    pub const Cmp = struct {
        op: Op,
        rhs: Expr,

        pub fn deinit(self: Cmp, allocator: std.mem.Allocator) void {
            self.rhs.deinit(allocator);
        }

        pub fn clone(self: Cmp, allocator: std.mem.Allocator) std.mem.Allocator.Error!Cmp {
            var copy = self;
            copy.rhs = try self.rhs.clone(allocator);
            errdefer copy.rhs.deinit(allocator);
            return copy;
        }

        pub fn usesName(self: Cmp, name: []const u8) bool {
            return self.rhs.usesName(name);
        }

        pub const Op = enum { eq, ne, lt, le, gt, ge, in };
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

        pub fn usesName(self: Match, name: []const u8) bool {
            if (self.subject.usesName(name)) return true;
            for (self.matchers) |matcher| {
                if (matcher.usesName(name)) return true;
                if (matcher.pattern.data == .ignore) break;
            }
            return false;
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
            var copy: Matcher = self;
            copy.pattern = try self.pattern.clone(allocator);
            errdefer copy.pattern.deinit(allocator);
            copy.expr = try self.expr.clone(allocator);
            return copy;
        }

        pub fn usesName(self: Matcher, name: []const u8) bool {
            return self.pattern.usesName(name) orelse self.expr.usesName(name);
        }
    };

    pub const Tri = struct {
        lhs: Expr,
        mhs: Expr,
        rhs: Expr,

        pub fn deinit(self: Tri, allocator: std.mem.Allocator) void {
            self.lhs.deinit(allocator);
            self.mhs.deinit(allocator);
            self.rhs.deinit(allocator);
        }

        pub fn clone(self: Tri, allocator: std.mem.Allocator) std.mem.Allocator.Error!Tri {
            var copy: Tri = undefined;
            copy.lhs = try self.lhs.clone(allocator);
            errdefer copy.lhs.deinit(allocator);
            copy.mhs = try self.mhs.clone(allocator);
            errdefer copy.mhs.deinit(allocator);
            copy.rhs = try self.rhs.clone(allocator);
            return copy;
        }

        pub fn usesName(self: Tri, name: []const u8) bool {
            return self.lhs.usesName(name) or self.mhs.usesName(name) or self.rhs.usesName(name);
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

        pub fn usesName(self: For, name: []const u8) bool {
            return self.subject.usesName(name) or self.matcher.usesName(name);
        }
    };

    pub const Pair = struct {
        key: Expr,
        value: Expr,

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

        pub fn usesName(self: Pair, name: []const u8) bool {
            return self.key.usesName(name) or self.value.usesName(name);
        }
    };

    pub const Stmt = struct {
        fn_name: ?[]const u8,
        pattern: Pattern,
        subject: Expr,
        is_pub: bool,

        pub fn deinit(self: Stmt, allocator: std.mem.Allocator) void {
            self.pattern.deinit(allocator);
            self.subject.deinit(allocator);
        }

        pub fn clone(self: Stmt, allocator: std.mem.Allocator) std.mem.Allocator.Error!Stmt {
            var copy: Stmt = self;
            copy.pattern = try self.pattern.clone(allocator);
            copy.subject = try self.subject.clone(allocator);
            return copy;
        }
    };
};
