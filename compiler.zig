const std = @import("std");

const Expr = @import("expr.zig").Expr;
const Instr = @import("instr.zig").Instr;
const Program = @import("program.zig").Program;

pub fn compile(allocator: std.mem.Allocator, expr: Expr) Compiler.Error!Program {
    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    _ = try compiler.compileExpr(expr, .returned);

    const instrs = try compiler.instrs.toOwnedSlice();
    return .{ .instrs = instrs };
}

const Compiler = struct {
    instrs: std.ArrayList(Instr),
    stack: Stack,

    const Error = std.mem.Allocator.Error || error{CompileError};

    fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .instrs = std.ArrayList(Instr).init(allocator),
            .stack = Stack.init(allocator),
        };
    }

    fn deinit(self: *Compiler) void {
        self.instrs.deinit();
        self.stack.deinit();
    }

    fn compileExpr(self: *Compiler, expr: Expr, usage: Usage) Error!Info {
        switch (expr) {
            inline .num, .bool, .null => |value, tag| {
                try self.instrs.append(@unionInit(Instr, @tagName(tag), value));
                try self.compileUsage(usage);
                return .any;
            },
            inline .pow, .mul, .div, .add, .sub, .eq, .ne, .lt, .le, .gt, .ge => |bin, tag| {
                _ = try self.compileExpr(bin.lhs, .used);
                try self.stack.push(null);
                _ = try self.compileExpr(bin.rhs, .used);
                self.stack.pop();
                try self.instrs.append(@field(Instr, @tagName(tag)));
                try self.compileUsage(usage);
                return .any;
            },
            inline .pos, .neg, .not => |expr_ptr, tag| {
                _ = try self.compileExpr(expr_ptr.*, .used);
                try self.instrs.append(@field(Instr, @tagName(tag)));
                try self.compileUsage(usage);
                return .any;
            },
            .@"and" => |bin| {
                var info = try self.compileExpr(bin.lhs, .used);
                switch (usage) {
                    .returned => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.null);

                        const ret_start = self.instrs.items.len;
                        try self.compileUsage(.returned);
                        self.instrs.items[ret_start - 1] = .{ .jmp_if = self.instrs.items.len - ret_start };

                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .returned));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .used));

                        self.instrs.items[rhs_start - 1] = .{ .jmp = self.instrs.items.len - rhs_start };
                    },
                }
                return info;
            },
            .@"or" => |bin| {
                var info = try self.compileExpr(bin.lhs, .used);
                switch (usage) {
                    .returned => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.{ .jmp_if = 1 });
                        try self.instrs.append(.null);

                        const ret_start = self.instrs.items.len;
                        try self.compileUsage(.returned);
                        self.instrs.items[ret_start - 1] = .{ .jmp = self.instrs.items.len - ret_start };

                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .returned));
                    },
                    .used => {
                        try self.instrs.append(.{ .local = self.stack.size() });
                        try self.instrs.append(.null);

                        const rhs_start = self.instrs.items.len;
                        try self.instrs.append(.{ .pop = self.stack.size() });
                        info = info.merge(try self.compileExpr(bin.rhs, .used));

                        self.instrs.items[rhs_start - 1] = .{ .jmp_if = self.instrs.items.len - rhs_start };
                    },
                }
                return info;
            },
        }
    }

    fn compileUsage(self: *Compiler, usage: Usage) Error!void {
        switch (usage) {
            .returned => try self.instrs.append(.ret),
            .used => {},
        }
    }
};

const Usage = enum { returned, used };
const Info = enum {
    any,
    all,

    fn merge(self: Info, other: Info) Info {
        if (self == other or self == .all) return other;
        if (other == .all) return self;
        return .any;
    }
};

const Stack = struct {
    entries: std.ArrayList(?Entry),

    fn init(allocator: std.mem.Allocator) Stack {
        return .{
            .entries = std.ArrayList(?Entry).init(allocator),
        };
    }

    fn deinit(self: *Stack) void {
        self.entries.deinit();
    }

    inline fn size(self: *Stack) usize {
        return self.entries.items.len;
    }

    fn push(self: *Stack, entry: ?Entry) !void {
        try self.entries.append(entry);
    }

    fn pop(self: *Stack) void {
        _ = self.entries.pop();
    }

    fn get(self: *Stack, name: []const u8) ?GetResult {
        return self.getFromIndex(0, name);
    }

    fn getFromIndex(self: *Stack, min_index: usize, name: []const u8) ?GetResult {
        const index = self.size();
        while (index > min_index) {
            index -= 1;
            const entry = self.entries.items[index] orelse continue;
            if (std.mem.eql(u8, entry.name, name)) {
                return .{ .index = index, .info = entry.info };
            }
        }
    }

    const Entry = struct {
        name: []const u8,
        info: Info,
    };

    const GetResult = struct {
        index: usize,
        info: Info,
    };
};

test "compile num 1" {
    const program = try compile(std.testing.allocator, .{ .num = 1337 });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1337 },
        .ret,
    }, program.instrs);
}

test "compile num 2" {
    const program = try compile(std.testing.allocator, .{ .num = 45.67 });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 45.67 },
        .ret,
    }, program.instrs);
}

test "compile bool 1" {
    const program = try compile(std.testing.allocator, .{ .bool = true });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = true },
        .ret,
    }, program.instrs);
}

test "compile bool 2" {
    const program = try compile(std.testing.allocator, .{ .bool = false });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .bool = false },
        .ret,
    }, program.instrs);
}

test "compile null" {
    const program = try compile(std.testing.allocator, .null);
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .null,
        .ret,
    }, program.instrs);
}

test "compile operators" {
    const program = try compile(std.testing.allocator, .{ .add = &.{
        .lhs = .{ .mul = &.{
            .lhs = .{ .num = 1 },
            .rhs = .{ .num = 2 },
        } },
        .rhs = .{ .div = &.{
            .lhs = .{ .num = 3 },
            .rhs = .{ .neg = &.{ .num = 4 } },
        } },
    } });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1 },
        .{ .num = 2 },
        .mul,
        .{ .num = 3 },
        .{ .num = 4 },
        .neg,
        .div,
        .add,
        .ret,
    }, program.instrs);
}
