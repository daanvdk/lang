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

    const Error = std.mem.Allocator.Error || error{CompileError};

    fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .instrs = std.ArrayList(Instr).init(allocator),
        };
    }

    fn deinit(self: *Compiler) void {
        self.instrs.deinit();
    }

    fn compileExpr(self: *Compiler, expr: Expr, usage: Usage) Error!Info {
        switch (expr) {
            inline .num, .bool, .null => |value, tag| {
                try self.instrs.append(@unionInit(Instr, @tagName(tag), value));
                try self.compileUsage(usage);
                return .any;
            },
        }
    }

    fn compileUsage(self: *Compiler, usage: Usage) Error!void {
        switch (usage) {
            .returned => try self.instrs.append(.ret),
        }
    }
};

const Usage = enum { returned };
const Info = enum { any };

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
