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
            .num => |value| {
                try self.instrs.append(.{ .num = value });
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

test "compile num" {
    const program = try compile(std.testing.allocator, .{ .num = 1337 });
    defer program.deinit(std.testing.allocator);

    try std.testing.expectEqualSlices(Instr, &.{
        .{ .num = 1337 },
        .ret,
    }, program.instrs);
}
