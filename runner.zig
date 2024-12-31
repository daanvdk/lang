const std = @import("std");

const Program = @import("program.zig").Program;
const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;

pub fn run(allocator: std.mem.Allocator, program: Program) Runner.Error!void {
    var runner = Runner.init(allocator);
    defer runner.deinit();
    _ = try runner.runProgram(program);
}

const Runner = struct {
    allocator: std.mem.Allocator,
    calls: std.ArrayListUnmanaged(*Call) = .{},
    last: ?*Value.Obj = null,

    const Error = std.mem.Allocator.Error || error{RunError};

    fn init(allocator: std.mem.Allocator) Runner {
        return .{ .allocator = allocator };
    }

    fn deinit(self: *Runner) void {
        self.calls.deinit(self.allocator);

        while (self.last) |obj| {
            self.last = obj.prev;
            switch (obj.type) {
                inline else => |obj_type| {
                    const detail = obj_type.detailed(obj);
                    detail.deinit(self.allocator);
                    self.allocator.destroy(detail);
                },
            }
        }
    }

    fn create(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) !*obj_type.Detail() {
        const detail = try self.allocator.create(obj_type.Detail());
        detail.* = data;
        detail.obj = .{
            .type = obj_type,
            .prev = self.last,
        };
        self.last = &detail.obj;
        return detail;
    }

    fn runProgram(self: *Runner, program: Program) Error!Value {
        const program_ = self.create(.program, .{ .instrs = program.instrs }) catch |err| {
            program.deinit(self.allocator);
            return err;
        };
        var call = Call{ .program = program_, .offset = 0 };
        defer call.stack.deinit(self.allocator);
        return try self.run(&call);
    }

    fn run(self: *Runner, call: *Call) Error!Value {
        try self.calls.append(self.allocator, call);
        defer _ = self.calls.pop();

        var ip = call.program.instrs.ptr + call.offset;
        while (true) {
            const instr = ip[0];
            ip += 1;
            switch (instr) {
                .local => |local| {
                    const value = call.stack.items[local];
                    try call.stack.append(self.allocator, value);
                },
                .pop => |local| {
                    _ = call.stack.orderedRemove(local);
                },
                inline .num, .bool, .null => |value, tag| {
                    try call.stack.append(self.allocator, @unionInit(Value, @tagName(tag), value));
                },

                .pow => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = std.math.pow(f64, lhs, rhs) });
                },
                .pos => {
                    const value = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = value });
                },
                .neg => {
                    const value = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = -value });
                },
                .mul => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs * rhs });
                },
                .div => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs / rhs });
                },
                .add => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs + rhs });
                },
                .sub => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs - rhs });
                },

                .eq => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, .{ .bool = lhs.eql(rhs) });
                },
                .ne => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, .{ .bool = !lhs.eql(rhs) });
                },
                .lt => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs < rhs });
                },
                .le => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs <= rhs });
                },
                .gt => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs > rhs });
                },
                .ge => {
                    const rhs = try expectNum(call.stack.pop());
                    const lhs = try expectNum(call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs >= rhs });
                },

                .not => {
                    const value = call.stack.pop();
                    try call.stack.append(self.allocator, .{ .bool = !value.truthy() });
                },
                .jmp => |jmp| {
                    ip += jmp;
                },
                .jmp_if => |jmp| {
                    if (call.stack.pop().truthy()) ip += jmp;
                },

                .ret => {
                    return call.stack.pop();
                },
            }
        }
    }

    fn expectNum(value: Value) !f64 {
        return switch (value) {
            .num => |num| num,
            else => error.RunError,
        };
    }

    const Call = struct {
        program: *Value.Program,
        offset: usize,
        stack: std.ArrayListUnmanaged(Value) = .{},
    };
};

fn cloneProgram(program: Program) !Program {
    return .{
        .instrs = try std.testing.allocator.dupe(Instr, program.instrs),
    };
}

test "run num 1" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .num = 1337 },
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 1337 }, value);
}

test "run num 2" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .num = 45.67 },
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 45.67 }, value);
}

test "run bool 1" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .bool = true },
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .bool = true }, value);
}

test "run bool 2" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .bool = false },
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .bool = false }, value);
}

test "run null" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .null,
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value.null, value);
}

test "run operators" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .num = 1 },
            .{ .num = 2 },
            .mul,
            .{ .num = 3 },
            .{ .num = 4 },
            .neg,
            .div,
            .add,
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 1.25 }, value);
}
