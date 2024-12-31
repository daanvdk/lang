const std = @import("std");

const Program = @import("program.zig").Program;
const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;

pub fn run(allocator: std.mem.Allocator, program: Program) Runner.Error!void {
    var runner = Runner.init(allocator);
    defer runner.deinit();
    _ = try runner.runProgram(program);
}

pub const Runner = struct {
    allocator: std.mem.Allocator,
    calls: std.ArrayListUnmanaged(*Call) = .{},
    last: ?*Value.Obj = null,

    pub const Error = std.mem.Allocator.Error || error{RunError};

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

    fn createValue(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) !Value {
        const detail = try self.create(obj_type, data);
        return detail.obj.toValue();
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
                .global => |global| {
                    const value = switch (global) {
                        inline else => |tag| Value{ .builtin = @field(Builtins, @tagName(tag)) },
                    };
                    try call.stack.append(self.allocator, value);
                },
                .local => |local| {
                    const value = call.stack.items[local];
                    try call.stack.append(self.allocator, value);
                },
                .pop => |local| {
                    _ = call.stack.orderedRemove(local);
                },
                inline .num, .bool, .null, .nil => |value, tag| {
                    try call.stack.append(self.allocator, @unionInit(Value, @tagName(tag), value));
                },

                .cons => {
                    const tail = try expectList(call.stack.pop());
                    const head = call.stack.pop();
                    const cons = try self.createValue(.cons, .{ .head = head, .tail = tail });
                    try call.stack.append(self.allocator, cons);
                },
                .decons => {
                    const cons = try expectCons(call.stack.pop());
                    try call.stack.append(self.allocator, cons.head);
                    try call.stack.append(self.allocator, Value.Cons.toValue(cons.tail));
                },
                .lambda => |lambda| {
                    const stack_ = try self.allocator.alloc(Value, lambda.caps);
                    const index = call.stack.items.len - lambda.caps;
                    @memcpy(stack_, call.stack.items[index..]);
                    call.stack.shrinkRetainingCapacity(index);

                    const value = self.createValue(.lambda, .{
                        .program = call.program,
                        .offset = (@intFromPtr(ip) - @intFromPtr(call.program.instrs.ptr)) / @sizeOf(Instr),
                        .stack = stack_,
                    }) catch |err| {
                        self.allocator.free(stack_);
                        return err;
                    };
                    try call.stack.append(self.allocator, value);
                    ip += lambda.len;
                },

                .call => {
                    const args = try expectList(call.stack.pop());
                    const value = switch (try expectCallable(call.stack.pop())) {
                        .builtin => |builtin| try builtin(self, args),
                        .lambda => |lambda| value: {
                            var call_ = Call{
                                .program = lambda.program,
                                .offset = lambda.offset,
                            };
                            defer call_.stack.deinit(self.allocator);

                            try call_.stack.ensureUnusedCapacity(self.allocator, lambda.stack.len + 1);
                            call_.stack.appendSliceAssumeCapacity(lambda.stack);
                            call_.stack.appendAssumeCapacity(Value.Cons.toValue(args));

                            break :value try self.run(&call_);
                        },
                    };
                    try call.stack.append(self.allocator, value);
                },
                .tail_call => {
                    const args = try expectList(call.stack.pop());
                    switch (try expectCallable(call.stack.pop())) {
                        .builtin => |builtin| {
                            call.stack.clearAndFree(self.allocator);
                            return try builtin(self, args);
                        },
                        .lambda => |lambda| {
                            call.program = lambda.program;
                            call.offset = lambda.offset;

                            call.stack.clearRetainingCapacity();
                            try call.stack.ensureUnusedCapacity(self.allocator, lambda.stack.len + 1);
                            call.stack.appendSliceAssumeCapacity(lambda.stack);
                            call.stack.appendAssumeCapacity(Value.Cons.toValue(args));

                            ip = call.program.instrs.ptr + call.offset;
                        },
                    }
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
                .no_match => {
                    return error.RunError;
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

    fn expectCons(value: Value) !*Value.Cons {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    fn expectList(value: Value) !?*Value.Cons {
        return switch (value) {
            .nil => null,
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    fn expectCallable(value: Value) !Callable {
        return switch (value) {
            .builtin => |builtin| .{ .builtin = builtin },
            .obj => |obj| switch (obj.type) {
                .lambda => .{ .lambda = @fieldParentPtr("obj", obj) },
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    const Callable = union(enum) {
        builtin: *const fn (*Runner, ?*Value.Cons) Error!Value,
        lambda: *Value.Lambda,
    };

    const Call = struct {
        program: *Value.Program,
        offset: usize,
        stack: std.ArrayListUnmanaged(Value) = .{},
    };

    const Builtins = struct {
        fn is_list(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .nil => true,
                .obj => |obj| obj.type == .cons,
                else => false,
            } };
        }
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

test "run var" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .num = 4 },
            .{ .local = 0 },
            .{ .local = 0 },
            .mul,
            .ret,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 16 }, value);
}

test "run lambda" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .lambda = .{ .caps = 0, .len = 13 } },
            .{ .local = 0 },
            .{ .jmp_if = 2 },
            .{ .pop = 0 },
            .no_match,
            .decons,
            .{ .jmp_if = 1 },
            .{ .jmp = 2 },
            .{ .pop = 0 },
            .no_match,
            .{ .local = 0 },
            .{ .local = 0 },
            .mul,
            .ret,
            .{ .local = 0 },
            .{ .num = 4 },
            .nil,
            .cons,
            .tail_call,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 16 }, value);
}

test "run closure" {
    var runner = Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .{ .num = 2 },
            .{ .local = 0 },
            .{ .lambda = .{ .caps = 1, .len = 13 } },
            .{ .local = 1 },
            .{ .jmp_if = 2 },
            .{ .pop = 1 },
            .no_match,
            .decons,
            .{ .jmp_if = 1 },
            .{ .jmp = 2 },
            .{ .pop = 1 },
            .no_match,
            .{ .local = 1 },
            .{ .local = 0 },
            .mul,
            .ret,
            .{ .local = 1 },
            .{ .num = 4 },
            .nil,
            .cons,
            .tail_call,
        },
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 8 }, value);
}
