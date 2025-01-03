const std = @import("std");

const Program = @import("program.zig").Program;
const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;
const parse = @import("parser.zig").parse;
const Buffer = @import("compiler.zig").Buffer;
const Compiler = @import("compiler.zig").Compiler;

pub fn run(allocator: std.mem.Allocator, program: Program) Runner.Error!void {
    var runner = try Runner.init(allocator);
    defer runner.deinit();
    _ = try runner.runProgram(program);
}

const stdlib = .{
    .send = (
        \\|iter, value| match iter do
        \\  [*list] = match list do
        \\    [head, *tail] = [head, tail]
        \\    _             = [null, null]
        \\  end
        \\  _ = iter(value)
        \\end
    ),
    .next = (
        \\|iter| do
        \\  [head, tail] = send(iter, null)
        \\  if tail == null do
        \\    null
        \\  else
        \\    [head, tail]
        \\  end
        \\end
    ),
    .list = (
        \\|*iters| match iters do
        \\  [[*list]] = list
        \\  [iter, *iters] = match next(iter) do
        \\    [head, tail] = do
        \\      [*tail] = list(tail, *iters)
        \\      [head, *tail]
        \\    end
        \\    _ = list(*iters)
        \\  end
        \\  _ = []
        \\end
    ),
    .map = (
        \\|iter, f| do*
        \\  for item in iter do
        \\    yield f(item)
        \\  end
        \\end
    ),
    .filter = (
        \\|iter, f| do*
        \\  for item in iter do
        \\    if f(item) do yield item end
        \\  end
        \\end
    ),
    .reduce = (
        \\|iter, f, acc| match next(iter) do
        \\  [head, tail] = reduce(tail, f, f(acc, head))
        \\  _ = acc
        \\end
    ),
    .count = (
        \\|*args| match args do
        \\  [start, stop, step] = do*
        \\    if (if step >= 0 do start < stop else start > stop end) do
        \\      yield start
        \\      yield* count(start + step, stop, step)
        \\    end
        \\  end
        \\  [start, stop] = count(start, stop, 1)
        \\  [stop] = count(0, stop, 1)
        \\end
    ),
};

const Stdlib = @TypeOf(stdlib);

const StdlibOffsets = offsets: {
    const stdfields = @typeInfo(Stdlib).Struct.fields;
    var fields: [stdfields.len]std.builtin.Type.StructField = undefined;

    for (0.., stdfields) |i, field| fields[i] = .{
        .name = field.name,
        .type = usize,
        .default_value = null,
        .is_comptime = false,
        .alignment = 0,
    };

    break :offsets @Type(.{ .Struct = .{
        .fields = &fields,
        .decls = &.{},
        .layout = .auto,
        .is_tuple = false,
    } });
};

const StdlibValues = values: {
    const stdfields = @typeInfo(Stdlib).Struct.fields;
    var fields: [stdfields.len]std.builtin.Type.StructField = undefined;

    for (0.., stdfields) |i, field| fields[i] = .{
        .name = field.name,
        .type = Value,
        .default_value = null,
        .is_comptime = false,
        .alignment = 0,
    };

    break :values @Type(.{ .Struct = .{
        .fields = &fields,
        .decls = &.{},
        .layout = .auto,
        .is_tuple = false,
    } });
};

pub const Runner = struct {
    allocator: std.mem.Allocator,
    calls: std.ArrayListUnmanaged(*Call) = .{},
    last: ?*Value.Obj = null,
    stdlib_values: StdlibValues,

    pub const Error = std.mem.Allocator.Error || error{RunError};

    fn init(allocator: std.mem.Allocator) Runner.Error!Runner {
        var offsets: StdlibOffsets = undefined;
        const base_program = program: {
            var buffer = Buffer.init(allocator);
            defer buffer.deinit();

            var compiler = Compiler.init(&buffer);
            defer compiler.deinit();

            inline for (@typeInfo(Stdlib).Struct.fields) |field| {
                @field(offsets, field.name) = compiler.instrs.items.len;
                const content = @field(stdlib, field.name);
                const expr = parse(allocator, content) catch |err| switch (err) {
                    error.ParseError => unreachable,
                    inline else => |err_| return err_,
                };
                defer expr.deinit(allocator);
                _ = compiler.compileExpr(expr, .returned) catch |err| switch (err) {
                    error.CompileError => unreachable,
                    inline else => |err_| return err_,
                };
            }

            const data = try buffer.content.toOwnedSlice();
            errdefer allocator.free(data);
            const instrs = try compiler.instrs.toOwnedSlice();
            break :program Program{ .instrs = instrs, .data = data };
        };

        var self = Runner{
            .allocator = allocator,
            .stdlib_values = undefined,
        };
        errdefer self.deinit();

        const program = self.create(.program, .{ .instrs = base_program.instrs, .data = base_program.data }) catch |err| {
            base_program.deinit(allocator);
            return err;
        };

        inline for (@typeInfo(Stdlib).Struct.fields) |field| {
            var call = Call{
                .program = program,
                .offset = @field(offsets, field.name),
            };
            defer call.stack.deinit(allocator);
            @field(self.stdlib_values, field.name) = try self.run(&call);
        }

        return self;
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

    fn createList(self: *Runner, items: []const Value) !?*Value.Cons {
        var tail: ?*Value.Cons = null;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            tail = try self.create(.cons, .{ .head = items[i], .tail = tail });
        }
        return tail;
    }

    fn createListValue(self: *Runner, items: []const Value) !Value {
        return Value.Cons.toValue(try self.createList(items));
    }

    fn runProgram(self: *Runner, program: Program) Error!Value {
        const program_ = self.create(.program, .{ .instrs = program.instrs, .data = program.data }) catch |err| {
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
            // std.debug.print("{any}\n{}\n", .{ call.stack.items, instr });
            ip += 1;
            switch (instr) {
                .global => |global| {
                    var value: Value = undefined;
                    switch (global) {
                        inline else => |tag| {
                            const field = @tagName(tag);
                            if (@hasField(Stdlib, field)) {
                                value = @field(self.stdlib_values, field);
                            } else {
                                value = .{ .builtin = @field(Builtins, field) };
                            }
                        },
                    }
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
                .short_str => |short| {
                    try call.stack.append(self.allocator, .{ .str = short });
                },
                .long_str => |str| {
                    const content = call.program.data[str.index .. str.index + str.len];
                    const value = try self.createValue(.str, .{ .content = content, .source = .{ .data = call.program } });
                    try call.stack.append(self.allocator, value);
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
                    const stack = try self.allocator.alloc(Value, lambda.caps);
                    const index = call.stack.items.len - lambda.caps;
                    @memcpy(stack, call.stack.items[index..]);
                    call.stack.shrinkRetainingCapacity(index);

                    const value = self.createValue(.lambda, .{
                        .program = call.program,
                        .offset = (@intFromPtr(ip) - @intFromPtr(call.program.instrs.ptr)) / @sizeOf(Instr),
                        .stack = stack,
                    }) catch |err| {
                        self.allocator.free(stack);
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
                .jmp_back => |jmp| {
                    ip -= jmp;
                },
                .jmp_if => |jmp| {
                    if (call.stack.pop().truthy()) ip += jmp;
                },

                .ret => {
                    return call.stack.pop();
                },
                .yield => {
                    const head = call.stack.pop();
                    const stack = try call.stack.toOwnedSlice(self.allocator);
                    const tail = self.createValue(.lambda, .{
                        .program = call.program,
                        .offset = (@intFromPtr(ip) - @intFromPtr(call.program.instrs.ptr)) / @sizeOf(Instr),
                        .stack = stack,
                    }) catch |err| {
                        self.allocator.free(stack);
                        return err;
                    };
                    return try self.createListValue(&.{ head, tail });
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

    fn expectLambda(value: Value) !*Value.Lambda {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .lambda => @fieldParentPtr("obj", obj),
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

        fn str(self: *Runner, args: ?*Value.Cons) Error!Value {
            var iter = Value.Cons.iter(args);
            var buffer: std.ArrayListUnmanaged(u8) = .{};
            defer buffer.deinit(self.allocator);

            while (iter.next()) |arg| {
                if (arg.toStr()) |content| {
                    try buffer.appendSlice(self.allocator, content);
                } else {
                    try buffer.writer(self.allocator).print("{}", .{arg});
                }
            }

            if (Value.toShort(buffer.items)) |short| {
                return .{ .str = short };
            } else {
                const content = try buffer.toOwnedSlice(self.allocator);
                errdefer self.allocator.free(content);
                return self.createValue(.str, .{ .content = content, .source = .alloc });
            }
        }

        fn join(self: *Runner, args: ?*Value.Cons) Error!Value {
            var arg_iter = Value.Cons.iter(args);
            var iter = try arg_iter.expectNext();
            const joiner = arg_iter.next() orelse Value{ .str = Value.toShort("").? };
            try arg_iter.expectEnd();

            var joiner_buffer: std.ArrayListUnmanaged(u8) = .{};
            defer joiner_buffer.deinit(self.allocator);
            if (joiner.toStr()) |content| {
                try joiner_buffer.appendSlice(self.allocator, content);
            } else {
                try joiner_buffer.writer(self.allocator).print("{}", .{joiner});
            }
            joiner_buffer.shrinkAndFree(self.allocator, joiner_buffer.items.len);

            var buffer: std.ArrayListUnmanaged(u8) = .{};
            defer buffer.deinit(self.allocator);

            var first = true;
            while (try self.next(iter)) |pair| {
                if (first) {
                    first = false;
                } else {
                    try buffer.appendSlice(self.allocator, joiner_buffer.items);
                }
                if (pair[0].toStr()) |content| {
                    try buffer.appendSlice(self.allocator, content);
                } else {
                    try buffer.writer(self.allocator).print("{}", .{pair[0]});
                }
                iter = pair[1];
            }

            if (Value.toShort(buffer.items)) |short| {
                return .{ .str = short };
            } else {
                const content = try buffer.toOwnedSlice(self.allocator);
                errdefer self.allocator.free(content);
                return self.createValue(.str, .{ .content = content, .source = .alloc });
            }
        }

        fn print(_: *Runner, args: ?*Value.Cons) Error!Value {
            var iter = Value.Cons.iter(args);
            var first = true;

            while (iter.next()) |arg| {
                if (first) {
                    first = false;
                } else {
                    std.debug.print(" ", .{});
                }
                if (arg.toStr()) |content| {
                    std.debug.print("{s}", .{content});
                } else {
                    std.debug.print("{}", .{arg});
                }
            }
            std.debug.print("\n", .{});

            return .null;
        }
    };

    fn next(self: *Runner, iter: Value) Error!?[2]Value {
        const result = try self.runGlobal("next", &.{iter});
        if (result == .null) return null;
        return try Value.Cons.expectN(try expectList(result), 2);
    }

    fn runGlobal(self: *Runner, comptime field: []const u8, args: []const Value) Error!Value {
        const lambda = try expectLambda(@field(self.stdlib_values, field));
        var call = Call{
            .program = lambda.program,
            .offset = lambda.offset,
        };
        defer call.stack.deinit(self.allocator);

        try call.stack.ensureUnusedCapacity(self.allocator, lambda.stack.len + 1);
        call.stack.appendSliceAssumeCapacity(lambda.stack);
        call.stack.appendAssumeCapacity(try self.createListValue(args));

        return try self.run(&call);
    }
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
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
        .data = "",
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 8 }, value);
}
