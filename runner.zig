const std = @import("std");

const Program = @import("program.zig").Program;
const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;
const internal_parse = @import("parser.zig").internal_parse;
const Buffer = @import("compiler.zig").Buffer;
const Compiler = @import("compiler.zig").Compiler;

const stdlib = .{
    .send = (
        \\|iter, value| do
        \\  if [*list] = iter do
        \\    if [head, *tail] = list do
        \\      [head, tail]
        \\    else
        \\      [null, null]
        \\    end
        \\  elif is_str(iter) do
        \\    @str_send(iter)
        \\  elif is_dict(iter) do
        \\    @dict_send(iter)
        \\  else
        \\    iter(value)
        \\  end
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
        \\|*iters| do
        \\  [iter, *iters] = iters else return []
        \\  if is_list(iter) and not iters do
        \\    iter
        \\  elif [head, tail] = next(iter) do
        \\    [*tail] = list(tail, *iters)
        \\    [head, *tail]
        \\  else
        \\    list(*iters)
        \\  end
        \\end
    ),
    .dict = (
        \\|*iters| do
        \\  [{*target}, *iters] = iters else return dict({}, *iters)
        \\  [iter, *iters] = iters else return target
        \\  if [head, tail] = next(iter) do
        \\    [key, value] = head
        \\    dict({*target, key = value}, tail, *iters)
        \\  else
        \\    dict(target, *iters)
        \\  end
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
        \\|iter, f, acc| do
        \\  [head, tail] = next(iter) else return acc
        \\  reduce(tail, f, f(acc, head))
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
    .len = (
        \\|*args| match args do
        \\  [iter, acc] = do
        \\    [_, tail] = next(iter) else return acc
        \\    len(tail, acc + 1)
        \\  end
        \\  [iter] = len(iter, 0)
        \\end
    ),
    .@"@dict_tail" = (
        \\|dict, key| |_| @dict_send(dict, key)
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
    const min_next_gc = 8 * 1024;

    allocator: std.mem.Allocator,
    calls: std.ArrayListUnmanaged(*Call) = .{},
    last: ?*Value.Obj = null,
    stdlib_values: StdlibValues,

    next_gc: usize,
    allocated: usize = 0,

    pub const Error = std.mem.Allocator.Error || error{RunError};

    pub fn init(allocator: std.mem.Allocator) Runner.Error!Runner {
        var offsets: StdlibOffsets = undefined;
        const base_program = program: {
            var buffer = Buffer.init(allocator);
            defer buffer.deinit();

            var compiler = Compiler.init(&buffer);
            defer compiler.deinit();

            inline for (@typeInfo(Stdlib).Struct.fields) |field| {
                @field(offsets, field.name) = compiler.instrs.items.len;
                const content = @field(stdlib, field.name);
                const expr = internal_parse(allocator, content) catch |err| switch (err) {
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
            .next_gc = std.math.maxInt(usize),
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

        self.next_gc = 0;
        self.checkGc();

        return self;
    }

    pub fn deinit(self: *Runner) void {
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

    pub fn create(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) !*obj_type.Detail() {
        const detail = try self.allocator.create(obj_type.Detail());
        detail.* = data;
        detail.obj = .{
            .type = obj_type,
            .prev = self.last,
            .seen = false,
            .hash = hash: {
                var hasher = Value.Hasher{};
                hasher.add(@intFromEnum(obj_type));
                data.hash(&hasher);
                break :hash hasher.hash;
            },
        };
        self.last = &detail.obj;
        self.allocated += @sizeOf(obj_type.Detail()) + detail.allocated();
        return detail;
    }

    fn checkGc(self: *Runner) void {
        if (self.allocated >= self.next_gc) {
            self.runGc();
            self.next_gc = @max(self.allocated * 2, min_next_gc);
        }
    }

    fn runGc(self: *Runner) void {
        inline for (@typeInfo(Stdlib).Struct.fields) |field| {
            @field(self.stdlib_values, field.name).mark();
        }

        for (self.calls.items) |call| {
            call.program.obj.mark();
            for (call.stack.items) |value| value.mark();
        }

        var curr = &self.last;
        while (curr.*) |obj| {
            if (obj.seen) {
                curr = &obj.prev;
                obj.seen = false;
            } else {
                curr.* = obj.prev;
                switch (obj.type) {
                    inline else => |obj_type| {
                        const detail = obj_type.detailed(obj);
                        self.allocated -= @sizeOf(obj_type.Detail()) + detail.allocated();
                        detail.deinit(self.allocator);
                        self.allocator.destroy(detail);
                    },
                }
            }
        }
    }

    pub fn createValue(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) !Value {
        const detail = try self.create(obj_type, data);
        return detail.obj.toValue();
    }

    pub fn createList(self: *Runner, items: []const Value) !?*Value.Cons {
        var tail: ?*Value.Cons = null;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            tail = try self.create(.cons, .{ .head = items[i], .tail = tail });
        }
        return tail;
    }

    pub fn createListValue(self: *Runner, items: []const Value) !Value {
        return Value.Cons.toValue(try self.createList(items));
    }

    pub fn runProgram(self: *Runner, program: Program) Error!Value {
        const program_ = self.create(.program, .{ .instrs = program.instrs, .data = program.data }) catch |err| {
            program.deinit(self.allocator);
            return err;
        };
        var call = Call{ .program = program_, .offset = 0 };
        defer call.stack.deinit(self.allocator);
        return try self.run(&call);
    }

    pub fn run(self: *Runner, call: *Call) Error!Value {
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
                .empty_dict => {
                    const dict = try self.createValue(.dict, .{ .data = Value.Dict.empty_data });
                    try call.stack.append(self.allocator, dict);
                },
                .put_dict => {
                    const value = call.stack.pop();
                    const key = call.stack.pop();
                    var dict = try expectDict(call.stack.pop());
                    dict = try dict.put(self, key, value);
                    try call.stack.append(self.allocator, dict.obj.toValue());
                },
                .pop_dict => {
                    const key = call.stack.pop();
                    const dict = try expectDict(call.stack.pop());
                    const result = try dict.pop(self, key);
                    try call.stack.append(self.allocator, result.value);
                    try call.stack.append(self.allocator, result.dict.obj.toValue());
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
                .get => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, switch (lhs) {
                        .obj => |obj| switch (obj.type) {
                            .cons => get: {
                                var index: usize = @intFromFloat(try expectNum(rhs));
                                var cons = Value.ObjType.cons.detailed(obj);
                                while (index > 0) : (index -= 1) {
                                    cons = cons.tail orelse return error.RunError;
                                }
                                break :get cons.head;
                            },
                            .dict => Value.ObjType.dict.detailed(obj).get(rhs) orelse return error.RunError,
                            else => return error.RunError,
                        },
                        else => return error.RunError,
                    });
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
                .in => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, .{ .bool = switch (rhs) {
                        .nil => false,
                        .obj => |obj| switch (obj.type) {
                            .cons => in: {
                                var cons = Value.ObjType.cons.detailed(obj);
                                while (true) {
                                    if (cons.head.eql(lhs)) break :in true;
                                    cons = cons.tail orelse break :in false;
                                }
                            },
                            .dict => Value.ObjType.dict.detailed(obj).get(lhs) != null,
                            else => return error.RunError,
                        },
                        else => return error.RunError,
                    } });
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
        self.checkGc();
    }

    pub fn expectNum(value: Value) !f64 {
        return switch (value) {
            .num => |num| num,
            else => error.RunError,
        };
    }

    pub fn expectStr(value: *const Value) ![]const u8 {
        return value.toStr() orelse error.RunError;
    }

    pub fn expectCons(value: Value) !*Value.Cons {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    pub fn expectDict(value: Value) !*Value.Dict {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .dict => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    pub fn expectLambda(value: Value) !*Value.Lambda {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .lambda => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    pub fn expectList(value: Value) !?*Value.Cons {
        return switch (value) {
            .nil => null,
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => error.RunError,
            },
            else => error.RunError,
        };
    }

    pub fn expectCallable(value: Value) !Callable {
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

    pub const Call = struct {
        program: *Value.Program,
        offset: usize,
        stack: std.ArrayListUnmanaged(Value) = .{},
    };

    const Builtins = struct {
        fn is_num(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .num };
        }

        fn is_bool(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .bool };
        }

        fn is_null(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .null };
        }

        fn is_str(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .str => true,
                .obj => |obj| obj.type == .str,
                else => false,
            } };
        }

        fn is_list(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .nil => true,
                .obj => |obj| obj.type == .cons,
                else => false,
            } };
        }

        fn is_dict(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .obj => |obj| obj.type == .dict,
                else => false,
            } };
        }

        fn is_func(_: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .builtin => true,
                .obj => |obj| obj.type == .lambda,
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

        fn @"@str_send"(self: *Runner, args: ?*Value.Cons) Error!Value {
            const arg = try Value.Cons.expectOne(args);
            const content = try expectStr(&arg);

            var head: Value = undefined;
            var tail: Value = undefined;
            if (initCharLen(content)) |char_len| {
                head = try self.strSlice(arg, content[0..char_len]);
                tail = try self.strSlice(arg, content[char_len..]);
            } else {
                head = .null;
                tail = .null;
            }

            return try self.createListValue(&.{ head, tail });
        }

        fn @"@dict_send"(self: *Runner, args: ?*Value.Cons) Error!Value {
            var arg_iter = Value.Cons.iter(args);
            const dict = try expectDict(try arg_iter.expectNext());
            const key = arg_iter.next();
            try arg_iter.expectEnd();

            var head: Value = undefined;
            var tail: Value = undefined;
            if (dict.next(key)) |item| {
                head = item.obj.toValue();
                tail = try self.runLambda(
                    self.stdlib_values.@"@dict_tail",
                    &.{ dict.obj.toValue(), item.head },
                );
            } else {
                head = .null;
                tail = .null;
            }
            return try self.createListValue(&.{ head, tail });
        }
    };

    fn initCharLen(content: []const u8) ?usize {
        if (content.len == 0) return null;
        return 1;
    }

    fn strSlice(self: *Runner, value: Value, content: []const u8) Error!Value {
        if (Value.toShort(content)) |short| return .{ .str = short };
        const str = Value.ObjType.str.detailed(value.obj);
        return try self.createValue(.str, .{
            .content = content,
            .source = switch (str.source) {
                .alloc => .{ .slice = str },
                else => str.source,
            },
        });
    }

    fn next(self: *Runner, iter: Value) Error!?[2]Value {
        const result = try self.runLambda(self.stdlib_values.next, &.{iter});
        if (result == .null) return null;
        return try Value.Cons.expectN(try expectList(result), 2);
    }

    pub fn runLambda(self: *Runner, value: Value, args: []const Value) Error!Value {
        const lambda = try expectLambda(value);
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
        .data = try std.testing.allocator.dupe(u8, program.data),
    };
}

test "run num 1" {
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
    var runner = try Runner.init(std.testing.allocator);
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
test "compile dict" {
    var runner = try Runner.init(std.testing.allocator);
    defer runner.deinit();

    const program = try cloneProgram(.{
        .instrs = &.{
            .empty_dict,
            .{ .short_str = Value.toShort("foo").? },
            .{ .num = 1 },
            .put_dict,
            .{ .short_str = Value.toShort("bar").? },
            .{ .num = 2 },
            .put_dict,
            .{ .short_str = Value.toShort("baz").? },
            .{ .num = 3 },
            .put_dict,
            .{ .short_str = Value.toShort("foo").? },
            .{ .local = 1 },
            .{ .local = 0 },
            .in,
            .{ .jmp_if = 3 },
            .{ .pop = 1 },
            .{ .pop = 0 },
            .no_match,
            .pop_dict,
            .{ .pop = 1 },
            .{ .local = 0 },
            .ret,
        },
        .data = "",
    });
    const value = try runner.runProgram(program);

    try std.testing.expectEqualDeep(Value{ .num = 1 }, value);
}
