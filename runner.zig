const std = @import("std");

const Program = @import("program.zig").Program;
const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;
const parse = @import("parser.zig").parse;
const Parser = @import("parser.zig").Parser;
const internal_parse = @import("parser.zig").internal_parse;
const compile = @import("compiler.zig").compile;
const Buffer = @import("compiler.zig").Buffer;
const Compiler = @import("compiler.zig").Compiler;
const paths = @import("paths.zig");

const suffix = ".lang";
const main = .{ .str = Value.toShort("main").? };

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
    modules: std.StringHashMapUnmanaged(Value) = .{},
    last: ?*Value.Obj = null,
    stdlib_values: StdlibValues,

    next_gc: usize,
    allocated: usize = 0,

    pub const Error = error{RunError};

    pub fn init(allocator: std.mem.Allocator) Runner.Error!Runner {
        const stdlib_ = compileStdlib(allocator) catch |err| {
            std.debug.print("{s}\n", .{@errorName(err)});
            return error.RunError;
        };

        var self = Runner{
            .allocator = allocator,
            .stdlib_values = undefined,
            .next_gc = std.math.maxInt(usize),
        };
        errdefer self.deinit();

        const program = try self.createProgram("<stdlib>", stdlib_.program);

        inline for (@typeInfo(Stdlib).Struct.fields) |field| {
            var call = Call{
                .program = program,
                .offset = @field(stdlib_.offsets, field.name),
            };
            defer call.stack.deinit(allocator);
            @field(self.stdlib_values, field.name) = try self.run(&call);
        }

        self.next_gc = 0;
        self.checkGc();

        return self;
    }

    fn compileStdlib(allocator: std.mem.Allocator) !struct { offsets: StdlibOffsets, program: Program } {
        var buffer = Buffer.init(allocator);
        defer buffer.deinit();

        var compiler = Compiler.init(&buffer);
        defer compiler.deinit();

        var offsets: StdlibOffsets = undefined;
        inline for (@typeInfo(Stdlib).Struct.fields) |field| {
            @field(offsets, field.name) = compiler.instrs.items.len;
            const content = @field(stdlib, field.name);
            const expr = internal_parse(allocator, content) catch |err| {
                std.debug.print("{s}\n", .{@errorName(err)});
                return error.RunError;
            };
            defer expr.deinit(allocator);
            _ = compiler.compileExpr(expr, .returned) catch |err| {
                std.debug.print("{s}\n", .{@errorName(err)});
                return error.RunError;
            };
        }

        const data = try buffer.content.toOwnedSlice();
        errdefer allocator.free(data);
        const instrs = try compiler.instrs.toOwnedSlice();
        errdefer allocator.free(instrs);
        const locations = try compiler.locations.toOwnedSlice();
        return .{
            .offsets = offsets,
            .program = .{ .instrs = instrs, .data = data, .locations = locations },
        };
    }

    pub fn deinit(self: *Runner) void {
        self.calls.deinit(self.allocator);

        var path_iter = self.modules.keyIterator();
        while (path_iter.next()) |path| self.allocator.free(path.*);
        self.modules.deinit(self.allocator);

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

    pub fn create(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) Error!*obj_type.Detail() {
        const detail = try self.wrapError(self.allocator.create(obj_type.Detail()));
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

        var module_iter = self.modules.valueIterator();
        while (module_iter.next()) |module| module.mark();

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

    pub fn createValue(self: *Runner, comptime obj_type: Value.ObjType, data: obj_type.Detail()) Error!Value {
        const detail = try self.create(obj_type, data);
        return detail.obj.toValue();
    }

    pub fn createProgram(self: *Runner, path: []const u8, program: Program) Error!*Value.Program {
        errdefer program.deinit(self.allocator);
        return try self.create(.program, .{
            .path = path,
            .instrs = program.instrs,
            .data = program.data,
            .locations = program.locations,
        });
    }

    pub fn createList(self: *Runner, items: []const Value) Error!?*Value.Cons {
        var tail: ?*Value.Cons = null;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            tail = try self.create(.cons, .{ .head = items[i], .tail = tail });
        }
        return tail;
    }

    pub fn createListValue(self: *Runner, items: []const Value) Error!Value {
        return Value.Cons.toValue(try self.createList(items));
    }

    pub fn runPath(self: *Runner, path: []const u8) !Value {
        errdefer self.allocator.free(path);

        const result = try self.modules.getOrPut(self.allocator, path);
        if (result.found_existing) {
            self.allocator.free(path);
            return result.value_ptr.*;
        }

        errdefer self.modules.removeByPtr(result.key_ptr);

        const program = program: {
            const file = try std.fs.cwd().openFile(path, .{});
            defer file.close();

            const content = try std.posix.mmap(
                null,
                (try file.metadata()).size(),
                std.posix.PROT.READ,
                .{ .TYPE = .SHARED },
                file.handle,
                0,
            );
            defer std.posix.munmap(content);

            const expr = try parse(self.allocator, content);
            defer expr.deinit(self.allocator);
            break :program try compile(self.allocator, expr);
        };

        const value = try self.runProgram(path, program);
        result.value_ptr.* = value;
        return value;
    }

    pub fn runPathAndMain(self: *Runner, path: []const u8) !void {
        const module = try self.expectDict(null, try self.runPath(path));
        if (module.get(main)) |main_| _ = try self.runLambda(main_, &.{});
    }

    pub fn runProgram(self: *Runner, path: []const u8, program: Program) Error!Value {
        const program_ = try self.createProgram(path, program);
        var call = Call{ .program = program_, .offset = 0 };
        defer call.stack.deinit(self.allocator);
        return try self.run(&call);
    }

    pub fn run(self: *Runner, call: *Call) Error!Value {
        return try self.wrapError(self.innerRun(call));
    }

    fn innerRun(self: *Runner, call: *Call) !Value {
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
                    const tail = try self.expectList(.{ ip, 1 }, call.stack.pop());
                    const head = call.stack.pop();
                    const cons = try self.createValue(.cons, .{ .head = head, .tail = tail });
                    try call.stack.append(self.allocator, cons);
                },
                .decons => {
                    const cons = try self.expectCons(null, call.stack.pop());
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
                    var dict = try self.expectDict(null, call.stack.pop());
                    dict = try dict.put(self, key, value);
                    try call.stack.append(self.allocator, dict.obj.toValue());
                },
                .pop_dict => {
                    const key = call.stack.pop();
                    const dict = try self.expectDict(null, call.stack.pop());
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
                    const args = try self.expectList(.{ ip, 1 }, call.stack.pop());
                    const value = switch (try self.expectCallable(.{ ip, 0 }, call.stack.pop())) {
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

                            const location = self.getLocation(ip, 1);
                            call.location = location;
                            defer call.location = .{};
                            break :value try self.run(&call_);
                        },
                    };
                    try call.stack.append(self.allocator, value);
                },
                .tail_call => {
                    const args = try self.expectList(.{ ip, 1 }, call.stack.pop());
                    switch (try self.expectCallable(.{ ip, 0 }, call.stack.pop())) {
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

                            call.is_tail_call = true;

                            ip = call.program.instrs.ptr + call.offset;
                        },
                    }
                },
                .get => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, if (lhs.toStr()) |content| char: {
                        const index = try self.expectIndex(.{ ip, 1 }, rhs);
                        var offset: usize = 0;
                        for (0..index) |_| {
                            offset += initCharLen(content[offset..]) orelse return self.runError(.{ ip, 1 }, "Index out of bounds: {}\n", .{index});
                        }
                        const len = initCharLen(content[offset..]) orelse return self.runError(.{ ip, 1 }, "Index out of bounds: {}\n", .{index});
                        break :char try self.strSlice(lhs, content[offset .. offset + len]);
                    } else switch (lhs) {
                        .obj => |obj| switch (obj.type) {
                            .cons => get: {
                                const index = try self.expectIndex(.{ ip, 1 }, rhs);
                                var cons = Value.ObjType.cons.detailed(obj);
                                for (0..index) |_| {
                                    cons = cons.tail orelse return self.runError(.{ ip, 1 }, "Index out of bounds: {}\n", .{index});
                                }
                                break :get cons.head;
                            },
                            .dict => Value.ObjType.dict.detailed(obj).get(rhs) orelse return self.runError(.{ ip, 1 }, "Key error: {}\n", .{rhs}),
                            else => return self.runError(.{ ip, 0 }, "Expected an indexable value\n", .{}),
                        },
                        else => return self.runError(.{ ip, 0 }, "Expected an indexable value\n", .{}),
                    });
                },

                .pow => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = std.math.pow(f64, lhs, rhs) });
                },
                .pos => {
                    const value = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = value });
                },
                .neg => {
                    const value = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = -value });
                },
                .mul => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs * rhs });
                },
                .div => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs / rhs });
                },
                .add => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .num = lhs + rhs });
                },
                .sub => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
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
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs < rhs });
                },
                .le => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs <= rhs });
                },
                .gt => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs > rhs });
                },
                .ge => {
                    const rhs = try self.expectNum(.{ ip, 1 }, call.stack.pop());
                    const lhs = try self.expectNum(.{ ip, 0 }, call.stack.pop());
                    try call.stack.append(self.allocator, .{ .bool = lhs >= rhs });
                },
                .in => {
                    const rhs = call.stack.pop();
                    const lhs = call.stack.pop();
                    try call.stack.append(self.allocator, .{ .bool = if (rhs.toStr()) |haystack| in: {
                        const needle = try self.expectStr(.{ ip, 0 }, &lhs);
                        break :in std.mem.indexOf(u8, haystack, needle) != null;
                    } else switch (rhs) {
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
                            else => return self.runError(.{ ip, 1 }, "Expected a searchable value\n", .{}),
                        },
                        else => return self.runError(.{ ip, 1 }, "Expected a searchable value\n", .{}),
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
                .no_match => return self.runError(.{ ip, 0 }, "No match\n", .{}),
            }
        }
        self.checkGc();
    }

    fn getLocation(self: *Runner, ip: [*]const Instr, index: usize) Instr.Location {
        const program = self.calls.items[self.calls.items.len - 1].program;
        const offset = (@intFromPtr(ip - 1) - @intFromPtr(program.instrs.ptr)) / @sizeOf(Instr);

        var min_index: usize = 0;
        var max_index = program.locations.len;
        while (min_index < max_index) {
            const mid_index = (min_index + max_index) / 2;
            const location = program.locations[mid_index];

            if (location.offset < offset or (location.offset == offset and location.index < index)) {
                min_index = mid_index + 1;
            } else if (location.offset == offset and location.index == index) {
                return location.location;
            } else {
                max_index = mid_index;
            }
        }

        return .{};
    }

    const LocationSpec = std.meta.Tuple(&.{ [*]const Instr, usize });

    fn runError(self: *Runner, spec: ?LocationSpec, comptime fmt: []const u8, args: anytype) error{RunError} {
        const location = if (spec) |spec_| @call(.auto, Runner.getLocation, .{self} ++ spec_) else Instr.Location{};
        const call = self.calls.items[self.calls.items.len - 1];
        call.location = location;
        self.printStackTrace();
        std.debug.print(fmt, args);
        call.location = .{};
        return error.RunError;
    }

    pub fn wrapError(self: *Runner, result: anytype) Error!@typeInfo(@TypeOf(result)).ErrorUnion.payload {
        return result catch |err| return switch (@as(anyerror, err)) {
            error.RunError => error.RunError,
            else => self.runError(null, "{s}\n", .{@errorName(err)}),
        };
    }

    pub fn expectNum(self: *Runner, spec: ?LocationSpec, value: Value) Error!f64 {
        return switch (value) {
            .num => |num| num,
            else => self.runError(spec, "Expected a number\n", .{}),
        };
    }

    pub fn expectIndex(self: *Runner, spec: ?LocationSpec, value: Value) Error!usize {
        const num = try self.expectNum(spec, value);
        if (num < 0 or num != std.math.round(num)) {
            return self.runError(spec, "Expected a positive integer\n", .{});
        }
        return @intFromFloat(num);
    }

    pub fn expectStr(self: *Runner, spec: ?LocationSpec, value: *const Value) Error![]const u8 {
        return value.toStr() orelse self.runError(spec, "Expected a string\n", .{});
    }

    pub fn expectCons(self: *Runner, spec: ?LocationSpec, value: Value) Error!*Value.Cons {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => self.runError(spec, "Expected a non empty list\n", .{}),
            },
            else => self.runError(spec, "Expected a non empty list\n", .{}),
        };
    }

    pub fn expectDict(self: *Runner, spec: ?LocationSpec, value: Value) Error!*Value.Dict {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .dict => @fieldParentPtr("obj", obj),
                else => self.runError(spec, "Expected a dictionary\n", .{}),
            },
            else => self.runError(spec, "Expected a dictionary\n", .{}),
        };
    }

    pub fn expectLambda(self: *Runner, spec: ?LocationSpec, value: Value) Error!*Value.Lambda {
        return switch (value) {
            .obj => |obj| switch (obj.type) {
                .lambda => @fieldParentPtr("obj", obj),
                else => self.runError(spec, "Expected a function\n", .{}),
            },
            else => self.runError(spec, "Expected a function\n", .{}),
        };
    }

    pub fn expectList(self: *Runner, spec: ?LocationSpec, value: Value) Error!?*Value.Cons {
        return switch (value) {
            .nil => null,
            .obj => |obj| switch (obj.type) {
                .cons => @fieldParentPtr("obj", obj),
                else => self.runError(spec, "Expected a list\n", .{}),
            },
            else => self.runError(spec, "Expected a list\n", .{}),
        };
    }

    pub fn expectCallable(self: *Runner, spec: ?LocationSpec, value: Value) Error!Callable {
        return switch (value) {
            .builtin => |builtin| .{ .builtin = builtin },
            .obj => |obj| switch (obj.type) {
                .lambda => .{ .lambda = @fieldParentPtr("obj", obj) },
                else => self.runError(spec, "Expected a lambda function\n", .{}),
            },
            else => self.runError(spec, "Expected a lambda function\n", .{}),
        };
    }

    pub fn printStackTrace(self: *Runner) void {
        std.debug.print("Stacktrace:\n", .{});
        for (self.calls.items) |call| {
            if (call.is_tail_call) {
                std.debug.print("  ...some calls were optimized out by tail call optimization...\n", .{});
            }
            if (call.location.index == 0 and call.location.len == 0) continue;

            const file = std.fs.cwd().openFile(call.program.path, .{}) catch continue;
            defer file.close();

            const content = std.posix.mmap(
                null,
                (file.metadata() catch continue).size(),
                std.posix.PROT.READ,
                .{ .TYPE = .SHARED },
                file.handle,
                0,
            ) catch continue;
            defer std.posix.munmap(content);

            var line: usize = 1;
            var line_start: usize = 0;
            for (0.., content[0..call.location.index]) |index, char| {
                if (char == '\n') {
                    line += 1;
                    line_start = index + 1;
                }
            }

            std.debug.print("  File {s} at line {}:\n", .{ call.program.path, line });
            const start: usize = call.location.index;
            const end: usize = start + call.location.len;

            while (line_start < end) {
                var line_end: usize = undefined;
                if (std.mem.indexOf(u8, content[line_start..], "\n")) |line_len| {
                    line_end = line_start + line_len;
                } else {
                    line_end = content.len;
                }
                std.debug.print("    {s}\n", .{content[line_start..line_end]});

                std.debug.print("    ", .{});
                for (line_start..@min(line_end, end)) |index| {
                    std.debug.print("{c}", .{@as(u8, if (index < start) ' ' else '^')});
                }
                std.debug.print("\n", .{});

                line += 1;
                line_start = line_end + 1;
            }
        }
    }

    const Callable = union(enum) {
        builtin: *const fn (*Runner, ?*Value.Cons) anyerror!Value,
        lambda: *Value.Lambda,
    };

    pub const Call = struct {
        program: *Value.Program,
        offset: usize,
        stack: std.ArrayListUnmanaged(Value) = .{},
        location: Instr.Location = .{},
        is_tail_call: bool = false,
    };

    const Builtins = struct {
        fn is_num(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .num };
        }

        fn is_bool(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .bool };
        }

        fn is_null(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = arg == .null };
        }

        fn is_str(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .str => true,
                .obj => |obj| obj.type == .str,
                else => false,
            } };
        }

        fn is_list(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .nil => true,
                .obj => |obj| obj.type == .cons,
                else => false,
            } };
        }

        fn is_dict(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .obj => |obj| obj.type == .dict,
                else => false,
            } };
        }

        fn is_func(_: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            return .{ .bool = switch (arg) {
                .builtin => true,
                .obj => |obj| obj.type == .lambda,
                else => false,
            } };
        }

        fn str(self: *Runner, args: ?*Value.Cons) anyerror!Value {
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

        fn join(self: *Runner, args: ?*Value.Cons) anyerror!Value {
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

        fn print(_: *Runner, args: ?*Value.Cons) anyerror!Value {
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

        fn @"@str_send"(self: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            const content = try self.expectStr(null, &arg);

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

        fn @"@dict_send"(self: *Runner, args: ?*Value.Cons) anyerror!Value {
            var arg_iter = Value.Cons.iter(args);
            const dict = try self.expectDict(null, try arg_iter.expectNext());
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

        fn import(self: *Runner, args: ?*Value.Cons) anyerror!Value {
            const arg = try Value.Cons.expectOne(args);
            const import_path = try self.expectStr(null, &arg);
            const curr_path = self.calls.items[self.calls.items.len - 1].program.path;

            var import_paths = paths.ImportIter.init(self.allocator, curr_path, import_path);
            while (try import_paths.next()) |path| {
                return self.runPath(path) catch |err| switch (err) {
                    error.FileNotFound => continue,
                    inline else => |err_| return err_,
                };
            }

            return self.runError(null, "Module not found: {s}\n", .{import_path});
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
        return try Value.Cons.expectN(try self.expectList(null, result), 2);
    }

    pub fn runLambda(self: *Runner, value: Value, args: []const Value) Error!Value {
        const lambda = try self.expectLambda(null, value);
        var call = Call{
            .program = lambda.program,
            .offset = lambda.offset,
        };
        defer call.stack.deinit(self.allocator);

        try self.wrapError(call.stack.ensureUnusedCapacity(self.allocator, lambda.stack.len + 1));
        call.stack.appendSliceAssumeCapacity(lambda.stack);
        call.stack.appendAssumeCapacity(try self.createListValue(args));

        return try self.run(&call);
    }
};

fn cloneProgram(program: Program) !Program {
    return .{
        .instrs = try std.testing.allocator.dupe(Instr, program.instrs),
        .data = try std.testing.allocator.dupe(u8, program.data),
        .locations = try std.testing.allocator.dupe(Program.Location, program.locations),
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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

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
        .locations = &.{},
    });
    const value = try runner.runProgram("", program);

    try std.testing.expectEqualDeep(Value{ .num = 1 }, value);
}
