const std = @import("std");

const parse = @import("parser.zig").parse;
const compile = @import("compiler.zig").compile;
const Runner = @import("runner.zig").Runner;
const Value = @import("value.zig").Value;

const MAIN = .{ .str = Value.toShort("main").? };

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var runner = try Runner.init(allocator);
    defer runner.deinit();

    var args = std.process.args();
    _ = args.next();
    while (args.next()) |arg| {
        const module = try Runner.expectDict(try runner.runProgram(program: {
            const file = try std.fs.cwd().openFile(arg, .{});
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

            const expr = try parse(allocator, content);
            defer expr.deinit(allocator);
            break :program try compile(allocator, expr);
        }));

        if (module.get(MAIN)) |main_| {
            _ = try runner.runLambda(main_, &.{});
        }
    }
}
