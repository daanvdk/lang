const std = @import("std");

const parse = @import("parser.zig").parse;
const compile = @import("compiler.zig").compile;
const run = @import("runner.zig").run;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = std.process.args();
    _ = args.next();
    while (args.next()) |arg| {
        try run(allocator, program: {
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
        });
    }
}
