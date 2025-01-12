const std = @import("std");

const Runner = @import("runner.zig").Runner;
const Value = @import("value.zig").Value;
const paths = @import("paths.zig");

pub fn main() void {
    innerMain() catch std.process.exit(1);
}

fn innerMain() Runner.Error!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var runner = try Runner.init(allocator);
    defer runner.deinit();

    var args = std.process.args();
    _ = args.next();
    while (args.next()) |arg| {
        const path = try runner.wrapError(paths.normalize(allocator, arg));
        try runner.wrapError(runner.runPathAndMain(path));
    }
}
