const std = @import("std");

const Runner = @import("runner.zig").Runner;
const Value = @import("value.zig").Value;
const paths = @import("paths.zig");

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
        const path = try paths.normalize(allocator, arg);
        const module = try Runner.expectDict(try runner.runPath(path));
        if (module.get(MAIN)) |main_| _ = try runner.runLambda(main_, &.{});
    }
}
