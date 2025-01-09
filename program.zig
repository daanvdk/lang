const std = @import("std");

const Instr = @import("instr.zig").Instr;
const Value = @import("value.zig").Value;

pub const Program = struct {
    instrs: []const Instr,
    data: []const u8,

    pub fn deinit(self: Program, allocator: std.mem.Allocator) void {
        allocator.free(self.instrs);
        allocator.free(self.data);
    }

    pub fn format(self: Program, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try self.write(writer, 0, 0);
    }

    fn write(self: Program, writer: anytype, init_i: usize, indent: usize) !void {
        var i = init_i;
        while (i < self.instrs.len) {
            try writer.print("{:0>4} ", .{i});
            for (0..indent) |_| try writer.print("│   ", .{});

            const instr = self.instrs[i];
            i += 1;

            switch (instr) {
                .num => |value| try writer.print("num: {d}\n", .{value}),
                .global => |value| try writer.print("global: {s}\n", .{@tagName(value)}),
                .lambda => |value| {
                    try writer.print("lambda: {d}\n", .{value.caps});
                    try (Program{
                        .instrs = self.instrs[0 .. i + value.len],
                        .data = self.data,
                    }).write(writer, i, indent + 1);
                    i += value.len;
                },
                .short_str => |*short| {
                    try writer.print("str: ", .{});
                    try Value.writeStr(Value.fromShort(short), writer);
                    try writer.print("\n", .{});
                },
                .long_str => |value| {
                    try writer.print("str: ", .{});
                    try Value.writeStr(self.data[value.index .. value.index + value.len], writer);
                    try writer.print("\n", .{});
                },
                inline else => |value, tag| {
                    try writer.print("{s}", .{@tagName(tag)});
                    if (@TypeOf(value) != void) {
                        try writer.print(": {any}", .{value});
                    }
                    try writer.print("\n", .{});
                },
            }
        }
    }
};
