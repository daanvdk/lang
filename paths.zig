const std = @import("std");

pub const PathBuilder = struct {
    path: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) !PathBuilder {
        var self = PathBuilder{ .path = std.ArrayList(u8).init(allocator) };
        try self.path.appendSlice(".");
        return self;
    }

    pub fn deinit(self: *PathBuilder) void {
        self.path.deinit();
    }

    pub fn result(self: *PathBuilder) ![]const u8 {
        return try self.path.toOwnedSlice();
    }

    pub fn append(self: *PathBuilder, path: []const u8) !void {
        var offset: usize = undefined;
        if (std.mem.startsWith(u8, path, "/")) {
            try self.replace("/");
            offset = 1;
        } else {
            offset = 0;
        }

        var iter = std.mem.tokenize(u8, path[offset..], "/");
        while (iter.next()) |name| {
            if (std.mem.eql(u8, ".", name) or std.mem.eql(u8, "", name)) {
                // do nothing
            } else if (std.mem.eql(u8, "..", name)) {
                try self.pop();
            } else {
                try self.push(name);
            }
        }
    }

    fn replace(self: *PathBuilder, path: []const u8) !void {
        try self.path.replaceRange(0, self.path.items.len, path);
    }

    fn push(self: *PathBuilder, name: []const u8) !void {
        if (std.mem.eql(u8, self.path.items, "/")) {
            try self.path.appendSlice(name);
        } else if (std.mem.eql(u8, self.path.items, ".")) {
            try self.replace(name);
        } else {
            try self.path.ensureUnusedCapacity(name.len + 1);
            self.path.appendAssumeCapacity('/');
            self.path.appendSliceAssumeCapacity(name);
        }
    }

    fn pop(self: *PathBuilder) !void {
        const sep = std.mem.lastIndexOf(u8, self.path.items, "/");
        const offset = if (sep) |index| index + 1 else 0;
        const name = self.path.items[offset..];

        if (std.mem.eql(u8, name, "")) {
            std.debug.assert(offset == 1);
            return error.PopRoot;
        } else if (std.mem.eql(u8, name, ".")) {
            std.debug.assert(offset == 0);
            try self.replace("..");
        } else if (std.mem.eql(u8, name, "..")) {
            try self.path.appendSlice("/..");
        } else if (sep) |index| {
            self.path.shrinkRetainingCapacity(index);
        } else {
            try self.replace(".");
        }
    }
};

pub fn normalize(allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    var builder = try PathBuilder.init(allocator);
    defer builder.deinit();
    try builder.append(path);
    return try builder.result();
}

pub fn join(allocator: std.mem.Allocator, paths: []const []const u8) ![]const u8 {
    var builder = try PathBuilder.init(allocator);
    defer builder.deinit();
    for (paths) |path| try builder.append(path);
    return try builder.result();
}

pub fn join_import(allocator: std.mem.Allocator, curr_path: []const u8, import_path: []const u8) ![]const u8 {
    var builder = try PathBuilder.init(allocator);
    defer builder.deinit();

    try builder.append(curr_path);
    try builder.pop();
    try builder.append(import_path);
    if (!std.mem.endsWith(u8, builder.path.items, ".lang")) {
        try builder.path.appendSlice(".lang");
    }

    return try builder.result();
}
