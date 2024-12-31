const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const Expr = @import("expr.zig").Expr;

pub fn parse(allocator: std.mem.Allocator, content: []const u8) Parser.Error!Expr {
    var parser = Parser.init(allocator, content);
    const body = try parser.parseBody(&.{.eof});
    return body.expr;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,

    token: ?Token = null,
    expected: std.EnumSet(Token.Type) = undefined,
    skip_newlines: bool = false,

    const Error = std.mem.Allocator.Error || error{ParseError};

    fn init(allocator: std.mem.Allocator, content: []const u8) Parser {
        return .{
            .allocator = allocator,
            .lexer = Lexer.init(content),
        };
    }

    fn parseBody(self: *Parser, ends: []const Token.Type) Error!Body {
        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = false;

        while (self.skip(&.{.newline}) != null) {}

        const expr = try self.parseExpr();
        errdefer expr.deinit(self.allocator);

        while (self.skip(&.{.newline}) != null) {}

        const end = try self.expect(ends);

        return .{ .expr = expr, .end = end };
    }

    fn parseExpr(self: *Parser) Error!Expr {
        return self.parseLit();
    }

    fn parseLit(self: *Parser) Error!Expr {
        if (self.peek(&.{.num})) {
            return try self.parseNum();
        } else if (self.peek(&.{.bool})) {
            return try self.parseBool();
        } else if (self.peek(&.{.null})) {
            return try self.parseNull();
        } else {
            _ = try self.expect(&.{});
            unreachable;
        }
    }

    fn parseNum(self: *Parser) Error!Expr {
        const token = try self.expect(&.{.num});
        const value = std.fmt.parseFloat(f64, token.content) catch return error.ParseError;
        return .{ .num = value };
    }

    fn parseBool(self: *Parser) Error!Expr {
        const token = try self.expect(&.{.bool});
        const value = std.mem.eql(u8, token.content, "true");
        return .{ .bool = value };
    }

    fn parseNull(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.null});
        return .null;
    }

    fn peek(self: *Parser, token_types: []const Token.Type) bool {
        const token = while (true) {
            if (self.token) |token| {
                switch (token.type) {
                    .space => {},
                    .newline => if (!self.skip_newlines) break token,
                    else => break token,
                }
            }
            self.token = self.lexer.next();
            self.expected = .{};
        };

        var found = false;
        for (token_types) |token_type| {
            if (token_type == token.type) found = true;
            self.expected.insert(token_type);
        }
        return found;
    }

    fn skip(self: *Parser, token_types: []const Token.Type) ?Token {
        if (!self.peek(token_types)) return null;
        const token = self.token.?;
        self.token = null;
        return token;
    }

    fn expect(self: *Parser, token_types: []const Token.Type) !Token {
        if (self.skip(token_types)) |token| return token;

        const token = self.token.?;

        const pos = self.lexer.getPos(token);
        std.debug.print("{}:{}: ", .{ pos.line, pos.column });

        if (token.type == .unknown) {
            std.debug.print("unknown token: {s}\n", .{token.content});
        } else {
            std.debug.print("got {s}, expected ", .{@tagName(token.type)});
            const n = self.expected.count();
            if (n == 0) {
                std.debug.print("nothing", .{});
            } else {
                var iter = self.expected.iterator();
                for (0..n) |i| {
                    if (i == 0) {
                        // no prefix
                    } else if (i == n - 1) {
                        std.debug.print(" or ", .{});
                    } else {
                        std.debug.print(", ", .{});
                    }
                    std.debug.print("{s}", .{@tagName(iter.next().?)});
                }
            }
            std.debug.print("\n", .{});
        }

        return error.ParseError;
    }
};

const Body = struct {
    expr: Expr,
    end: Token,
};

test "parse num 1" {
    const expr = try parse(std.testing.allocator, "1337");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .num = 1337 }, expr);
}

test "parse num 2" {
    const expr = try parse(std.testing.allocator, "45.67");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .num = 45.67 }, expr);
}

test "parse bool 1" {
    const expr = try parse(std.testing.allocator, "true");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .bool = true }, expr);
}

test "parse bool 2" {
    const expr = try parse(std.testing.allocator, "false");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .bool = false }, expr);
}

test "parse null" {
    const expr = try parse(std.testing.allocator, "null");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr.null, expr);
}
