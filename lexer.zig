const std = @import("std");

const Token = @import("token.zig").Token;

pub const Lexer = struct {
    content: []const u8,
    index: usize = 0,
    next_token: ?Token = null,
    internal: bool = false,

    pub fn init(content: []const u8) Lexer {
        return .{ .content = content };
    }

    pub fn next(self: *Lexer) Token {
        if (self.next_token) |token| {
            self.next_token = null;
            return token;
        }

        if (self.nextKnown()) |token| {
            return token;
        }

        const start = self.index;
        const end = while (true) {
            self.index += 1;
            if (self.nextKnown()) |token| {
                self.next_token = token;
                break self.index - token.content.len;
            }
        };

        return .{
            .type = .unknown,
            .content = self.content[start..end],
        };
    }

    fn nextKnown(self: *Lexer) ?Token {
        const start = self.index;
        const token_type = self.nextKnownType() orelse return null;
        return .{
            .type = token_type,
            .content = self.content[start..self.index],
        };
    }

    fn nextKnownType(self: *Lexer) ?Token.Type {
        const char0 = self.nextChar() orelse return .eof;
        switch (char0) {
            'A'...'Z', 'a'...'z', '_', '@' => {
                if (char0 == '@' and !self.internal) {
                    self.pushChar(char0);
                    return null;
                }

                const start = self.index - 1;
                while (self.nextChar()) |char1| {
                    switch (char1) {
                        'A'...'Z', 'a'...'z', '0'...'9', '_' => {},
                        else => {
                            self.pushChar(char1);
                            break;
                        },
                    }
                }

                const content = self.content[start..self.index];
                inline for (@typeInfo(@TypeOf(keywords)).Struct.fields) |field| {
                    inline for (@field(keywords, field.name)) |keyword| {
                        if (std.mem.eql(u8, content, keyword)) return @field(Token.Type, field.name);
                    }
                }

                return .name;
            },
            '0'...'9' => {
                var has_decimal = false;
                while (self.nextChar()) |char1| {
                    switch (char1) {
                        '0'...'9' => {},
                        '.' => {
                            if (!has_decimal) {
                                if (self.nextChar()) |char2| {
                                    switch (char2) {
                                        '0'...'9' => {
                                            has_decimal = true;
                                            continue;
                                        },
                                        else => {
                                            self.pushChar(char2);
                                            break;
                                        },
                                    }
                                }
                            }
                            self.pushChar(char1);
                            break;
                        },
                        else => {
                            self.pushChar(char1);
                            break;
                        },
                    }
                }
                return .num;
            },
            '"' => return .str,

            '[' => return .llist,
            ']' => return .rlist,
            ',' => return .comma,
            '{' => return .ldict,
            '}' => return .rdict,
            '|' => return .lambda,

            '(' => return .lpar,
            ')' => return .rpar,
            '.' => return .dot,

            '^' => return .pow,
            '*' => return .mul,
            '/' => return .div,
            '+' => return .add,
            '-' => {
                if (self.nextChar()) |char1| {
                    if (char1 == '>') return .pipe;
                    self.pushChar(char1);
                }
                return .sub;
            },

            '=' => {
                if (self.nextChar()) |char1| {
                    if (char1 == '=') return .eq;
                    self.pushChar(char1);
                }
                return .assign;
            },
            '!' => {
                if (self.nextChar()) |char1| {
                    if (char1 == '=') return .ne;
                    self.pushChar(char1);
                }
                self.pushChar(char0);
                return null;
            },
            '<' => {
                if (self.nextChar()) |char1| {
                    if (char1 == '=') return .le;
                    self.pushChar(char1);
                }
                return .lt;
            },
            '>' => {
                if (self.nextChar()) |char1| {
                    if (char1 == '=') return .ge;
                    self.pushChar(char1);
                }
                return .gt;
            },

            '\n' => return .newline,
            ' ', '\t', '\r' => {
                while (self.nextChar()) |char1| {
                    switch (char1) {
                        ' ', '\t', '\r' => {},
                        else => {
                            self.pushChar(char1);
                            break;
                        },
                    }
                }
                return .space;
            },
            '#' => {
                while (self.nextChar()) |char1| {
                    switch (char1) {
                        '\n' => {
                            self.pushChar(char1);
                            break;
                        },
                        else => {},
                    }
                }
                return .comment;
            },

            else => {
                self.pushChar(char0);
                return null;
            },
        }
    }

    fn nextChar(self: *Lexer) ?u8 {
        if (self.index >= self.content.len) return null;
        const char = self.content[self.index];
        self.index += 1;
        return char;
    }

    fn pushChar(self: *Lexer, char: u8) void {
        self.index -= 1;
        std.debug.assert(self.content[self.index] == char);
    }

    pub fn getPos(self: *const Lexer, token: Token) Pos {
        const index = @intFromPtr(token.content.ptr) - @intFromPtr(self.content.ptr);
        return self.getIndexPos(index);
    }

    fn getIndexPos(self: *const Lexer, index: usize) Pos {
        var pos = Pos{ .line = 1, .column = 1 };
        for (self.content[0..index]) |char| {
            if (char == '\n') {
                pos.line += 1;
                pos.column = 1;
            } else {
                pos.column += 1;
            }
        }
        return pos;
    }

    pub fn strChunk(self: *Lexer, allocator: std.mem.Allocator) !Chunk {
        var content = std.ArrayList(u8).init(allocator);
        defer content.deinit();

        const expr = while (true) {
            var char = try self.expectChar();
            switch (char) {
                '"' => break false,
                '$' => {
                    if (self.nextChar()) |next_char| {
                        if (next_char == '{') break true;
                        self.pushChar(next_char);
                    }
                },
                '\\' => {
                    char = try self.expectChar();
                    switch (char) {
                        'n' => char = '\n',
                        't' => char = '\t',
                        'r' => char = '\r',
                        'x' => {
                            const d1 = try self.expectHexChar();
                            const d2 = try self.expectHexChar();
                            char = (d1 << 4) | d2;
                        },
                        else => {},
                    }
                },
                else => {},
            }
            try content.append(char);
        };

        return .{
            .content = try content.toOwnedSlice(),
            .expr = expr,
        };
    }

    fn expectChar(self: *Lexer) !u8 {
        if (self.nextChar()) |char| return char;
        const pos = self.getIndexPos(self.index);
        std.debug.print("{}:{}: unterminated str\n", .{ pos.line, pos.column });
        return error.ParseError;
    }

    fn expectHexChar(self: *Lexer) !u8 {
        const char = try self.expectChar();
        switch (char) {
            '0'...'9' => return char - '0',
            'a'...'f' => return char - 'a' + 10,
            'A'...'F' => return char - 'A' + 10,
            else => {
                self.pushChar(char);
                const pos = self.getIndexPos(self.index);
                std.debug.print("{}:{}: expected hex digit\n", .{ pos.line, pos.column });
                return error.ParseError;
            },
        }
    }

    pub const Pos = struct {
        line: usize,
        column: usize,
    };

    pub const Chunk = struct {
        content: []const u8,
        expr: bool,
    };

    const keywords = .{
        .bool = .{ "true", "false" },
        .null = .{"null"},
        .ignore = .{"_"},
        .not = .{"not"},
        .@"and" = .{"and"},
        .@"or" = .{"or"},
        .do = .{"do"},
        .end = .{"end"},
        .match = .{"match"},
        .@"if" = .{"if"},
        .elif = .{"elif"},
        .@"else" = .{"else"},
        .@"for" = .{"for"},
        .in = .{"in"},
        .yield = .{"yield"},
        .@"return" = .{"return"},
        .@"pub" = .{"pub"},
        .@"fn" = .{"fn"},
    };
};
