const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const Expr = @import("expr.zig").Expr;
const Pattern = @import("pattern.zig").Pattern;

pub fn parse(allocator: std.mem.Allocator, content: []const u8) Parser.Error!Expr {
    var parser = Parser.init(allocator, content);
    const body = try parser.parseBody(&.{.eof});
    return body.expr;
}

pub fn internal_parse(allocator: std.mem.Allocator, content: []const u8) Parser.Error!Expr {
    var parser = Parser.init(allocator, content);
    parser.lexer.internal = true;
    const body = try parser.parseBody(&.{.eof});
    return body.expr;
}

const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,

    token: ?Token = null,
    expected: std.EnumSet(Token.Type) = undefined,
    skip_newlines: bool = false,
    is_gen: bool = false,

    const Error = std.mem.Allocator.Error || error{ParseError};

    fn init(allocator: std.mem.Allocator, content: []const u8) Parser {
        return .{
            .allocator = allocator,
            .lexer = Lexer.init(content),
        };
    }

    fn parseBody(self: *Parser, ends: []const Token.Type) Error!Body {
        var lets = std.ArrayList(Let).init(self.allocator);
        var last_expr: ?Expr = null;
        defer {
            for (lets.items) |let| let.deinit(self.allocator);
            lets.deinit();
            if (last_expr) |expr| expr.deinit(self.allocator);
        }

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = false;

        const end = while (true) {
            while (self.skip(&.{.newline}) != null) {}
            if (self.skip(ends)) |end| break end;

            if (last_expr) |expr| {
                try lets.append(.{
                    .pattern = .ignore,
                    .expr = expr,
                    .default = null,
                });
                last_expr = null;
            }

            const result = try self.parseExpr(.both);
            const assign = switch (result) {
                .expr => false,
                .pattern => assign: {
                    errdefer result.deinit(self.allocator);
                    _ = try self.expect(&.{.assign});
                    break :assign true;
                },
                .both => self.skip(&.{.assign}) != null,
            };

            if (assign) {
                const pattern = ParseType.both.toPattern(self.allocator, result);
                errdefer pattern.deinit(self.allocator);

                const expr = try self.parseExpr(.expr);
                errdefer expr.deinit(self.allocator);

                var default: ?Expr = undefined;
                if (self.skip(&.{.@"else"}) == null) {
                    default = null;
                } else {
                    _ = try self.expect(&.{.@"return"});
                    default = try self.parseExpr(.expr);
                }
                errdefer if (default) |default_| default_.deinit(self.allocator);

                try lets.append(.{
                    .pattern = pattern,
                    .expr = expr,
                    .default = default,
                });
            } else {
                last_expr = ParseType.both.toExpr(self.allocator, result);
            }

            if (!self.peek(ends)) _ = try self.expect(&.{.newline});
        };

        var expr = last_expr orelse .null;
        last_expr = null;
        errdefer expr.deinit(self.allocator);

        while (lets.popOrNull()) |let| {
            errdefer let.deinit(self.allocator);

            const matchers = try self.allocator.alloc(Expr.Matcher, if (let.default == null) 1 else 2);
            errdefer self.allocator.free(matchers);
            matchers[0] = .{ .pattern = let.pattern, .expr = expr };
            if (let.default) |default| matchers[1] = .{ .pattern = .ignore, .expr = default };

            const match = try self.allocator.create(Expr.Match);
            match.* = .{ .subject = let.expr, .matchers = matchers };
            expr = .{ .match = match };
        }

        return .{ .expr = expr, .end = end };
    }

    fn parseExpr(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        return self.parseOr(parse_type);
    }

    fn parseOr(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseAnd(parse_type);
        if (!parse_type.isExpr(result) or self.skip(&.{.@"or"}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseOr(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        return parse_type.fromExpr(.{ .@"or" = bin });
    }

    fn parseAnd(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseNot(parse_type);
        if (!parse_type.isExpr(result) or self.skip(&.{.@"and"}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseAnd(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        return parse_type.fromExpr(.{ .@"and" = bin });
    }

    fn parseNot(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        if (parse_type == .pattern) return try self.parseCmp(parse_type);
        if (self.skip(&.{.not}) == null) return try self.parseCmp(parse_type);

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseNot(.expr);
        return parse_type.fromExpr(.{ .not = expr });
    }

    fn parseCmp(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseAdd(parse_type);
        if (!parse_type.isExpr(result)) return result;
        const op = self.skip(&.{ .eq, .ne, .lt, .le, .gt, .ge, .in }) orelse return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseAdd(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        return parse_type.fromExpr(switch (op.type) {
            inline .eq, .ne, .lt, .le, .gt, .ge, .in => |tag| @unionInit(Expr, @tagName(tag), bin),
            else => unreachable,
        });
    }

    fn parseAdd(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseMul(parse_type);
        if (!parse_type.isExpr(result) or !self.peek(&.{ .add, .sub })) return result;

        var lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        while (self.skip(&.{ .add, .sub })) |op| {
            const rhs = try self.parseMul(.expr);
            errdefer rhs.deinit(self.allocator);

            const bin = try self.allocator.create(Expr.Bin);
            bin.* = .{ .lhs = lhs, .rhs = rhs };
            lhs = switch (op.type) {
                inline .add, .sub => |tag| @unionInit(Expr, @tagName(tag), bin),
                else => unreachable,
            };
        }

        return parse_type.fromExpr(lhs);
    }

    fn parseMul(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseNeg(parse_type);
        if (!parse_type.isExpr(result) or !self.peek(&.{ .mul, .div })) return result;

        var lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        while (self.skip(&.{ .mul, .div })) |op| {
            const rhs = try self.parseNeg(.expr);
            errdefer rhs.deinit(self.allocator);

            const bin = try self.allocator.create(Expr.Bin);
            bin.* = .{ .lhs = lhs, .rhs = rhs };
            lhs = switch (op.type) {
                inline .mul, .div => |tag| @unionInit(Expr, @tagName(tag), bin),
                else => unreachable,
            };
        }

        return parse_type.fromExpr(lhs);
    }

    fn parseNeg(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        if (parse_type == .pattern) return try self.parsePow(parse_type);
        const op = self.skip(&.{ .add, .sub }) orelse return try self.parsePow(parse_type);

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseNeg(.expr);
        return parse_type.fromExpr(switch (op.type) {
            .add => .{ .pos = expr },
            .sub => .{ .neg = expr },
            else => unreachable,
        });
    }

    fn parsePow(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const result = try self.parseCall(parse_type, false);
        if (!parse_type.isExpr(result) or self.skip(&.{.pow}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseNeg(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        return parse_type.fromExpr(.{ .pow = bin });
    }

    fn parseCall(self: *Parser, comptime parse_type: ParseType, get_only: bool) Error!parse_type.Result() {
        const result = try self.parseLit(parse_type);
        if (!parse_type.isExpr(result) or !self.peek(if (get_only) &.{ .dot, .llist } else &.{ .lpar, .pipe, .dot, .llist })) return result;

        var lhs = parse_type.toExpr(self.allocator, result);

        while (true) {
            if (!get_only and self.peek(&.{.lpar})) {
                errdefer lhs.deinit(self.allocator);
                const rhs = try self.parseCol(.list, .expr, .lpar, .rpar);
                errdefer rhs.deinit(self.allocator);

                const bin = try self.allocator.create(Expr.Bin);
                bin.* = .{ .lhs = lhs, .rhs = rhs };
                lhs = .{ .call = bin };
            } else if (!get_only and self.skip(&.{.pipe}) != null) {
                var owner = true;
                errdefer if (owner) lhs.deinit(self.allocator);

                const bin = try self.allocator.create(Expr.Bin);
                errdefer self.allocator.destroy(bin);

                bin.lhs = try self.parseCall(.expr, true);
                errdefer bin.lhs.deinit(self.allocator);

                _ = try self.expect(&.{.lpar});
                const old_skip = self.skip_newlines;
                defer self.skip_newlines = old_skip;
                self.skip_newlines = true;
                var state = ColState(.list, .expr).init(self.allocator);
                defer state.deinit();
                try state.append(false, undefined, lhs);
                owner = false;
                bin.rhs = try self.parseColTail(.list, .expr, &state, .rpar);

                lhs = .{ .call = bin };
            } else if (self.skip(&.{.dot}) != null) {
                errdefer lhs.deinit(self.allocator);
                const name = try self.expect(&.{.name});
                const rhs = Expr{ .str = try self.allocator.dupe(u8, name.content) };
                errdefer rhs.deinit(self.allocator);

                const bin = try self.allocator.create(Expr.Bin);
                bin.* = .{ .lhs = lhs, .rhs = rhs };
                lhs = .{ .get = bin };
            } else if (self.skip(&.{.llist}) != null) {
                const old_skip = self.skip_newlines;
                defer self.skip_newlines = old_skip;
                self.skip_newlines = true;

                errdefer lhs.deinit(self.allocator);
                const rhs = try self.parseExpr(.expr);
                errdefer rhs.deinit(self.allocator);
                _ = try self.expect(&.{.rlist});

                const bin = try self.allocator.create(Expr.Bin);
                bin.* = .{ .lhs = lhs, .rhs = rhs };
                lhs = .{ .get = bin };
            } else {
                break;
            }
        }

        return parse_type.fromExpr(lhs);
    }

    fn parseLit(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        if (self.peek(&.{.name})) {
            return try self.parseName(parse_type);
        } else if (self.peek(&.{.num})) {
            return try parse_type.fromLit(self.allocator, try self.parseNum());
        } else if (self.peek(&.{.bool})) {
            return try parse_type.fromLit(self.allocator, try self.parseBool());
        } else if (self.peek(&.{.null})) {
            return try parse_type.fromLit(self.allocator, try self.parseNull());
        } else if (self.peek(&.{.str})) {
            return try parse_type.fromLit(self.allocator, try self.parseStr());
        } else if (parse_type != .expr and self.peek(&.{.ignore})) {
            return parse_type.fromPattern(try self.parseIgnore());
        } else if (self.peek(&.{.llist})) {
            return try self.parseCol(.list, parse_type, .llist, .rlist);
        } else if (self.peek(&.{.ldict})) {
            return try self.parseCol(.dict, parse_type, .ldict, .rdict);
        } else if (parse_type != .pattern and self.peek(&.{.lambda})) {
            return parse_type.fromExpr(try self.parseLambda());
        } else if (self.peek(&.{.lpar})) {
            return try parse_type.fromLit(self.allocator, try self.parsePar());
        } else if (parse_type != .pattern and self.peek(&.{.do})) {
            return parse_type.fromExpr(try self.parseDo());
        } else if (parse_type != .pattern and self.peek(&.{.match})) {
            return parse_type.fromExpr(try self.parseMatch());
        } else if (parse_type != .pattern and self.peek(&.{.@"if"})) {
            return parse_type.fromExpr(try self.parseIf());
        } else if (parse_type != .pattern and self.peek(&.{.@"for"})) {
            return parse_type.fromExpr(try self.parseFor());
        } else if (parse_type != .pattern and self.is_gen and self.peek(&.{.yield})) {
            return parse_type.fromExpr(try self.parseYield());
        } else if (parse_type != .pattern and self.peek(&.{.@"return"})) {
            return parse_type.fromExpr(try self.parseReturn());
        } else {
            _ = try self.expect(&.{});
            unreachable;
        }
    }

    fn parseName(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const token = try self.expect(&.{.name});
        return switch (parse_type) {
            .expr, .pattern => .{ .name = token.content },
            .both => .{ .both = .{
                .expr = .{ .name = token.content },
                .pattern = .{ .name = token.content },
            } },
        };
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

    fn parseStr(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.str});

        var exprs = std.ArrayList(Expr).init(self.allocator);
        defer {
            for (exprs.items) |expr| expr.deinit(self.allocator);
            exprs.deinit();
        }

        while (true) {
            const chunk = try self.lexer.strChunk(self.allocator);

            if (!chunk.expr and exprs.items.len == 0) {
                return .{ .str = chunk.content };
            }

            if (chunk.content.len > 0) {
                errdefer self.allocator.free(chunk.content);
                try exprs.append(.{ .str = chunk.content });
            } else {
                self.allocator.free(chunk.content);
            }

            if (!chunk.expr) break;

            const old_skip = self.skip_newlines;
            defer self.skip_newlines = old_skip;
            self.skip_newlines = true;

            const expr = try self.parseExpr(.expr);
            errdefer expr.deinit(self.allocator);
            _ = try self.expect(&.{.rdict});
            try exprs.append(expr);
        }

        const bin = try self.allocator.create(Expr.Bin);
        errdefer self.allocator.destroy(bin);
        bin.* = .{
            .lhs = .{ .global = .str },
            .rhs = .{ .list = try exprs.toOwnedSlice() },
        };
        return .{ .call = bin };
    }

    fn parseIgnore(self: *Parser) Error!Pattern {
        _ = try self.expect(&.{.ignore});
        return .ignore;
    }

    fn parseCol(
        self: *Parser,
        comptime col_type: ColType,
        comptime parse_type: ParseType,
        start: Token.Type,
        end: Token.Type,
    ) Error!parse_type.Result() {
        _ = try self.expect(&.{start});

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        var state = ColState(col_type, parse_type).init(self.allocator);
        defer state.deinit();

        return try self.parseColTail(col_type, parse_type, &state, end);
    }

    fn parseColTail(
        self: *Parser,
        comptime col_type: ColType,
        comptime parse_type: ParseType,
        state: *ColState(col_type, parse_type),
        end: Token.Type,
    ) Error!parse_type.Result() {
        while (self.skip(&.{end}) == null) {
            const is_col = self.skip(&.{.mul}) != null;
            var owner = true;

            var key: Expr = undefined;
            if (col_type == .dict and !is_col) {
                key = try self.parseExpr(.expr);
                errdefer key.deinit(self.allocator);
                _ = try self.expect(&.{.assign});
            }
            errdefer if (col_type == .dict and !is_col and owner) key.deinit(self.allocator);

            const item = try self.parseExpr(parse_type);
            errdefer if (owner) item.deinit(self.allocator);

            if (!self.peek(&.{end})) _ = try self.expect(&.{.comma});

            switch (parse_type) {
                .expr, .pattern => {
                    try state.append(is_col, key, item);
                },
                .both => switch (item) {
                    .expr => |expr| {
                        var expr_state = try state.toExpr();
                        defer expr_state.deinit();
                        try expr_state.append(is_col, key, expr);
                        owner = false;
                        return parse_type.fromExpr(try self.parseColTail(col_type, .expr, &expr_state, end));
                    },
                    .pattern => |pattern| {
                        var pattern_state = try state.toPattern();
                        defer pattern_state.deinit();
                        try pattern_state.append(is_col, key, pattern);
                        owner = false;
                        return parse_type.fromPattern(try self.parseColTail(col_type, .pattern, &pattern_state, end));
                    },
                    .both => |both| {
                        try state.append(is_col, key, both);
                    },
                },
            }
        }
        return try state.toResult();
    }

    fn parseLambda(self: *Parser) Error!Expr {
        const old_is_gen = self.is_gen;
        defer self.is_gen = old_is_gen;
        self.is_gen = false;

        const matcher = try self.allocator.create(Expr.Matcher);
        errdefer self.allocator.destroy(matcher);
        matcher.pattern = try self.parseCol(.list, .pattern, .lambda, .lambda);
        errdefer matcher.pattern.deinit(self.allocator);
        matcher.expr = try self.parseExpr(.expr);
        return .{ .lambda = matcher };
    }

    fn parsePar(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.lpar});

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        const expr = try self.parseExpr(.expr);
        errdefer expr.deinit(self.allocator);

        _ = try self.expect(&.{.rpar});

        return expr;
    }

    fn parseDo(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.do});
        const is_gen = self.skip(&.{.mul}) != null;

        const old_is_gen = self.is_gen;
        defer self.is_gen = old_is_gen;
        if (is_gen) self.is_gen = true;

        const body = try self.parseBody(&.{.end});
        if (!is_gen) return body.expr;

        errdefer body.expr.deinit(self.allocator);
        const expr_ptr = try self.allocator.create(Expr);
        expr_ptr.* = body.expr;
        return .{ .gen = expr_ptr };
    }

    fn parseMatch(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.match});

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        const subject = try self.parseExpr(.expr);
        errdefer subject.deinit(self.allocator);

        _ = try self.expect(&.{.do});
        self.skip_newlines = false;

        var matchers = std.ArrayList(Expr.Matcher).init(self.allocator);
        defer {
            for (matchers.items) |matcher| matcher.deinit(self.allocator);
            matchers.deinit();
        }

        while (true) {
            while (self.skip(&.{.newline}) != null) {}
            if (self.skip(&.{.end}) != null) break;

            const pattern = try self.parseExpr(.pattern);
            errdefer pattern.deinit(self.allocator);

            _ = try self.expect(&.{.assign});

            const expr = try self.parseExpr(.expr);
            errdefer expr.deinit(self.allocator);

            if (!self.peek(&.{.end})) _ = try self.expect(&.{.newline});

            try matchers.append(.{ .pattern = pattern, .expr = expr });
        }

        const match = try self.allocator.create(Expr.Match);
        errdefer self.allocator.destroy(match);
        match.* = .{ .subject = subject, .matchers = try matchers.toOwnedSlice() };
        return .{ .match = match };
    }

    fn parseIf(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.@"if"});

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        var branches = std.ArrayList(IfBranch).init(self.allocator);
        defer {
            for (branches.items) |branch| branch.deinit(self.allocator);
            branches.deinit();
        }

        const has_else = while (true) {
            const result = try self.parseExpr(.both);
            const assign = switch (result) {
                .expr => false,
                .pattern => assign: {
                    errdefer result.deinit(self.allocator);
                    _ = try self.expect(&.{.assign});
                    break :assign true;
                },
                .both => self.skip(&.{.assign}) != null,
            };

            var cond: IfCond = undefined;
            if (assign) {
                const pattern = ParseType.both.toPattern(self.allocator, result);
                errdefer pattern.deinit(self.allocator);
                const expr = try self.parseExpr(.expr);
                cond = .{ .match = .{ .pattern = pattern, .expr = expr } };
            } else {
                cond = .{ .@"if" = ParseType.both.toExpr(self.allocator, result) };
            }
            errdefer cond.deinit(self.allocator);

            _ = try self.expect(&.{.do});
            const body = try self.parseBody(&.{ .elif, .@"else", .end });
            errdefer body.expr.deinit(self.allocator);

            try branches.append(.{ .cond = cond, .then = body.expr });

            switch (body.end.type) {
                .elif => {},
                .@"else" => break true,
                .end => break false,
                else => unreachable,
            }
        };

        var expr: Expr = undefined;
        if (has_else) {
            const body = try self.parseBody(&.{.end});
            expr = body.expr;
        } else {
            expr = .null;
        }
        errdefer expr.deinit(self.allocator);

        while (branches.popOrNull()) |branch| {
            errdefer branch.deinit(self.allocator);
            switch (branch.cond) {
                .@"if" => |cond| {
                    const if_ = try self.allocator.create(Expr.If);
                    if_.* = .{ .cond = cond, .then = branch.then, .else_ = expr };
                    expr = .{ .@"if" = if_ };
                },
                .match => |cond| {
                    const matchers = try self.allocator.alloc(Expr.Matcher, 2);
                    errdefer self.allocator.free(matchers);
                    matchers[0] = .{ .pattern = cond.pattern, .expr = branch.then };
                    matchers[1] = .{ .pattern = .ignore, .expr = expr };

                    const match = try self.allocator.create(Expr.Match);
                    match.* = .{ .subject = cond.expr, .matchers = matchers };
                    expr = .{ .match = match };
                },
            }
        }

        return expr;
    }

    fn parseFor(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.@"for"});

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        const pattern = try self.parseExpr(.pattern);
        errdefer pattern.deinit(self.allocator);

        _ = try self.expect(&.{.in});

        const subject = try self.parseExpr(.expr);
        errdefer subject.deinit(self.allocator);

        _ = try self.expect(&.{.do});

        const body = try self.parseBody(&.{.end});
        errdefer body.expr.deinit(self.allocator);

        const for_ = try self.allocator.create(Expr.For);
        for_.* = .{
            .subject = subject,
            .matcher = .{ .pattern = pattern, .expr = body.expr },
        };
        return .{ .@"for" = for_ };
    }

    fn parseYield(self: *Parser) Error!Expr {
        std.debug.assert(self.is_gen);
        _ = try self.expect(&.{.yield});
        const is_all = self.skip(&.{.mul}) != null;

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpr(.expr);

        return if (is_all) .{ .yield_all = expr } else .{ .yield = expr };
    }

    fn parseReturn(self: *Parser) Error!Expr {
        _ = try self.expect(&.{.@"return"});

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpr(.expr);

        return .{ .@"return" = expr };
    }

    fn peek(self: *Parser, token_types: []const Token.Type) bool {
        const token = while (true) {
            if (self.token) |token| {
                switch (token.type) {
                    .space, .comment => {},
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

const ParseType = enum {
    expr,
    pattern,
    both,

    fn Result(comptime self: ParseType) type {
        return switch (self) {
            .expr => Expr,
            .pattern => Pattern,
            .both => ParseResult,
        };
    }

    inline fn isExpr(comptime self: ParseType, result: self.Result()) bool {
        return switch (self) {
            .expr => true,
            .pattern => false,
            .both => result != .pattern,
        };
    }

    inline fn toExpr(comptime self: ParseType, allocator: std.mem.Allocator, result: self.Result()) Expr {
        return switch (self) {
            .expr => result,
            .pattern => unreachable,
            .both => switch (result) {
                .expr => |expr| expr,
                .pattern => unreachable,
                .both => |both| expr: {
                    both.pattern.deinit(allocator);
                    break :expr both.expr;
                },
            },
        };
    }

    inline fn fromExpr(comptime self: ParseType, expr: Expr) self.Result() {
        return switch (self) {
            .expr => expr,
            .pattern => unreachable,
            .both => .{ .expr = expr },
        };
    }

    inline fn isPattern(comptime self: ParseType, result: self.Result()) bool {
        return switch (self) {
            .expr => false,
            .pattern => true,
            .both => result != .expr,
        };
    }

    inline fn toPattern(comptime self: ParseType, allocator: std.mem.Allocator, result: self.Result()) Pattern {
        return switch (self) {
            .expr => unreachable,
            .pattern => result,
            .both => switch (result) {
                .expr => unreachable,
                .pattern => |pattern| pattern,
                .both => |both| pattern: {
                    both.expr.deinit(allocator);
                    break :pattern both.pattern;
                },
            },
        };
    }

    inline fn fromPattern(comptime self: ParseType, pattern: Pattern) self.Result() {
        return switch (self) {
            .expr => unreachable,
            .pattern => pattern,
            .both => .{ .pattern = pattern },
        };
    }

    inline fn fromLit(comptime self: ParseType, allocator: std.mem.Allocator, expr: Expr) !self.Result() {
        switch (self) {
            .expr => return expr,
            .pattern => {
                errdefer expr.deinit(allocator);
                const expr_ptr = try allocator.create(Expr);
                expr_ptr.* = expr;
                return .{ .expr = expr_ptr };
            },
            .both => {
                errdefer expr.deinit(allocator);
                const expr_ptr = try allocator.create(Expr);
                errdefer allocator.destroy(expr_ptr);
                expr_ptr.* = try expr.clone(allocator);
                return .{ .both = .{
                    .expr = expr,
                    .pattern = .{ .expr = expr_ptr },
                } };
            },
        }
    }
};

const ParseResult = union(ParseType) {
    expr: Expr,
    pattern: Pattern,
    both: Both,

    fn deinit(self: ParseResult, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |value| value.deinit(allocator),
        }
    }
};

const Both = struct {
    expr: Expr,
    pattern: Pattern,

    fn deinit(self: Both, allocator: std.mem.Allocator) void {
        self.expr.deinit(allocator);
        self.pattern.deinit(allocator);
    }

    const Pair = struct {
        key: Expr,
        value: Both,

        fn deinit(self: Pair, allocator: std.mem.Allocator) void {
            self.key.deinit(allocator);
            self.value.deinit(allocator);
        }
    };
};

const ColType = enum { list, dict };

fn ColState(comptime col_type: ColType, comptime parse_type: ParseType) type {
    return struct {
        const Col = switch (parse_type) {
            .expr => Expr,
            .pattern => Pattern,
            .both => Both,
        };
        const Item = switch (col_type) {
            .list => Col,
            .dict => Col.Pair,
        };

        items: std.ArrayList(Item),
        cols: std.ArrayList(Col),

        const Self = @This();

        fn init(allocator: std.mem.Allocator) Self {
            return .{
                .items = std.ArrayList(Item).init(allocator),
                .cols = std.ArrayList(Col).init(allocator),
            };
        }

        fn deinit(self: *Self) void {
            for (self.items.items) |item| item.deinit(self.items.allocator);
            self.items.deinit();
            for (self.cols.items) |col| col.deinit(self.items.allocator);
            self.cols.deinit();
        }

        inline fn append(self: *Self, is_col: bool, key: Expr, value: Col) !void {
            if (is_col) {
                try self.appendCol(value);
            } else if (col_type == .dict) {
                try self.appendItem(.{ .key = key, .value = value });
            } else {
                try self.appendItem(value);
            }
        }

        fn appendItem(self: *Self, item: Item) !void {
            try self.items.append(item);
        }

        fn appendCol(self: *Self, col: Col) !void {
            try self.itemsToCol();
            try self.cols.append(col);
        }

        fn toExpr(self: *Self) !ColState(col_type, .expr) {
            if (parse_type != .both) unreachable;

            var expr_state = ColState(col_type, .expr).init(self.items.allocator);
            errdefer expr_state.deinit();

            try expr_state.items.ensureUnusedCapacity(self.items.items.len);
            for (self.items.items) |both| {
                if (col_type == .dict) {
                    both.value.pattern.deinit(self.items.allocator);
                    expr_state.items.appendAssumeCapacity(.{ .key = both.key, .value = both.value.expr });
                } else {
                    both.pattern.deinit(self.items.allocator);
                    expr_state.items.appendAssumeCapacity(both.expr);
                }
            }
            self.items.clearAndFree();

            try expr_state.cols.ensureUnusedCapacity(self.cols.items.len);
            for (self.cols.items) |both| {
                both.pattern.deinit(self.items.allocator);
                expr_state.cols.appendAssumeCapacity(both.expr);
            }
            self.cols.clearAndFree();

            return expr_state;
        }

        fn toPattern(self: *Self) !ColState(col_type, .pattern) {
            if (parse_type != .both) unreachable;

            var pattern_state = ColState(col_type, .pattern).init(self.items.allocator);
            errdefer pattern_state.deinit();

            try pattern_state.items.ensureUnusedCapacity(self.items.items.len);
            for (self.items.items) |both| {
                if (col_type == .dict) {
                    both.value.expr.deinit(self.items.allocator);
                    pattern_state.items.appendAssumeCapacity(.{ .key = both.key, .value = both.value.pattern });
                } else {
                    both.expr.deinit(self.items.allocator);
                    pattern_state.items.appendAssumeCapacity(both.pattern);
                }
            }
            self.items.clearAndFree();

            try pattern_state.cols.ensureUnusedCapacity(self.cols.items.len);
            for (self.cols.items) |both| {
                both.expr.deinit(self.items.allocator);
                pattern_state.cols.appendAssumeCapacity(both.pattern);
            }
            self.cols.clearAndFree();

            return pattern_state;
        }

        fn aggregate(self: *Self, comptime field: []const u8, comptime tag: []const u8) !Col {
            switch (parse_type) {
                .expr, .pattern => {
                    return @unionInit(Col, tag, try @field(self, field).toOwnedSlice());
                },
                .both => {
                    const allocator = self.items.allocator;
                    const is_pair = comptime col_type == .dict and std.mem.eql(u8, field, "items");

                    const exprs = try allocator.alloc(if (is_pair) Expr.Pair else Expr, @field(self, field).items.len);
                    errdefer allocator.free(exprs);
                    const patterns = try allocator.alloc(if (is_pair) Pattern.Pair else Pattern, @field(self, field).items.len);

                    var i: usize = 0;
                    errdefer if (is_pair) {
                        for (patterns[0..i]) |pattern| pattern.key.deinit(allocator);
                    };
                    while (i < @field(self, field).items.len) : (i += 1) {
                        const both = @field(self, field).items[i];
                        if (is_pair) {
                            exprs[i] = .{ .key = both.key, .value = both.value.expr };
                            patterns[i] = .{ .key = try both.key.clone(allocator), .value = both.value.pattern };
                        } else {
                            exprs[i] = both.expr;
                            patterns[i] = both.pattern;
                        }
                    }
                    @field(self, field).clearAndFree();

                    return .{
                        .expr = @unionInit(Expr, tag, exprs),
                        .pattern = @unionInit(Pattern, tag, patterns),
                    };
                },
            }
        }

        fn itemsToCol(self: *Self) !void {
            if (self.items.items.len == 0) return;
            const col = try self.aggregate("items", @tagName(col_type));
            errdefer col.deinit(self.items.allocator);
            try self.cols.append(col);
        }

        fn toResult(self: *Self) !parse_type.Result() {
            var result: Col = undefined;
            if (self.cols.items.len == 0) {
                result = try self.aggregate("items", @tagName(col_type));
            } else {
                try self.itemsToCol();
                result = try self.aggregate("cols", @tagName(col_type) ++ "s");
            }
            return if (parse_type == .both) .{ .both = result } else result;
        }
    };
}

const Let = struct {
    pattern: Pattern,
    expr: Expr,
    default: ?Expr,

    fn deinit(self: Let, allocator: std.mem.Allocator) void {
        self.pattern.deinit(allocator);
        self.expr.deinit(allocator);
        if (self.default) |expr| expr.deinit(allocator);
    }
};

const IfCond = union(enum) {
    @"if": Expr,
    match: Expr.Matcher,

    fn deinit(self: IfCond, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |value| value.deinit(allocator),
        }
    }
};

const IfBranch = struct {
    cond: IfCond,
    then: Expr,

    fn deinit(self: IfBranch, allocator: std.mem.Allocator) void {
        self.cond.deinit(allocator);
        self.then.deinit(allocator);
    }
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

test "parse operators" {
    const expr = try parse(std.testing.allocator, "1 * 2 + 3 / -4");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .add = &.{
        .lhs = .{ .mul = &.{
            .lhs = .{ .num = 1 },
            .rhs = .{ .num = 2 },
        } },
        .rhs = .{ .div = &.{
            .lhs = .{ .num = 3 },
            .rhs = .{ .neg = &.{ .num = 4 } },
        } },
    } }, expr);
}

test "parse var" {
    const expr = try parse(std.testing.allocator, (
        \\x = 4
        \\x * x
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .match = &.{
        .subject = .{ .num = 4 },
        .matchers = &.{.{
            .pattern = .{ .name = "x" },
            .expr = .{ .mul = &.{
                .lhs = .{ .name = "x" },
                .rhs = .{ .name = "x" },
            } },
        }},
    } }, expr);
}

test "parse lambda" {
    const expr = try parse(std.testing.allocator, (
        \\sqr = |x| x * x
        \\sqr(4)
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .match = &.{
        .subject = .{ .lambda = &.{
            .pattern = .{ .list = &.{
                .{ .name = "x" },
            } },
            .expr = .{ .mul = &.{
                .lhs = .{ .name = "x" },
                .rhs = .{ .name = "x" },
            } },
        } },
        .matchers = &.{.{
            .pattern = .{ .name = "sqr" },
            .expr = .{ .call = &.{
                .lhs = .{ .name = "sqr" },
                .rhs = .{ .list = &.{
                    .{ .num = 4 },
                } },
            } },
        }},
    } }, expr);
}

test "parse closure" {
    const expr = try parse(std.testing.allocator, (
        \\n = 2
        \\times_n = |x| x * n
        \\times_n(4)
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .match = &.{
        .subject = .{ .num = 2 },
        .matchers = &.{.{
            .pattern = .{ .name = "n" },
            .expr = .{ .match = &.{
                .subject = .{ .lambda = &.{
                    .pattern = .{ .list = &.{
                        .{ .name = "x" },
                    } },
                    .expr = .{ .mul = &.{
                        .lhs = .{ .name = "x" },
                        .rhs = .{ .name = "n" },
                    } },
                } },
                .matchers = &.{.{
                    .pattern = .{ .name = "times_n" },
                    .expr = .{ .call = &.{
                        .lhs = .{ .name = "times_n" },
                        .rhs = .{ .list = &.{
                            .{ .num = 4 },
                        } },
                    } },
                }},
            } },
        }},
    } }, expr);
}

test "parse dict" {
    const expr = try parse(std.testing.allocator, (
        \\{"foo" = foo, *_} = {"foo" = 1, "bar" = 2, "baz" = 3}
        \\foo
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{ .match = &.{
        .subject = .{ .dict = &.{
            .{ .key = .{ .str = "foo" }, .value = .{ .num = 1 } },
            .{ .key = .{ .str = "bar" }, .value = .{ .num = 2 } },
            .{ .key = .{ .str = "baz" }, .value = .{ .num = 3 } },
        } },
        .matchers = &.{.{
            .pattern = .{ .dicts = &.{
                .{ .dict = &.{
                    .{ .key = .{ .str = "foo" }, .value = .{ .name = "foo" } },
                } },
                .ignore,
            } },
            .expr = .{ .name = "foo" },
        }},
    } }, expr);
}
