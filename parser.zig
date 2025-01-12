const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const Expr = @import("expr.zig").Expr;
const Pattern = @import("pattern.zig").Pattern;
const Instr = @import("instr.zig").Instr;

pub fn parse(allocator: std.mem.Allocator, content: []const u8) Parser.Error!Expr {
    var parser = Parser.init(allocator, content);
    return try parser.parseModule();
}

pub fn internal_parse(allocator: std.mem.Allocator, content: []const u8) Parser.Error!Expr {
    var parser = Parser.init(allocator, content);
    parser.lexer.internal = true;
    const body = try parser.parseBody(&.{.eof});
    return body.expr;
}

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,

    token: ?Token = null,
    expected: std.EnumSet(Token.Type) = undefined,
    last_index: usize = 0,
    skip_newlines: bool = false,
    is_gen: bool = false,

    error_reason: enum { unexpected_token, invalid_float, duplicate_fn_name } = undefined,

    pub const Error = std.mem.Allocator.Error || error{ParseError};

    pub fn init(allocator: std.mem.Allocator, content: []const u8) Parser {
        return .{
            .allocator = allocator,
            .lexer = Lexer.init(content),
        };
    }

    pub fn parseModule(self: *Parser) Error!Expr {
        const start = self.getStart();
        var fn_names = std.StringHashMap(void).init(self.allocator);
        defer fn_names.deinit();

        var stmts = std.ArrayList(Expr.Stmt).init(self.allocator);
        defer {
            for (stmts.items) |stmt| stmt.deinit(self.allocator);
            stmts.deinit();
        }

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = false;

        while (true) {
            while (self.skip(&.{.newline}) != null) {}
            if (self.skip(&.{.eof}) != null) break;

            var stmt: Expr.Stmt = undefined;
            stmt.is_pub = self.skip(&.{.@"pub"}) != null;

            if (self.skip(&.{.@"fn"}) != null) {
                const token = try self.expect(&.{.name});
                const result = try fn_names.getOrPut(token.content);

                if (result.found_existing) {
                    self.token = token;
                    self.error_reason = .duplicate_fn_name;
                    return error.ParseError;
                }

                stmt.fn_name = token.content;
                stmt.pattern = try self.parseCol(.list, .pattern, .lpar, .rpar);
                errdefer stmt.pattern.deinit(self.allocator);
                stmt.subject = try self.parseExpr(.expr);
            } else if (stmt.is_pub) {
                stmt.fn_name = null;
                stmt.pattern = try self.parseGuard(.pattern);
                errdefer stmt.pattern.deinit(self.allocator);
                _ = try self.expect(&.{.assign});
                stmt.subject = try self.parseExpr(.expr);
            } else {
                stmt.fn_name = null;
                const result = try self.parseGuard(.both);
                const assign = switch (result.data) {
                    .expr => false,
                    .pattern => assign: {
                        errdefer result.deinit(self.allocator);
                        _ = try self.expect(&.{.assign});
                        break :assign true;
                    },
                    .both => self.skip(&.{.assign}) != null,
                };
                if (assign) {
                    stmt.pattern = ParseType.both.toPattern(self.allocator, result);
                    errdefer stmt.pattern.deinit(self.allocator);
                    stmt.subject = try self.parseExpr(.expr);
                } else {
                    stmt.pattern = .{ .data = .ignore, .location = .{} };
                    stmt.subject = ParseType.both.toExpr(self.allocator, result);
                }
            }

            errdefer stmt.deinit(self.allocator);
            if (!self.peek(&.{.eof})) _ = try self.expect(&.{.newline});
            try stmts.append(stmt);
        }

        const location = self.getLocation(start);
        return .{
            .data = .{ .module = try stmts.toOwnedSlice() },
            .location = location,
        };
    }

    fn parseBody(self: *Parser, ends: []const Token.Type) Error!Body {
        var lets = std.ArrayList(Let).init(self.allocator);
        var last_expr: ?Expr = null;
        var last_start: usize = undefined;
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
                    .pattern = .{ .data = .ignore, .location = .{} },
                    .expr = expr,
                    .default = null,
                    .start = last_start,
                });
                last_expr = null;
            }

            const start = self.getStart();
            const result = try self.parseGuard(.both);

            const assign = switch (result.data) {
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
                    .start = start,
                });
            } else {
                last_expr = ParseType.both.toExpr(self.allocator, result);
                last_start = start;
            }

            if (!self.peek(ends)) _ = try self.expect(&.{.newline});
        };

        var expr = last_expr orelse Expr{ .data = .null, .location = .{} };
        last_expr = null;
        errdefer expr.deinit(self.allocator);

        while (lets.popOrNull()) |let| {
            errdefer let.deinit(self.allocator);

            const matchers = try self.allocator.alloc(Expr.Matcher, if (let.default == null) 1 else 2);
            errdefer self.allocator.free(matchers);
            matchers[0] = .{ .pattern = let.pattern, .expr = expr };
            if (let.default) |default| matchers[1] = .{
                .pattern = .{ .data = .ignore, .location = .{} },
                .expr = default,
            };

            const match = try self.allocator.create(Expr.Match);
            match.* = .{ .subject = let.expr, .matchers = matchers };
            const location = self.getLocation(let.start);
            expr = .{ .data = .{ .match = match }, .location = location };
        }

        return .{ .expr = expr, .end = end };
    }

    fn parseGuard(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseExpr(parse_type);
        if (!parse_type.isPattern(result) or self.skip(&.{.@"if"}) == null) return result;

        const pattern = parse_type.toPattern(self.allocator, result);
        errdefer pattern.deinit(self.allocator);

        const cond = try self.parseExpr(.expr);
        errdefer cond.deinit(self.allocator);

        const guard = try self.allocator.create(Pattern.Guard);
        guard.* = .{ .pattern = pattern, .cond = cond };
        const location = self.getLocation(start);
        return parse_type.fromPattern(.{
            .data = .{ .guard = guard },
            .location = location,
        });
    }

    fn parseExpr(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        return self.parseOr(parse_type);
    }

    fn parseOr(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseAnd(parse_type);
        if (!parse_type.isExpr(result) or self.skip(&.{.@"or"}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseOr(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{ .data = .{ .@"or" = bin }, .location = location });
    }

    fn parseAnd(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseNot(parse_type);
        if (!parse_type.isExpr(result) or self.skip(&.{.@"and"}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseAnd(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{ .data = .{ .@"and" = bin }, .location = location });
    }

    fn parseNot(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        if (parse_type == .pattern) return try self.parseCmp(parse_type);
        if (self.skip(&.{.not}) == null) return try self.parseCmp(parse_type);

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseNot(.expr);
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{ .data = .{ .not = expr }, .location = location });
    }

    fn parseCmp(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseAdd(parse_type);
        if (!parse_type.isExpr(result)) return result;
        const token = self.skip(&.{ .eq, .ne, .lt, .le, .gt, .ge, .in }) orelse return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        var last_op = switch (token.type) {
            inline .eq, .ne, .lt, .le, .gt, .ge, .in => |tag| @field(Expr.Cmp.Op, @tagName(tag)),
            else => unreachable,
        };
        var last_rhs = try self.parseAdd(.expr);
        errdefer last_rhs.deinit(self.allocator);

        var cmps = std.ArrayList(Expr.Cmp).init(self.allocator);
        defer {
            for (cmps.items) |op| op.deinit(self.allocator);
            cmps.deinit();
        }
        while (self.skip(&.{ .eq, .ne, .lt, .le, .gt, .ge, .in })) |token_| {
            const op = switch (token_.type) {
                inline .eq, .ne, .lt, .le, .gt, .ge, .in => |tag| @field(Expr.Cmp.Op, @tagName(tag)),
                else => unreachable,
            };
            const rhs = try self.parseAdd(.expr);
            errdefer rhs.deinit(self.allocator);

            try cmps.append(.{ .op = last_op, .rhs = last_rhs });
            last_op = op;
            last_rhs = rhs;
        }

        const cmp_root = try self.allocator.create(Expr.CmpRoot);
        errdefer self.allocator.destroy(cmp_root);
        cmp_root.* = .{
            .lhs = lhs,
            .cmps = try cmps.toOwnedSlice(),
            .last_op = last_op,
            .last_rhs = last_rhs,
        };
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{ .data = .{ .cmp = cmp_root }, .location = location });
    }

    fn parseAdd(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseMul(parse_type);
        if (!parse_type.isExpr(result) or !self.peek(&.{ .add, .sub })) return result;

        var lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        while (self.skip(&.{ .add, .sub })) |op| {
            const rhs = try self.parseMul(.expr);
            errdefer rhs.deinit(self.allocator);

            const bin = try self.allocator.create(Expr.Bin);
            bin.* = .{ .lhs = lhs, .rhs = rhs };
            const location = self.getLocation(start);
            lhs = .{
                .data = switch (op.type) {
                    inline .add, .sub => |tag| @unionInit(Expr.Data, @tagName(tag), bin),
                    else => unreachable,
                },
                .location = location,
            };
        }

        return parse_type.fromExpr(lhs);
    }

    fn parseMul(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseNeg(parse_type);
        if (!parse_type.isExpr(result) or !self.peek(&.{ .mul, .div })) return result;

        var lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        while (self.skip(&.{ .mul, .div })) |op| {
            const rhs = try self.parseNeg(.expr);
            errdefer rhs.deinit(self.allocator);

            const bin = try self.allocator.create(Expr.Bin);
            bin.* = .{ .lhs = lhs, .rhs = rhs };
            const location = self.getLocation(start);
            lhs = .{
                .data = switch (op.type) {
                    inline .mul, .div => |tag| @unionInit(Expr.Data, @tagName(tag), bin),
                    else => unreachable,
                },
                .location = location,
            };
        }

        return parse_type.fromExpr(lhs);
    }

    fn parseNeg(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        if (parse_type == .pattern) return try self.parsePow(parse_type);
        const op = self.skip(&.{ .add, .sub }) orelse return try self.parsePow(parse_type);

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseNeg(.expr);
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{
            .data = switch (op.type) {
                .add => .{ .pos = expr },
                .sub => .{ .neg = expr },
                else => unreachable,
            },
            .location = location,
        });
    }

    fn parsePow(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const result = try self.parseCall(parse_type, false);
        if (!parse_type.isExpr(result) or self.skip(&.{.pow}) == null) return result;

        const lhs = parse_type.toExpr(self.allocator, result);
        errdefer lhs.deinit(self.allocator);

        const rhs = try self.parseNeg(.expr);
        errdefer rhs.deinit(self.allocator);

        const bin = try self.allocator.create(Expr.Bin);
        bin.* = .{ .lhs = lhs, .rhs = rhs };
        const location = self.getLocation(start);
        return parse_type.fromExpr(.{ .data = .{ .pow = bin }, .location = location });
    }

    fn parseCall(self: *Parser, comptime parse_type: ParseType, get_only: bool) Error!parse_type.Result() {
        const start = self.getStart();
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
                const location = self.getLocation(start);
                lhs = .{ .data = .{ .call = bin }, .location = location };
            } else if (!get_only and self.skip(&.{.pipe}) != null) {
                var owner = true;
                errdefer if (owner) lhs.deinit(self.allocator);

                const bin = try self.allocator.create(Expr.Bin);
                errdefer self.allocator.destroy(bin);

                bin.lhs = try self.parseCall(.expr, true);
                errdefer bin.lhs.deinit(self.allocator);

                const col_start = self.getStart();
                _ = try self.expect(&.{.lpar});
                const items_start = self.getStart();

                const old_skip = self.skip_newlines;
                defer self.skip_newlines = old_skip;
                self.skip_newlines = true;

                var state = ColState(.list, .expr).init(self.allocator);
                defer state.deinit();
                try state.append(null, undefined, lhs);
                owner = false;
                bin.rhs = try self.parseColTail(.list, .expr, &state, col_start, items_start, .rpar);

                const location = self.getLocation(start);
                lhs = .{ .data = .{ .call = bin }, .location = location };
            } else if (self.skip(&.{.dot}) != null) {
                errdefer lhs.deinit(self.allocator);
                const key_start = self.getStart();
                const name = try self.expect(&.{.name});
                const key_location = self.getLocation(key_start);
                const rhs = Expr{
                    .data = .{ .str = try self.allocator.dupe(u8, name.content) },
                    .location = key_location,
                };
                errdefer rhs.deinit(self.allocator);

                const bin = try self.allocator.create(Expr.Bin);
                bin.* = .{ .lhs = lhs, .rhs = rhs };
                const location = self.getLocation(start);
                lhs = .{ .data = .{ .get = bin }, .location = location };
            } else if (self.skip(&.{.llist}) != null) {
                const old_skip = self.skip_newlines;
                defer self.skip_newlines = old_skip;
                self.skip_newlines = true;

                errdefer lhs.deinit(self.allocator);
                const mhs = try self.parseExpr(.expr);
                errdefer mhs.deinit(self.allocator);

                if (self.skip(&.{.slice}) != null) {
                    const rhs = try self.parseExpr(.expr);
                    errdefer rhs.deinit(self.allocator);

                    _ = try self.expect(&.{.rlist});
                    const tri = try self.allocator.create(Expr.Tri);
                    tri.* = .{ .lhs = lhs, .mhs = mhs, .rhs = rhs };
                    const location = self.getLocation(start);
                    lhs = .{ .data = .{ .slice = tri }, .location = location };
                } else {
                    _ = try self.expect(&.{.rlist});
                    const bin = try self.allocator.create(Expr.Bin);
                    bin.* = .{ .lhs = lhs, .rhs = mhs };
                    const location = self.getLocation(start);
                    lhs = .{ .data = .{ .get = bin }, .location = location };
                }
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
            return try self.parseStr(parse_type);
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
        } else if (parse_type != .pattern and self.peek(&.{.assert})) {
            return parse_type.fromExpr(try self.parseAssert());
        } else {
            _ = try self.expect(&.{});
            unreachable;
        }
    }

    fn parseName(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        const token = try self.expect(&.{.name});
        const location = self.getLocation(start);

        return .{
            .data = switch (parse_type) {
                .expr, .pattern => .{ .name = token.content },
                .both => .{ .both = .{
                    .expr = .{ .name = token.content },
                    .pattern = .{ .name = token.content },
                } },
            },
            .location = location,
        };
    }

    fn parseNum(self: *Parser) Error!Expr {
        const start = self.getStart();
        const token = try self.expect(&.{.num});
        const value = std.fmt.parseFloat(f64, token.content) catch {
            self.error_reason = .invalid_float;
            return error.ParseError;
        };
        const location = self.getLocation(start);
        return .{ .data = .{ .num = value }, .location = location };
    }

    fn parseBool(self: *Parser) Error!Expr {
        const start = self.getStart();
        const token = try self.expect(&.{.bool});
        const value = std.mem.eql(u8, token.content, "true");
        const location = self.getLocation(start);
        return .{ .data = .{ .bool = value }, .location = location };
    }

    fn parseNull(self: *Parser) Error!Expr {
        const start = self.getStart();
        _ = try self.expect(&.{.null});
        const location = self.getLocation(start);
        return .{ .data = .null, .location = location };
    }

    fn parseStr(self: *Parser, comptime parse_type: ParseType) Error!parse_type.Result() {
        const start = self.getStart();
        _ = try self.expect(&.{.str});
        var state = StrState(parse_type).init(self.allocator);
        defer state.deinit();
        return try self.parseStrTail(parse_type, &state, start);
    }

    fn parseStrTail(self: *Parser, comptime parse_type: ParseType, state: *StrState(parse_type), start: usize) Error!parse_type.Result() {
        while (true) {
            const chunk_start = self.lexer.index;
            const chunk = try self.lexer.strChunk(self.allocator);
            const chunk_end = self.lexer.index - @as(usize, if (chunk.expr) 2 else 1);
            const chunk_location = Instr.Location{
                .index = @truncate(chunk_start),
                .len = @truncate(chunk_end - chunk_start),
            };
            self.last_index = self.lexer.index;

            if (chunk.expr) {
                const result: parse_type.Result() = result: {
                    errdefer self.allocator.free(chunk.content);

                    const old_skip = self.skip_newlines;
                    defer self.skip_newlines = old_skip;
                    self.skip_newlines = true;

                    const result = try self.parseExpr(parse_type);
                    errdefer result.deinit(self.allocator);
                    _ = try self.expect(&.{.rdict});

                    break :result result;
                };

                switch (parse_type) {
                    .expr, .pattern => try state.append(chunk.content, chunk_location, result),
                    .both => switch (result.detail()) {
                        .expr => |expr| {
                            var expr_state = state.toExpr();
                            try expr_state.append(chunk.content, chunk_location, expr);
                            return parse_type.fromExpr(try self.parseStrTail(.expr, &expr_state, start));
                        },
                        .pattern => |pattern| {
                            var pattern_state = state.toPattern();
                            try pattern_state.append(chunk.content, chunk_location, pattern);
                            return parse_type.fromPattern(try self.parseStrTail(.pattern, &pattern_state, start));
                        },
                        .both => |both| try state.append(chunk.content, chunk_location, both),
                    },
                }
            } else if (state.isEmpty()) {
                const location = self.getLocation(start);
                const expr = .{
                    .data = .{ .str = chunk.content },
                    .location = location,
                };
                return try parse_type.fromLit(self.allocator, expr);
            } else {
                const location = self.getLocation(start);
                return .{
                    .data = try state.toResult(chunk.content, chunk_location),
                    .location = location,
                };
            }
        }
    }

    fn parseIgnore(self: *Parser) Error!Pattern {
        const start = self.getStart();
        _ = try self.expect(&.{.ignore});
        const location = self.getLocation(start);
        return .{ .data = .ignore, .location = location };
    }

    fn parseCol(
        self: *Parser,
        comptime col_type: ColType,
        comptime parse_type: ParseType,
        start: Token.Type,
        end: Token.Type,
    ) Error!parse_type.Result() {
        const col_start = self.getStart();
        _ = try self.expect(&.{start});
        const items_start = self.getStart();

        const old_skip = self.skip_newlines;
        defer self.skip_newlines = old_skip;
        self.skip_newlines = true;

        var state = ColState(col_type, parse_type).init(self.allocator);
        defer state.deinit();

        return try self.parseColTail(col_type, parse_type, &state, col_start, items_start, end);
    }

    fn parseColTail(
        self: *Parser,
        comptime col_type: ColType,
        comptime parse_type: ParseType,
        state: *ColState(col_type, parse_type),
        start: usize,
        init_items_start: usize,
        end: Token.Type,
    ) Error!parse_type.Result() {
        var items_start = init_items_start;
        const items_location = while (true) {
            var items_location: ?Instr.Location = self.getLocation(start);
            if (self.skip(&.{end}) != null) break items_location.?;
            if (self.skip(&.{.mul}) == null) items_location = null;
            var owner = true;

            var key: Expr = undefined;
            var item: parse_type.Result() = undefined;

            if (col_type != .dict or items_location != null) {
                item = try self.parseExpr(parse_type);
            } else if (self.peek(&.{.name}) and self.peekAhead(1, &.{ .comma, end })) {
                const key_start = self.getStart();
                const token = try self.expect(&.{.name});
                const location = self.getLocation(key_start);

                key = .{
                    .data = .{ .str = try self.allocator.dupe(u8, token.content) },
                    .location = location,
                };

                item = .{
                    .data = switch (parse_type) {
                        .expr, .pattern => .{ .name = token.content },
                        .both => .{ .both = .{
                            .expr = .{ .name = token.content },
                            .pattern = .{ .name = token.content },
                        } },
                    },
                    .location = location,
                };
            } else {
                key = try self.parseExpr(.expr);
                errdefer key.deinit(self.allocator);
                _ = try self.expect(&.{.assign});
                item = try self.parseExpr(parse_type);
            }

            errdefer if (owner) {
                if (col_type == .dict and items_location == null) key.deinit(self.allocator);
                item.deinit(self.allocator);
            };

            if (!self.peek(&.{end})) _ = try self.expect(&.{.comma});

            if (items_location != null) items_start = self.getStart();

            switch (parse_type) {
                .expr, .pattern => {
                    try state.append(items_location, key, item);
                },
                .both => switch (item.detail()) {
                    .expr => |expr| {
                        var expr_state = try state.toExpr();
                        defer expr_state.deinit();
                        try expr_state.append(items_location, key, expr);
                        owner = false;
                        return parse_type.fromExpr(try self.parseColTail(col_type, .expr, &expr_state, start, items_start, end));
                    },
                    .pattern => |pattern| {
                        var pattern_state = try state.toPattern();
                        defer pattern_state.deinit();
                        try pattern_state.append(items_location, key, pattern);
                        owner = false;
                        return parse_type.fromPattern(try self.parseColTail(col_type, .pattern, &pattern_state, start, items_start, end));
                    },
                    .both => |both| {
                        try state.append(items_location, key, both);
                    },
                },
            }
        };

        return .{
            .data = try state.toResult(items_location),
            .location = self.getLocation(start),
        };
    }

    fn parseLambda(self: *Parser) Error!Expr {
        const old_is_gen = self.is_gen;
        defer self.is_gen = old_is_gen;
        self.is_gen = false;

        const start = self.getStart();

        const matcher = try self.allocator.create(Expr.Matcher);
        errdefer self.allocator.destroy(matcher);
        matcher.pattern = try self.parseCol(.list, .pattern, .lambda, .lambda);
        errdefer matcher.pattern.deinit(self.allocator);
        matcher.expr = try self.parseExpr(.expr);

        const location = self.getLocation(start);
        return .{ .data = .{ .lambda = matcher }, .location = location };
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
        const start = self.getStart();
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

        const location = self.getLocation(start);
        return .{ .data = .{ .gen = expr_ptr }, .location = location };
    }

    fn parseMatch(self: *Parser) Error!Expr {
        const start = self.getStart();
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

            const pattern = try self.parseGuard(.pattern);
            errdefer pattern.deinit(self.allocator);

            _ = try self.expect(&.{.assign});

            const expr = try self.parseExpr(.expr);
            errdefer expr.deinit(self.allocator);

            if (!self.peek(&.{.end})) _ = try self.expect(&.{.newline});

            try matchers.append(.{ .pattern = pattern, .expr = expr });
        }

        const match = try self.allocator.create(Expr.Match);
        errdefer self.allocator.destroy(match);
        match.* = .{
            .subject = subject,
            .matchers = try matchers.toOwnedSlice(),
        };
        const location = self.getLocation(start);
        return .{ .data = .{ .match = match }, .location = location };
    }

    fn parseIf(self: *Parser) Error!Expr {
        var start = self.getStart();
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
            const result = try self.parseGuard(.both);
            const assign = switch (result.data) {
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

            try branches.append(.{ .cond = cond, .then = body.expr, .start = start });

            start = self.lexer.getIndex(body.end);
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
            expr = .{ .data = .null, .location = self.getLocation(start) };
        }
        errdefer expr.deinit(self.allocator);

        while (branches.popOrNull()) |branch| {
            errdefer branch.deinit(self.allocator);
            expr = .{
                .data = switch (branch.cond) {
                    .@"if" => |cond| data: {
                        const if_ = try self.allocator.create(Expr.Tri);
                        if_.* = .{ .lhs = cond, .mhs = branch.then, .rhs = expr };
                        break :data .{ .@"if" = if_ };
                    },
                    .match => |cond| data: {
                        const matchers = try self.allocator.alloc(Expr.Matcher, 2);
                        errdefer self.allocator.free(matchers);
                        matchers[0] = .{ .pattern = cond.pattern, .expr = branch.then };
                        matchers[1] = .{ .pattern = .{ .data = .ignore, .location = .{} }, .expr = expr };

                        const match = try self.allocator.create(Expr.Match);
                        match.* = .{ .subject = cond.expr, .matchers = matchers };
                        break :data .{ .match = match };
                    },
                },
                .location = self.getLocation(branch.start),
            };
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

        const start = self.getStart();
        const subject = try self.parseExpr(.expr);
        const location = self.getLocation(start);
        errdefer subject.deinit(self.allocator);

        _ = try self.expect(&.{.do});

        const body = try self.parseBody(&.{.end});
        errdefer body.expr.deinit(self.allocator);

        const for_ = try self.allocator.create(Expr.For);
        for_.* = .{
            .subject = subject,
            .matcher = .{ .pattern = pattern, .expr = body.expr },
        };
        return .{ .data = .{ .@"for" = for_ }, .location = location };
    }

    fn parseYield(self: *Parser) Error!Expr {
        std.debug.assert(self.is_gen);
        const start = self.getStart();
        _ = try self.expect(&.{.yield});
        const is_all = self.skip(&.{.mul}) != null;

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpr(.expr);

        const location = self.getLocation(start);
        return .{
            .data = if (is_all) .{ .yield_all = expr } else .{ .yield = expr },
            .location = location,
        };
    }

    fn parseReturn(self: *Parser) Error!Expr {
        const start = self.getStart();
        _ = try self.expect(&.{.@"return"});

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpr(.expr);

        const location = self.getLocation(start);
        return .{ .data = .{ .@"return" = expr }, .location = location };
    }

    fn parseAssert(self: *Parser) Error!Expr {
        const start = self.getStart();
        _ = try self.expect(&.{.assert});

        const expr = try self.allocator.create(Expr);
        errdefer self.allocator.destroy(expr);
        expr.* = try self.parseExpr(.expr);

        const location = self.getLocation(start);
        return .{ .data = .{ .assert = expr }, .location = location };
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

    fn peekAhead(self: *Parser, n: usize, token_types: []const Token.Type) bool {
        if (n == 0) return self.peek(token_types);

        _ = self.peek(&.{});

        const lexer = self.lexer;
        const token = self.token;
        const expected = self.expected;
        defer {
            self.lexer = lexer;
            self.token = token;
            self.expected = expected;
        }

        self.token = null;
        return self.peekAhead(n - 1, token_types);
    }

    fn skip(self: *Parser, token_types: []const Token.Type) ?Token {
        if (!self.peek(token_types)) return null;
        const token = self.token.?;
        self.token = null;

        self.last_index = self.lexer.index;
        if (self.lexer.next_token) |token_| self.last_index -= token_.content.len;

        return token;
    }

    fn expect(self: *Parser, token_types: []const Token.Type) !Token {
        if (self.skip(token_types)) |token| return token;

        self.error_reason = .unexpected_token;
        return error.ParseError;
    }

    pub fn printErrorReason(self: *Parser) void {
        const token = self.token.?;
        switch (self.error_reason) {
            .unexpected_token => {
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
            },
            .invalid_float => {
                std.debug.print("cannot convert to float\n", .{});
            },
            .duplicate_fn_name => {
                std.debug.print("duplicate fn name\n", .{});
            },
        }
    }

    fn getStart(self: *Parser) usize {
        _ = self.peek(&.{});
        var index = self.lexer.index;
        if (self.lexer.next_token) |token| index -= token.content.len;
        if (self.token) |token| index -= token.content.len;
        return index;
    }

    fn getLocation(self: *Parser, start: usize) Instr.Location {
        return .{
            .index = @truncate(start),
            .len = @truncate(self.last_index - start),
        };
    }

    fn isSpace(self: *Parser, init_index: usize) bool {
        return switch (self.lexer.content[init_index]) {
            ' ', '\n', '\t', '\r' => true,
            else => false,
        };
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
            .both => result.data != .pattern,
        };
    }

    inline fn toExpr(comptime self: ParseType, allocator: std.mem.Allocator, result: self.Result()) Expr {
        return switch (self) {
            .expr => result,
            .pattern => unreachable,
            .both => switch (result.detail()) {
                .expr => |expr| expr,
                .pattern => unreachable,
                .both => |both| expr: {
                    both.pattern().deinit(allocator);
                    break :expr both.expr();
                },
            },
        };
    }

    inline fn fromExpr(comptime self: ParseType, expr: Expr) self.Result() {
        return switch (self) {
            .expr => expr,
            .pattern => unreachable,
            .both => .{
                .data = .{ .expr = expr.data },
                .location = expr.location,
            },
        };
    }

    inline fn isPattern(comptime self: ParseType, result: self.Result()) bool {
        return switch (self) {
            .expr => false,
            .pattern => true,
            .both => result.data != .expr,
        };
    }

    inline fn toPattern(comptime self: ParseType, allocator: std.mem.Allocator, result: self.Result()) Pattern {
        return switch (self) {
            .expr => unreachable,
            .pattern => result,
            .both => switch (result.detail()) {
                .expr => unreachable,
                .pattern => |pattern| pattern,
                .both => |both| pattern: {
                    both.expr().deinit(allocator);
                    break :pattern both.pattern();
                },
            },
        };
    }

    inline fn fromPattern(comptime self: ParseType, pattern: Pattern) self.Result() {
        return switch (self) {
            .expr => unreachable,
            .pattern => pattern,
            .both => .{
                .data = .{ .pattern = pattern.data },
                .location = pattern.location,
            },
        };
    }

    inline fn fromLit(comptime self: ParseType, allocator: std.mem.Allocator, expr: Expr) !self.Result() {
        switch (self) {
            .expr => return expr,
            .pattern => {
                errdefer expr.deinit(allocator);
                const expr_ptr = try allocator.create(Expr);
                expr_ptr.* = expr;
                return .{
                    .data = .{ .expr = expr_ptr },
                    .location = expr.location,
                };
            },
            .both => {
                errdefer expr.deinit(allocator);
                const expr_ptr = try allocator.create(Expr);
                errdefer allocator.destroy(expr_ptr);
                expr_ptr.* = try expr.clone(allocator);
                return .{
                    .data = .{ .both = .{
                        .expr = expr.data,
                        .pattern = .{ .expr = expr_ptr },
                    } },
                    .location = expr.location,
                };
            },
        }
    }
};

const ParseResult = struct {
    data: Data,
    location: Instr.Location,

    const Data = union(ParseType) {
        expr: Expr.Data,
        pattern: Pattern.Data,
        both: Both.Data,
    };

    const Detail = union(ParseType) {
        expr: Expr,
        pattern: Pattern,
        both: Both,
    };

    fn detail(self: ParseResult) Detail {
        return switch (self.data) {
            inline else => |data, tag| @unionInit(Detail, @tagName(tag), .{
                .data = data,
                .location = self.location,
            }),
        };
    }

    fn deinit(self: ParseResult, allocator: std.mem.Allocator) void {
        switch (self.detail()) {
            inline else => |value| value.deinit(allocator),
        }
    }
};

const Both = struct {
    data: Data,
    location: Instr.Location,

    const Data = struct {
        expr: Expr.Data,
        pattern: Pattern.Data,
    };

    fn expr(self: Both) Expr {
        return .{ .data = self.data.expr, .location = self.location };
    }

    fn pattern(self: Both) Pattern {
        return .{ .data = self.data.pattern, .location = self.location };
    }

    fn deinit(self: Both, allocator: std.mem.Allocator) void {
        self.expr().deinit(allocator);
        self.pattern().deinit(allocator);
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

        inline fn append(self: *Self, items_location: ?Instr.Location, key: Expr, value: Col) !void {
            if (items_location) |location| {
                try self.appendCol(location, value);
            } else if (col_type == .dict) {
                try self.appendItem(.{ .key = key, .value = value });
            } else {
                try self.appendItem(value);
            }
        }

        fn appendItem(self: *Self, item: Item) !void {
            try self.items.append(item);
        }

        fn appendCol(self: *Self, location: Instr.Location, col: Col) !void {
            try self.itemsToCol(location);
            try self.cols.append(col);
        }

        fn toExpr(self: *Self) !ColState(col_type, .expr) {
            if (parse_type != .both) unreachable;

            var expr_state = ColState(col_type, .expr).init(self.items.allocator);
            errdefer expr_state.deinit();

            try expr_state.items.ensureUnusedCapacity(self.items.items.len);
            for (self.items.items) |both| {
                if (col_type == .dict) {
                    both.value.pattern().deinit(self.items.allocator);
                    expr_state.items.appendAssumeCapacity(.{ .key = both.key, .value = both.value.expr() });
                } else {
                    both.pattern().deinit(self.items.allocator);
                    expr_state.items.appendAssumeCapacity(both.expr());
                }
            }
            self.items.clearAndFree();

            try expr_state.cols.ensureUnusedCapacity(self.cols.items.len);
            for (self.cols.items) |both| {
                both.pattern().deinit(self.items.allocator);
                expr_state.cols.appendAssumeCapacity(both.expr());
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
                    both.value.expr().deinit(self.items.allocator);
                    pattern_state.items.appendAssumeCapacity(.{ .key = both.key, .value = both.value.pattern() });
                } else {
                    both.expr().deinit(self.items.allocator);
                    pattern_state.items.appendAssumeCapacity(both.pattern());
                }
            }
            self.items.clearAndFree();

            try pattern_state.cols.ensureUnusedCapacity(self.cols.items.len);
            for (self.cols.items) |both| {
                both.expr().deinit(self.items.allocator);
                pattern_state.cols.appendAssumeCapacity(both.pattern());
            }
            self.cols.clearAndFree();

            return pattern_state;
        }

        fn aggregate(self: *Self, comptime field: []const u8, comptime tag: []const u8) !Col.Data {
            switch (parse_type) {
                .expr, .pattern => {
                    return @unionInit(Col.Data, tag, try @field(self, field).toOwnedSlice());
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
                            exprs[i] = .{ .key = both.key, .value = both.value.expr() };
                            patterns[i] = .{ .key = try both.key.clone(allocator), .value = both.value.pattern() };
                        } else {
                            exprs[i] = both.expr();
                            patterns[i] = both.pattern();
                        }
                    }
                    @field(self, field).clearAndFree();

                    return .{
                        .expr = @unionInit(Expr.Data, tag, exprs),
                        .pattern = @unionInit(Pattern.Data, tag, patterns),
                    };
                },
            }
        }

        fn itemsToCol(self: *Self, location: Instr.Location) !void {
            if (self.items.items.len == 0) return;
            const col = Col{
                .data = try self.aggregate("items", @tagName(col_type)),
                .location = location,
            };
            errdefer col.deinit(self.items.allocator);
            try self.cols.append(col);
        }

        fn toResult(self: *Self, location: Instr.Location) !parse_type.Result().Data {
            var result: Col.Data = undefined;
            if (self.cols.items.len == 0) {
                result = try self.aggregate("items", @tagName(col_type));
            } else {
                try self.itemsToCol(location);
                result = try self.aggregate("cols", @tagName(col_type) ++ "s");
            }
            return if (parse_type == .both) .{ .both = result } else result;
        }
    };
}

fn StrState(comptime parse_type: ParseType) type {
    return switch (parse_type) {
        .expr => struct {
            const Self = @This();

            exprs: std.ArrayList(Expr),

            fn init(allocator: std.mem.Allocator) Self {
                return .{ .exprs = std.ArrayList(Expr).init(allocator) };
            }

            fn deinit(self: *Self) void {
                self.exprs.deinit();
            }

            fn isEmpty(self: *Self) bool {
                return self.exprs.items.len == 0;
            }

            fn append(self: *Self, prefix: []const u8, prefix_location: Instr.Location, item: Expr) !void {
                errdefer item.deinit(self.exprs.allocator);
                try self.appendChunk(prefix, prefix_location);
                try self.exprs.append(item);
            }

            fn toResult(self: *Self, suffix: []const u8, suffix_location: Instr.Location) !Expr.Data {
                try self.appendChunk(suffix, suffix_location);
                const bin = try self.exprs.allocator.create(Expr.Bin);
                errdefer self.exprs.allocator.destroy(bin);
                bin.* = .{
                    .lhs = .{
                        .data = .{ .global = .str },
                        .location = .{},
                    },
                    .rhs = .{
                        .data = .{ .list = try self.exprs.toOwnedSlice() },
                        .location = .{},
                    },
                };
                return .{ .call = bin };
            }

            fn appendChunk(self: *Self, chunk: []const u8, chunk_location: Instr.Location) !void {
                if (chunk.len == 0) {
                    self.exprs.allocator.free(chunk);
                } else {
                    errdefer self.exprs.allocator.free(chunk);
                    try self.exprs.append(.{
                        .data = .{ .str = chunk },
                        .location = chunk_location,
                    });
                }
            }
        },
        .pattern => struct {
            const Self = @This();

            is_empty: bool = true,
            prefix: []const u8 = undefined,
            splits: std.ArrayList(Pattern.StrSplit),
            last_pattern: Pattern = undefined,

            fn init(allocator: std.mem.Allocator) Self {
                return .{ .splits = std.ArrayList(Pattern.StrSplit).init(allocator) };
            }

            fn deinit(self: *Self) void {
                self.splits.deinit();
                if (!self.is_empty) {
                    self.splits.allocator.free(self.prefix);
                    self.last_pattern.deinit(self.splits.allocator);
                }
            }

            fn isEmpty(self: *Self) bool {
                return self.is_empty;
            }

            fn append(self: *Self, prefix: []const u8, _: Instr.Location, item: Pattern) !void {
                if (self.is_empty) {
                    self.is_empty = false;
                    self.prefix = prefix;
                } else {
                    errdefer {
                        self.splits.allocator.free(prefix);
                        item.deinit(self.splits.allocator);
                    }
                    try self.splits.append(.{
                        .pattern = self.last_pattern,
                        .separator = prefix,
                    });
                }
                self.last_pattern = item;
            }

            fn toResult(self: *Self, suffix: []const u8, _: Instr.Location) !Pattern.Data {
                errdefer self.splits.allocator.free(suffix);
                const str = try self.splits.allocator.create(Pattern.Str);
                errdefer self.splits.allocator.destroy(str);
                str.* = .{
                    .prefix = self.prefix,
                    .splits = try self.splits.toOwnedSlice(),
                    .last_pattern = self.last_pattern,
                    .suffix = suffix,
                };
                return .{ .str = str };
            }
        },
        .both => struct {
            allocator: std.mem.Allocator,
            expr: StrState(.expr),
            pattern: StrState(.pattern),

            const Self = @This();

            fn init(allocator: std.mem.Allocator) Self {
                return .{
                    .allocator = allocator,
                    .expr = StrState(.expr).init(allocator),
                    .pattern = StrState(.pattern).init(allocator),
                };
            }

            fn deinit(self: *Self) void {
                self.expr.deinit();
                self.pattern.deinit();
            }

            fn toExpr(self: *Self) StrState(.expr) {
                self.pattern.deinit();
                return self.expr;
            }

            fn toPattern(self: *Self) StrState(.pattern) {
                self.expr.deinit();
                return self.pattern;
            }

            fn isEmpty(self: *Self) bool {
                const is_empty = self.expr.isEmpty();
                std.debug.assert(is_empty == self.pattern.isEmpty());
                return is_empty;
            }

            fn append(self: *Self, prefix: []const u8, prefix_location: Instr.Location, item: Both) !void {
                const prefix_copy = self.allocator.dupe(u8, prefix) catch |err| {
                    self.allocator.free(prefix);
                    item.deinit(self.allocator);
                    return err;
                };
                self.expr.append(prefix, prefix_location, item.expr()) catch |err| {
                    self.allocator.free(prefix_copy);
                    item.pattern().deinit(self.allocator);
                    return err;
                };
                try self.pattern.append(prefix_copy, prefix_location, item.pattern());
            }

            fn toResult(self: *Self, suffix: []const u8, suffix_location: Instr.Location) !ParseResult.Data {
                const suffix_copy = self.allocator.dupe(u8, suffix) catch |err| {
                    self.allocator.free(suffix);
                    return err;
                };
                const expr = self.expr.toResult(suffix, suffix_location) catch |err| {
                    self.allocator.free(suffix_copy);
                    return err;
                };
                errdefer (Expr{ .data = expr, .location = .{} }).deinit(self.allocator);
                const pattern = try self.pattern.toResult(suffix_copy, suffix_location);
                return .{ .both = .{
                    .expr = expr,
                    .pattern = pattern,
                } };
            }
        },
    };
}

const Let = struct {
    pattern: Pattern,
    expr: Expr,
    default: ?Expr,
    start: usize,

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
    start: usize,

    fn deinit(self: IfBranch, allocator: std.mem.Allocator) void {
        self.cond.deinit(allocator);
        self.then.deinit(allocator);
    }
};

test "parse num 1" {
    const expr = try internal_parse(std.testing.allocator, "1337");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .num = 1337 },
        .location = .{ .index = 0, .len = 4 },
    }, expr);
}

test "parse num 2" {
    const expr = try internal_parse(std.testing.allocator, "45.67");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .num = 45.67 },
        .location = .{ .index = 0, .len = 5 },
    }, expr);
}

test "parse bool 1" {
    const expr = try internal_parse(std.testing.allocator, "true");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .bool = true },
        .location = .{ .index = 0, .len = 4 },
    }, expr);
}

test "parse bool 2" {
    const expr = try internal_parse(std.testing.allocator, "false");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .bool = false },
        .location = .{ .index = 0, .len = 5 },
    }, expr);
}

test "parse null" {
    const expr = try internal_parse(std.testing.allocator, "null");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .null,
        .location = .{ .index = 0, .len = 4 },
    }, expr);
}

test "parse operators" {
    const expr = try internal_parse(std.testing.allocator, "1 * 2 + 3 / -4");
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .add = &.{
            .lhs = .{
                .data = .{ .mul = &.{
                    .lhs = .{
                        .data = .{ .num = 1 },
                        .location = .{ .index = 0, .len = 1 },
                    },
                    .rhs = .{
                        .data = .{ .num = 2 },
                        .location = .{ .index = 4, .len = 1 },
                    },
                } },
                .location = .{ .index = 0, .len = 5 },
            },
            .rhs = .{
                .data = .{ .div = &.{
                    .lhs = .{
                        .data = .{ .num = 3 },
                        .location = .{ .index = 8, .len = 1 },
                    },
                    .rhs = .{
                        .data = .{ .neg = &.{
                            .data = .{ .num = 4 },
                            .location = .{ .index = 13, .len = 1 },
                        } },
                        .location = .{ .index = 12, .len = 2 },
                    },
                } },
                .location = .{ .index = 8, .len = 6 },
            },
        } },
        .location = .{ .index = 0, .len = 14 },
    }, expr);
}

test "parse var" {
    const expr = try internal_parse(std.testing.allocator, (
        \\x = 4
        \\x * x
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .num = 4 },
                .location = .{ .index = 4, .len = 1 },
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "x" },
                    .location = .{ .index = 0, .len = 1 },
                },
                .expr = .{
                    .data = .{ .mul = &.{
                        .lhs = .{
                            .data = .{ .name = "x" },
                            .location = .{ .index = 6, .len = 1 },
                        },
                        .rhs = .{
                            .data = .{ .name = "x" },
                            .location = .{ .index = 10, .len = 1 },
                        },
                    } },
                    .location = .{ .index = 6, .len = 5 },
                },
            }},
        } },
        .location = .{ .index = 0, .len = 11 },
    }, expr);
}

test "parse lambda" {
    const expr = try internal_parse(std.testing.allocator, (
        \\sqr = |x| x * x
        \\sqr(4)
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .lambda = &.{
                    .pattern = .{
                        .data = .{ .list = &.{
                            .{
                                .data = .{ .name = "x" },
                                .location = .{ .index = 7, .len = 1 },
                            },
                        } },
                        .location = .{ .index = 6, .len = 3 },
                    },
                    .expr = .{
                        .data = .{ .mul = &.{
                            .lhs = .{
                                .data = .{ .name = "x" },
                                .location = .{ .index = 10, .len = 1 },
                            },
                            .rhs = .{
                                .data = .{ .name = "x" },
                                .location = .{ .index = 14, .len = 1 },
                            },
                        } },
                        .location = .{ .index = 10, .len = 5 },
                    },
                } },
                .location = .{ .index = 6, .len = 9 },
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "sqr" },
                    .location = .{ .index = 0, .len = 3 },
                },
                .expr = .{
                    .data = .{ .call = &.{
                        .lhs = .{
                            .data = .{ .name = "sqr" },
                            .location = .{ .index = 16, .len = 3 },
                        },
                        .rhs = .{
                            .data = .{ .list = &.{
                                .{
                                    .data = .{ .num = 4 },
                                    .location = .{ .index = 20, .len = 1 },
                                },
                            } },
                            .location = .{ .index = 19, .len = 3 },
                        },
                    } },
                    .location = .{ .index = 16, .len = 6 },
                },
            }},
        } },
        .location = .{ .index = 0, .len = 22 },
    }, expr);
}

test "parse closure" {
    const expr = try internal_parse(std.testing.allocator, (
        \\n = 2
        \\times_n = |x| x * n
        \\times_n(4)
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .num = 2 },
                .location = .{ .index = 4, .len = 1 },
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .name = "n" },
                    .location = .{ .index = 0, .len = 1 },
                },
                .expr = .{
                    .data = .{ .match = &.{
                        .subject = .{
                            .data = .{ .lambda = &.{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "x" },
                                            .location = .{ .index = 17, .len = 1 },
                                        },
                                    } },
                                    .location = .{ .index = 16, .len = 3 },
                                },
                                .expr = .{
                                    .data = .{ .mul = &.{
                                        .lhs = .{
                                            .data = .{ .name = "x" },
                                            .location = .{ .index = 20, .len = 1 },
                                        },
                                        .rhs = .{
                                            .data = .{ .name = "n" },
                                            .location = .{ .index = 24, .len = 1 },
                                        },
                                    } },
                                    .location = .{ .index = 20, .len = 5 },
                                },
                            } },
                            .location = .{ .index = 16, .len = 9 },
                        },
                        .matchers = &.{.{
                            .pattern = .{
                                .data = .{ .name = "times_n" },
                                .location = .{ .index = 6, .len = 7 },
                            },
                            .expr = .{
                                .data = .{ .call = &.{
                                    .lhs = .{
                                        .data = .{ .name = "times_n" },
                                        .location = .{ .index = 26, .len = 7 },
                                    },
                                    .rhs = .{
                                        .data = .{ .list = &.{
                                            .{
                                                .data = .{ .num = 4 },
                                                .location = .{ .index = 34, .len = 1 },
                                            },
                                        } },
                                        .location = .{ .index = 33, .len = 3 },
                                    },
                                } },
                                .location = .{ .index = 26, .len = 10 },
                            },
                        }},
                    } },
                    .location = .{ .index = 6, .len = 30 },
                },
            }},
        } },
        .location = .{ .index = 0, .len = 36 },
    }, expr);
}

test "parse dict" {
    const expr = try internal_parse(std.testing.allocator, (
        \\{"foo" = foo, *_} = {"foo" = 1, "bar" = 2, "baz" = 3}
        \\foo
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .match = &.{
            .subject = .{
                .data = .{ .dict = &.{
                    .{
                        .key = .{
                            .data = .{ .str = "foo" },
                            .location = .{ .index = 21, .len = 5 },
                        },
                        .value = .{
                            .data = .{ .num = 1 },
                            .location = .{ .index = 29, .len = 1 },
                        },
                    },
                    .{
                        .key = .{
                            .data = .{ .str = "bar" },
                            .location = .{ .index = 32, .len = 5 },
                        },
                        .value = .{
                            .data = .{ .num = 2 },
                            .location = .{ .index = 40, .len = 1 },
                        },
                    },
                    .{
                        .key = .{
                            .data = .{ .str = "baz" },
                            .location = .{ .index = 43, .len = 5 },
                        },
                        .value = .{
                            .data = .{ .num = 3 },
                            .location = .{ .index = 51, .len = 1 },
                        },
                    },
                } },
                .location = .{ .index = 20, .len = 33 },
            },
            .matchers = &.{.{
                .pattern = .{
                    .data = .{ .dicts = &.{
                        .{
                            .data = .{ .dict = &.{
                                .{
                                    .key = .{
                                        .data = .{ .str = "foo" },
                                        .location = .{ .index = 1, .len = 5 },
                                    },
                                    .value = .{
                                        .data = .{ .name = "foo" },
                                        .location = .{ .index = 9, .len = 3 },
                                    },
                                },
                            } },
                            .location = .{ .index = 0, .len = 13 },
                        },
                        .{
                            .data = .ignore,
                            .location = .{ .index = 15, .len = 1 },
                        },
                    } },
                    .location = .{ .index = 0, .len = 17 },
                },
                .expr = .{
                    .data = .{ .name = "foo" },
                    .location = .{ .index = 54, .len = 3 },
                },
            }},
        } },
        .location = .{ .index = 0, .len = 57 },
    }, expr);
}

test "parse module" {
    const expr = try parse(std.testing.allocator, (
        \\A = 0
        \\B = 1
        \\pub fn fib(*args) match args do
        \\  [0, a, _] = a
        \\  [n, a, b] = fib(n - 1, b, a + b)
        \\  [n]       = fib(n, A, B)
        \\end
    ));
    defer expr.deinit(std.testing.allocator);
    try std.testing.expectEqualDeep(Expr{
        .data = .{ .module = &.{
            .{
                .fn_name = null,
                .is_pub = false,
                .pattern = .{
                    .data = .{ .name = "A" },
                    .location = .{ .index = 0, .len = 1 },
                },
                .subject = .{
                    .data = .{ .num = 0 },
                    .location = .{ .index = 4, .len = 1 },
                },
            },
            .{
                .fn_name = null,
                .is_pub = false,
                .pattern = .{
                    .data = .{ .name = "B" },
                    .location = .{ .index = 6, .len = 1 },
                },
                .subject = .{
                    .data = .{ .num = 1 },
                    .location = .{ .index = 10, .len = 1 },
                },
            },
            .{
                .fn_name = "fib",
                .is_pub = true,
                .pattern = .{
                    .data = .{ .lists = &.{
                        .{
                            .data = .{ .name = "args" },
                            .location = .{ .index = 24, .len = 4 },
                        },
                    } },
                    .location = .{ .index = 22, .len = 7 },
                },
                .subject = .{
                    .data = .{ .match = &.{
                        .subject = .{
                            .data = .{ .name = "args" },
                            .location = .{ .index = 36, .len = 4 },
                        },
                        .matchers = &.{
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .expr = &.{
                                                .data = .{ .num = 0 },
                                                .location = .{ .index = 47, .len = 1 },
                                            } },
                                            .location = .{ .index = 47, .len = 1 },
                                        },
                                        .{
                                            .data = .{ .name = "a" },
                                            .location = .{ .index = 50, .len = 1 },
                                        },
                                        .{
                                            .data = .ignore,
                                            .location = .{ .index = 53, .len = 1 },
                                        },
                                    } },
                                    .location = .{ .index = 46, .len = 9 },
                                },
                                .expr = .{
                                    .data = .{ .name = "a" },
                                    .location = .{ .index = 58, .len = 1 },
                                },
                            },
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "n" },
                                            .location = .{ .index = 63, .len = 1 },
                                        },
                                        .{
                                            .data = .{ .name = "a" },
                                            .location = .{ .index = 66, .len = 1 },
                                        },
                                        .{
                                            .data = .{ .name = "b" },
                                            .location = .{ .index = 69, .len = 1 },
                                        },
                                    } },
                                    .location = .{ .index = 62, .len = 9 },
                                },
                                .expr = .{
                                    .data = .{ .call = &.{
                                        .lhs = .{
                                            .data = .{ .name = "fib" },
                                            .location = .{ .index = 74, .len = 3 },
                                        },
                                        .rhs = .{
                                            .data = .{ .list = &.{
                                                .{
                                                    .data = .{ .sub = &.{
                                                        .lhs = .{
                                                            .data = .{ .name = "n" },
                                                            .location = .{ .index = 78, .len = 1 },
                                                        },
                                                        .rhs = .{
                                                            .data = .{ .num = 1 },
                                                            .location = .{ .index = 82, .len = 1 },
                                                        },
                                                    } },
                                                    .location = .{ .index = 78, .len = 5 },
                                                },
                                                .{
                                                    .data = .{ .name = "b" },
                                                    .location = .{ .index = 85, .len = 1 },
                                                },
                                                .{
                                                    .data = .{ .add = &.{
                                                        .lhs = .{
                                                            .data = .{ .name = "a" },
                                                            .location = .{ .index = 88, .len = 1 },
                                                        },
                                                        .rhs = .{
                                                            .data = .{ .name = "b" },
                                                            .location = .{ .index = 92, .len = 1 },
                                                        },
                                                    } },
                                                    .location = .{ .index = 88, .len = 5 },
                                                },
                                            } },
                                            .location = .{ .index = 77, .len = 17 },
                                        },
                                    } },
                                    .location = .{ .index = 74, .len = 20 },
                                },
                            },
                            .{
                                .pattern = .{
                                    .data = .{ .list = &.{
                                        .{
                                            .data = .{ .name = "n" },
                                            .location = .{ .index = 98, .len = 1 },
                                        },
                                    } },
                                    .location = .{ .index = 97, .len = 3 },
                                },
                                .expr = .{
                                    .data = .{ .call = &.{
                                        .lhs = .{
                                            .data = .{ .name = "fib" },
                                            .location = .{ .index = 109, .len = 3 },
                                        },
                                        .rhs = .{
                                            .data = .{ .list = &.{
                                                .{
                                                    .data = .{ .name = "n" },
                                                    .location = .{ .index = 113, .len = 1 },
                                                },
                                                .{
                                                    .data = .{ .name = "A" },
                                                    .location = .{ .index = 116, .len = 1 },
                                                },
                                                .{
                                                    .data = .{ .name = "B" },
                                                    .location = .{ .index = 119, .len = 1 },
                                                },
                                            } },
                                            .location = .{ .index = 112, .len = 9 },
                                        },
                                    } },
                                    .location = .{ .index = 109, .len = 12 },
                                },
                            },
                        },
                    } },
                    .location = .{ .index = 30, .len = 95 },
                },
            },
        } },
        .location = .{ .index = 0, .len = 125 },
    }, expr);
}
