pub const Token = struct {
    type: Type,
    content: []const u8,

    pub const Type = enum {
        name,
        num,
        bool,
        null,
        str,
        ignore,

        llist,
        rlist,
        comma,
        ldict,
        rdict,
        lambda,

        lpar,
        rpar,
        pipe,
        dot,

        pow,
        mul,
        div,
        add,
        sub,

        eq,
        ne,
        lt,
        le,
        gt,
        ge,

        not,
        @"and",
        @"or",

        assign,

        do,
        end,
        match,
        @"if",
        elif,
        @"else",
        @"for",
        in,
        yield,
        @"return",

        @"pub",
        @"fn",

        space,
        comment,
        newline,

        unknown,
        eof,
    };
};
