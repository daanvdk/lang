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

        space,
        newline,

        unknown,
        eof,
    };
};
