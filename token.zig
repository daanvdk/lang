pub const Token = struct {
    type: Type,
    content: []const u8,

    pub const Type = enum {
        num,
        bool,
        null,

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

        space,
        newline,

        unknown,
        eof,
    };
};
