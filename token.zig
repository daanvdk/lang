pub const Token = struct {
    type: Type,
    content: []const u8,

    pub const Type = enum {
        num,
        bool,
        null,

        space,
        newline,

        unknown,
        eof,
    };
};
