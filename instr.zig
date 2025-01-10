pub const Instr = union(enum) {
    global: Global,
    local: usize,
    pop: usize,
    num: f64,
    bool: bool,
    null,
    short_str: [8]u8,
    long_str: packed struct {
        index: u32,
        len: u32,
    },

    nil,
    cons,
    decons,

    empty_dict,
    put_dict,
    pop_dict,

    lambda: packed struct {
        caps: u32,
        len: u32,
    },

    call,
    tail_call,
    get,

    pow,
    pos,
    neg,
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
    in,

    not,

    jmp: usize,
    jmp_back: usize,
    jmp_if: usize,

    ret,
    yield,
    no_match,

    pub const Global = enum {
        is_num,
        is_bool,
        is_null,
        is_str,
        is_list,
        is_dict,
        is_func,
        str,
        join,
        print,
        import,

        next,
        send,
        list,
        dict,
        map,
        filter,
        reduce,
        count,
        len,

        @"@str_send",
        @"@dict_send",
        @"@dict_tail",
    };
};
