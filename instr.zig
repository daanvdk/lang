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
    lambda: packed struct {
        caps: u32,
        len: u32,
    },

    call,
    tail_call,

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

    not,

    jmp: usize,
    jmp_back: usize,
    jmp_if: usize,

    ret,
    yield,
    no_match,

    pub const Global = enum {
        is_list,
        str,
        join,
        print,

        next,
        send,
        list,
        map,
        filter,
        reduce,
        count,
    };
};
