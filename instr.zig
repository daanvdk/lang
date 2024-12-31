pub const Instr = union(enum) {
    global: Global,
    local: usize,
    pop: usize,
    num: f64,
    bool: bool,
    null,

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
    jmp_if: usize,

    ret,
    no_match,

    pub const Global = enum {
        is_list,
    };
};
