pub const Instr = union(enum) {
    local: usize,
    pop: usize,
    num: f64,
    bool: bool,
    null,

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
};
