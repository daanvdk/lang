pub const Instr = union(enum) {
    num: f64,
    bool: bool,
    null,

    ret,
};
