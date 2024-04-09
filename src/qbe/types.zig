
/// The base types
pub const BaseTy = enum(u2) {
    word   = 0, // 32-bit integer
    long   = 1, // 64-bit integer
    single = 2, // 32-bit float
    double = 3, // 64-bit float
};

/// Used in aggregate type and data definitions
pub const ExtTy = enum(u3) {
    byte   = 4, //  8-bit integer
    half   = 5, // 16-bit integer

    word   = 0, // 32-bit integer
    long   = 1, // 64-bit integer
    single = 2, // 32-bit float
    double = 3, // 64-bit float
};

/// Identifier for aggregate types.
pub const AggTy = []const u8;

/// Used in aggregate type definitions.
pub const SubTy = union(enum(u1)) {
    ext_ty: ExtTy = 0,
    agg_ty: AggTy = 1,
};

/// Only used in AbiTy
pub const SubwTy = enum(u3) {
    s_byte = 4, // signed    8-bit integer
    s_half = 5, // signed   16-bit integer
    u_byte = 6, // unsigned  8-bit integer
    u_half = 7, // unsigned 16-bit integer

    word   = 0, //          32-bit integer
    long   = 1, //          64-bit integer
    single = 2, //          32-bit float
    double = 3, //          64-bit float
};

/// Used for function arguments, parameters, and return types.
pub const AbiTy = union(enum(u1)) {
    subw_ty:  SubwTy = 0,
    agg_ty:   AggTy  = 1,
};

pub const Val = union(enum(u3)) {
    gbl: []const u8 = 0,
    tmp: []const u8 = 1,
    int: i64 = 2,
    sin: f64 = 3,
    dou: f64 = 4,
};

pub const InstUn = enum {
    exts,
    copy,
};

pub const InstBi = enum {
    add,
    sub,
    mul,
    ugt,
};

