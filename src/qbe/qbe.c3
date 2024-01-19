module qbe;
import std::io;
import std::io::file;

// Rename some of these to make them match the BNF of QBE IR

enum TypeKind {
    N, // no type
    B, //  8-bit integer
    H, // 16-bit integer
    W, // 32-bit integer
    L, // 64-bit integer
    S, // 32-bit floating point
    D, // 64-bit floating point
}

struct Type {
    TypeKind kind;
}

struct Literal {
    String literal;
    Type type;
}

// Rename Data to Global
struct Data {
    String name;
    Literal[] literals;
}

struct Temp {
    String name;
    Type type;
}

enum ValueKind {
    LITERAL,
    DATA,
    TEMP,
}

struct Value {
    ValueKind kind;
    union {
        Literal literal;
        Data data;
        Temp temp;
    }
}

enum ExprKind {
    LITERAL,
    DATA,
    TEMP,
    ADD,
    SUB,
    CALL,
}

struct Expr {
    ExprKind kind;
    union {
        struct { // LITERAL
            Literal literal;
        }
        struct { // DATA
            String data_name;
        }
        struct { // TEMP
            Temp temp;
        }
        struct { // ADD, SUB
            Value left;
            Value right;
        }
        struct { // CALL
            String call_name;
            Value[] args;
        }
    }
}

enum InstKind {
    TEMP,
    JNZ,
    RET,
}

struct Inst {
    InstKind kind;
    union {
        struct { // TEMP
            String name;
            Type temp_type;
            Expr temp_expr;
        }
        struct { // JNZ
            Value jnz_value;
            String not_zero_block;
            String if_zero_block;
        }
        struct { // RET
            Value ret_value;
        }
    }
}

struct Block {
    String name;
    Inst[] insts;
}

struct Func {
    String name;
    Type type;
    Temp[] args;
    Block[] blocks;
}

fn void! emit_hello_world(OutStream* out) {
    io::fprintn(out,
`# Define the string constant.
data $str = { b "hello world", b 0 }

export function w $main() {
@start
        # Call the puts function with $str as argument.
        %r =w call $puts(l $str)
        ret 0
}`
    )!;
}

fn void! emit_type(OutStream* out, Type type) {
    switch (type.kind) {
        case TypeKind.N: break;
        case TypeKind.B: io::fprint(out, "b")!;
        case TypeKind.H: io::fprint(out, "h")!;
        case TypeKind.W: io::fprint(out, "w")!;
        case TypeKind.L: io::fprint(out, "l")!;
        case TypeKind.S: io::fprint(out, "s")!;
        case TypeKind.D: io::fprint(out, "d")!;
    }
}

fn void! emit_literal(OutStream* out, Literal literal) {
    emit_type(out, literal.type)!;
    io::fprintf(out, " %s", literal.literal)!;
}

fn void! emit_data(OutStream* out, Data data) {
    io::fprintf(out, "data $%s = {", data.name)!;
    foreach (value : data.literals) {
        emit_literal(out, value)!;
        io::fprint(out, ", ")!;
    }
    io::fprintn(out, "}")!;
}

fn void! emit_temp(OutStream* out, Temp temp) {
    emit_type(out, temp.type)!;
    io::fprintf(out, " %%%s", temp.name)!;
}

fn void! emit_value(OutStream* out, Value value) {
    switch (value.kind) {
        case ValueKind.LITERAL:
            io::fprintf(out, "%s", value.literal.literal)!;

        case ValueKind.DATA:
            io::fprintf(out, "$%s", value.data.name)!;

        case ValueKind.TEMP:
            io::fprintf(out, "%%%s", value.temp.name)!;
    }
}

fn void! emit_typed_value(OutStream* out, Value value) {
    switch (value.kind) {
        case ValueKind.LITERAL:
            emit_literal(out, value.literal)!;

        case ValueKind.DATA:
            io::fprintf(out, "l $%s", value.data.name)!;
        
        case ValueKind.TEMP:
            emit_temp(out, value.temp)!;
    }
}

fn void! emit_expr(OutStream* out, Expr expr) {
    switch (expr.kind) {
        case ExprKind.LITERAL:
            emit_literal(out, expr.literal)!;

        case ExprKind.DATA:
            io::fprintf(out, "l $%s", expr.data_name)!;
        
        case ExprKind.TEMP:
            emit_temp(out, expr.temp)!;

        case ExprKind.ADD:
            io::fprint(out, "add ")!;
            emit_value(out, expr.left)!;
            io::fprint(out, ", ")!;
            emit_value(out, expr.right)!;

        case ExprKind.SUB:
            io::fprint(out, "sub ")!;
            emit_value(out, expr.left)!;
            io::fprint(out, ", ")!;
            emit_value(out, expr.right)!;

        case ExprKind.CALL:
            io::fprintf(out, "call $%s(", expr.call_name)!;
            foreach (arg : expr.args) {
                emit_typed_value(out, arg)!;
                io::fprint(out, ", ")!;
            }
            io::fprint(out, ")")!;
    }
}

fn void! emit_inst(OutStream* out, Inst inst) {
    switch (inst.kind) {
        case InstKind.TEMP:
            io::fprintf(out, "%%%s =", inst.name)!;
            emit_type(out, inst.temp_type)!;
            io::fprint(out, " ")!;
            emit_expr(out, inst.temp_expr)!;

        case InstKind.JNZ:
            io::fprint(out, "jnz ")!;
            emit_value(out, inst.jnz_value)!;
            io::fprintf(out, ", @%s, @%s", inst.not_zero_block, inst.if_zero_block)!;

        case InstKind.RET:
            io::fprint(out, "ret ")!;
            emit_value(out, inst.ret_value)!;
    }
}

fn void! emit_block(OutStream* out, Block block) {
    io::fprintf(out, "@%s\n", block.name)!;
    foreach (inst : block.insts) {
        io::fprint(out, "    ")!;
        emit_inst(out, inst)!;
        io::fprintn(out, "")!;
    }
}

fn void! emit_func(OutStream* out, Func func) {
    io::fprintf(out, "export function ")!;
    emit_type(out, func.type)!;
    io::fprintf(out, " $%s(", func.name)!;
    foreach (arg : func.args) {
        emit_temp(out, arg)!;
        io::fprint(out, ", ")!;
    }
    io::fprint(out, ") {\n")!;
    foreach (block : func.blocks) {
        emit_block(out, block)!;
    }
    io::fprintn(out, "}")!;
}

