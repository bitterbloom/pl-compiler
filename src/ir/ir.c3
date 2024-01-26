module ir;

/*
This module contains the data types for the IR.

Grammar:
    <___> = placeholder
    [___]? = optional
    [___], = repeatable (comma separated)
*/

/*
module <Module> {
    <...>
}
*/
struct Mod {
    String name;
    //Data[] datas;
    Func[] funcs;
    Mod[] submods;
}

/*
function $<function>([%<arg>: <Type>], ) -> <Type> {
    <...>
}
*/
struct Func {
    String name;
    bool exported;
    Temp[] args;
    Type type;
    Temp[] temps;
    Inst[] insts;
}

/*
@<block> {
    <...>
}
*/
struct Block {
    Temp[] temps;
    Inst[] insts;
}

struct Inst {
    InstKind kind;
    union {
        struct { // SET
            Temp*  s_temp;
            Expr   s_expr;
        }
        struct { // JUMP
            Temp*  j_temp;  // null if unconditional
            Block* j_block;
            Block* j_else;  // null if unconditional or no else
        }
        struct { // RETURN
            Expr   r_expr;
        }
        struct { // BLOCK
            Block* b_block;
        }
    }
}

enum InstKind {
    SET,    // %<temp> = <expr>
    JUMP,   // jump @<block> [if %<temp> [else @<block>]?]?
    RETURN, // return <expr>
    BLOCK,  // @<block> { <...> }
}

struct Expr {
    ExprKind kind;
    union {
        struct { // VALUE
            Value v_value;
        }
        struct { // CALL
            Func*  c_func;
            Expr[] c_args;
            bool   c_pure;
        }
        struct { // BINARY
            Value* b_left;
            Value* b_right;
            Oper   b_oper;
        }
    }
}

enum ExprKind {
    VALUE,  // <value>
    CALL,   // <func>(<expr>, ...)
    BINARY, // <expr> <oper> <expr>
}

enum Oper {
    ADD_UNDEF, // +undef // overflow is undefined
}

struct Value {
    ValueKind kind;
    union {
        struct { // TEMP
            Temp* t_temp;
        }
        struct { // CONST
            ulong c_const;
        }
    }
}

enum ValueKind {
    TEMP,   // %<temp>
    CONST,  // <const>
}

struct Temp {
    String name;
    Type type;
}

enum TypeKind {
    U8, U16, U32,
}

struct Type {
    TypeKind kind;
}

