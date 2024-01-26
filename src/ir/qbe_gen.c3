module ir::qbe_gen;

import std::io;
import qbe;

/*
This module contains the IR to QBE translations.
*/

fn qbe::Program gen_program(/*ir::Mod mod*/) {
    // TODO: generate submodules
    qbe::Data dummy_fmt = Data{
        .name = "dummy_fmt",
        .literals = Literal[]{
            Literal{`"function not implemented"`, {qbe::TypeKind.B}},
            Literal{`0`, {qbe::TypeKind.B}},
        },
    };

    return {
        .datas = {dummy_fmt},
        .funcs = {gen_func({
            .name = "main",
            .exported = true,
            .args = {},
            .type = {
                .kind = ir::TypeKind.U8,
            },
            .temps = {},
            .insts = {},
        })},
    };
}

fn qbe::Func gen_func(ir::Func func) {
    // TODO: generate blocks
    qbe::Type qbe_type = gen_type(func.type);
    qbe::Temp[] qbe_args = gen_args(func.args);
    return {
        .name = func.name,
        .exported = func.exported,
        .type = qbe_type,
        .args = qbe_args,
        .blocks = Block[]{
            Block{
                .name = "entry",
                .insts = Inst[]{
                    Inst{
                        .kind = InstKind.TEMP,
                        .name = "a",
                        .temp_type = {qbe::TypeKind.W},
                        .temp_expr = Expr{
                            .kind = ExprKind.CALL,
                            .call_name = "puts",
                            .args = {
                                Value{
                                    .kind = ValueKind.DATA,
                                    .data = dummy_fmt,
                                },
                            },
                        },
                    },
                    Inst{
                        .kind = InstKind.RET,
                        .ret_value = Value{
                            .kind = ValueKind.LITERAL,
                            .literal = Literal{
                                .literal = "0",
                                .type = qbe_type,
                            },
                        },
                    },
                },
            },
        },
    };
}

fn qbe::Type gen_type(ir::Type type) {
    // TODO: generate types
    return {qbe::TypeKind.W};
}

fn qbe::Temp[] gen_args(ir::Temp[] args) {
    // TODO: generate args
    return Temp[]{};
}

