const std = @import("std");
const ir = @import("types.zig");
const qbe = @import("../qbe/types.zig");
const emit = @import("../qbe/emit.zig");

pub fn emitQbe(out: anytype, program: ir.Program) !void {
    var buffer: [1024]u8 = undefined;
    var temp_alloc = std.heap.FixedBufferAllocator.init(&buffer);

    for (program.mods.list.items) |mod| {
        for (mod.globals.list.items) |global| {
            switch (global.ty) {
                .void_ty, .uint8_ty, .uint32_ty, .vararg_ty => @panic("todo"),
                _ => |index| switch (program.tys.get(index).*) {
                    .arr_ty => |of| {
                        if (of == .uint8_ty) {
                            switch (global.expr) {
                                .uint32 => std.debug.panic("A global of type array of UInt8 should not be initialized with a UInt32", .{}),
                                .str => |str| {
                                    try emit.emitDataStr(out, global.ident, str);
                                },
                            }
                        }
                    },
                    else => @panic("todo"),
                },
            }
        }

        for (mod.funcs.list.items) |item| switch (item) {
            .external => {}, // We don't need to emit anything for external functions
            .internal => |func| {
                if (tyToQbeAbi(&program, func.ret_ty)) |ret_ty|
                    try emit.emitFuncSigBegin(out, func.ident, ret_ty, func.exported)
                else
                    try emit.emitFuncVoidSigBegin(out, func.ident, func.exported);

                var param_count: u32 = 0;

                // The params of a function will always be the first instructions in the
                // index list.
                params: for (func.indices.items) |index| {
                    switch (program.insts.get(index).*) {
                        .param => |param| {
                            if (tyToQbeAbi(&program, param.ty)) |ty| {
                                try emit.emitFuncParam(out, param.ident, ty);
                                param_count += 1;
                            } else
                                std.debug.assert(false);
                        },
                        else => break :params,
                    }
                }

                try emit.emitFuncSigEnd(out);

                var next_unnamed_label: u32 = 0;
                //var next_unnamed_variable = 0;

                try emit.emitBlockBegin(out, try std.fmt.allocPrint(temp_alloc.allocator(), ".{d}", .{next_unnamed_label}));
                temp_alloc.reset();

                try emitInsts(out, &program, &mod, func.indices.items[param_count..]);

                try emit.emitFuncEnd(out);
            },
        };
    }
}

fn emitInsts(out: anytype, program: *const ir.Program, mod: *const ir.Mod, indices: []ir.InstList.Index) !void {
    for (indices) |inst| {
        switch (program.insts.get(inst).*) {
            .local, .param => {
                // It should be the programmers responsibility to make sure that a local
                // is assigned a value before use.
                // TODO: Maybe emit an error if this is not the case?
            },
            .set => |set| switch (set.expr) {
                .local => @panic("todo"),
                .uint32 => |uint32| {
                    const target: ir.Inst.Local = switch (program.insts.get(set.target).*) {
                        .param => |param| param,
                        .local => |local| local,
                        else => std.debug.panic(
                            "A local argument should only either be a local or param instruction", .{}
                        ),
                    };

                    const ident: []const u8 = target.ident;
                    const set_ty: qbe.BaseTy = tyToQbeBase(target.ty);

                    try emit.emitInstUn(
                        out, ident, set_ty,
                        qbe.InstUn.copy,
                        qbe.Val{.int = uint32},
                    );                                      
                },
                .bi_op => |bi_op| {
                    const target: ir.Inst.Local = switch (program.insts.get(set.target).*) {
                        .param => |param| param,
                        .local => |local| local,
                        else => std.debug.panic(
                            "A local argument should only either be a local or param instruction", .{}
                        ),
                    };

                    const ident: []const u8 = target.ident;
                    const set_ty: qbe.BaseTy = tyToQbeBase(target.ty);

                    var op_ty: qbe.BaseTy = undefined;

                    var lhs: qbe.Val = undefined;

                    // TODO: Move these switch statements into a function
                    switch (bi_op.lhs) {
                        .local => |index| {
                            const local: ir.Inst.Local = switch (program.insts.get(index).*) {
                                .param => |param| param,
                                .local => |local| local,
                                else => std.debug.panic(
                                    "A local argument should only either be a local or param instruction", .{}
                                ),
                            };

                            op_ty = switch (local.ty) {
                                .uint32_ty => qbe.BaseTy.word,

                                .void_ty, .uint8_ty,
                                .vararg_ty => @panic("todo"),
                                _ => @panic("todo"),
                            };
                            lhs = qbe.Val{.tmp = local.ident};
                        },
                        .global => @panic("todo"),
                        .uint32 => @panic("todo"),
                        .vararg => std.debug.panic(
                            "Vararg should not be used as an argument to a binary operation", .{}
                        ),
                    }

                    const rhs: qbe.Val = switch (bi_op.rhs) {
                        .local => |index| switch (program.insts.get(index).*) {
                            .param => |param| qbe.Val{.tmp = param.ident},
                            .local => |local| qbe.Val{.tmp = local.ident},
                            else => std.debug.panic(
                                "A local argument should only either be a local or param instruction", .{}
                            ),
                        },
                        .global => @panic("todo"),
                        .uint32 => |uint32| qbe.Val{.int = uint32},
                        .vararg => std.debug.panic(
                            "Vararg should not be used as an argument to a binary operation", .{}
                        ),
                    };

                    const op: qbe.InstBi = switch (bi_op.op) {
                        .add => qbe.InstBi.add,
                        .sub => qbe.InstBi.sub,
                        .mul => qbe.InstBi.mul,
                        .ugt => qbe.InstBi.ugt,
                    };

                    try emit.emitInstBi(out, ident, set_ty, op, op_ty, lhs, rhs);
                },
                .call0 => @panic("todo"),
                .call1 => |call| {
                    const target: ir.Inst.Local = switch (program.insts.get(set.target).*) {
                        .param => |param| param,
                        .local => |local| local,
                        else => std.debug.panic(
                            "A local argument should only either be a local or param instruction", .{}
                        ),
                    };

                    const set_ident: []const u8 = target.ident;
                    const set_ty: ?qbe.AbiTy = tyToQbeAbi(null, target.ty);

                    const func: ir.Func = mod.funcs.get(call.index).*;
                    const func_ident: []const u8 = switch (func) {
                        .external => |external| external.ident,
                        .internal => |internal| internal.ident,
                    };

                    if (set_ty) |ty|
                        try emit.emitInstCallBegin(out, set_ident, ty, func_ident)
                    else // A set instruction cannot call a function that returns void.
                        std.debug.assert(false);

                    try emitArgs(out, program, mod, func, &.{call.arg});

                    try emit.emitInstCallEnd(out);
                },
                .call2 => |call| {
                    const target: ir.Inst.Local = switch (program.insts.get(set.target).*) {
                        .param => |param| param,
                        .local => |local| local,
                        else => std.debug.panic(
                            "A local argument should only either be a local or param instruction", .{}
                        ),
                    };

                    const set_ident: []const u8 = target.ident;
                    const set_ty: ?qbe.AbiTy = tyToQbeAbi(null, target.ty);

                    const func: ir.Func = mod.funcs.get(call.index).*;
                    const func_ident: []const u8 = switch (func) {
                        .external => |external| external.ident,
                        .internal => |internal| internal.ident,
                    };

                    if (set_ty) |ty|
                        try emit.emitInstCallBegin(out, set_ident, ty, func_ident)
                    else // A set instruction cannot call a function that returns void.
                        std.debug.assert(false);

                    try emitArgs(out, program, mod, func, &.{call.arg0, call.arg1});

                    try emit.emitInstCallEnd(out);
                },
                .calln => |call| {
                    const target: ir.Inst.Local = switch (program.insts.get(set.target).*) {
                        .param => |param| param,
                        .local => |local| local,
                        else => std.debug.panic(
                            "A local argument should only either be a local or param instruction", .{}
                        ),
                    };

                    const set_ident: []const u8 = target.ident;
                    const set_ty: ?qbe.AbiTy = tyToQbeAbi(null, target.ty);

                    const func: ir.Func = mod.funcs.get(call.index).*;
                    const func_ident: []const u8 = switch (func) {
                        .external => |external| external.ident,
                        .internal => |internal| internal.ident,
                    };

                    if (set_ty) |ty|
                        try emit.emitInstCallBegin(out, set_ident, ty, func_ident)
                    else // A set instruction cannot call a function that returns void.
                        std.debug.assert(false);

                    var args: []const ir.Inst.Expr.Arg = undefined;
                    args.ptr = call.args;
                    args.len = call.arg_len;

                    try emitArgs(out, program, mod, func, args);

                    try emit.emitInstCallEnd(out);
                },
            },
            .jump => |jump| {
                switch (program.insts.get(jump.block).*) {
                    .block => |block| {
                        try emit.emitBlockEndJmp(out, block.ident);
                    },
                    else => std.debug.assert(false),
                }
            },
            .branch => |branch| {
                const val: qbe.Val = switch (branch.cond) {
                    .local => |local| qbe.Val{
                        .tmp = program.insts.get(local).local.ident,
                    },
                    .global => @panic("todo"),
                    .uint32 => @panic("todo"),
                    .vararg => std.debug.panic(
                        "Vararg should not be used as an argument to a branch statement", .{}
                    ),
                };
                try emit.emitBlockEndJnz(
                    out, val,
                    program.insts.get(branch.block_t).block.ident,
                    program.insts.get(branch.block_f).block.ident,
                );
            },
            .ret => |ret| {
                const val: qbe.Val = switch (ret.expr) {
                    .local => |index| qbe.Val{
                        .tmp = program.insts.get(index).local.ident,
                    },
                    .global => @panic("todo"),
                    .uint32 => |uint32| qbe.Val{.int = uint32},
                    .vararg => std.debug.panic(
                        "Vararg should not be used as an argument to a return statement", .{}
                    ),
                };
                try emit.emitBlockEndRet(out, val);
            },
            .block => |block| {
                try emit.emitBlockBegin(out, block.ident);
                try emitInsts(out, program, mod, block.indices.items);
            }
        }
    }
}

fn emitArgs(out: anytype, program: *const ir.Program, mod: *const ir.Mod, func: ir.Func, args: []const ir.Inst.Expr.Arg) !void {
    // TODO: Verify that the argument types are correct
    for (args, 0..) |arg, i| {
        _ = i;

        switch (func) {
            .external => |_| {
                switch (arg) {
                    .local => |index| {
                        const local: ir.Inst.Local = switch (program.insts.get(index).*) {
                            .param => |param| param,
                            .local => |local| local,
                            else => std.debug.panic(
                                "A local argument should only either be a local or param instruction", .{}
                            ),
                        };

                        const arg_ty = if (tyToQbeAbi(program, local.ty)) |ty| ty else @panic("todo");

                        try emit.emitInstCallArg(out, arg_ty, .{.tmp = local.ident});
                    },
                    .global => |index| {
                        try emit.emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = mod.globals.get(index).ident});
                    },
                    .uint32 => @panic("todo"),
                    .vararg => {
                        try emit.emitInstCallArgElipsis(out);
                    },
                }
            },
            .internal => |_| {
                switch (arg) {
                    .local => |index| {
                        const local: ir.Inst.Local = switch (program.insts.get(index).*) {
                            .param => |param| param,
                            .local => |local| local,
                            else => std.debug.panic(
                                "A local argument should only either be a local or param instruction", .{}
                            ),
                        };

                        const arg_ty = if (tyToQbeAbi(program, local.ty)) |ty| ty else @panic("todo");

                        try emit.emitInstCallArg(out, arg_ty, .{.tmp = local.ident});
                    },
                    .global => |index| {
                        try emit.emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = mod.globals.get(index).ident});
                    },
                    .uint32 => @panic("todo"),
                    .vararg => {
                        try emit.emitInstCallArgElipsis(out);
                    },
                }
            }
        }
    }
}

fn tyToQbeAbi(program: ?*const ir.Program, index: ir.TyList.Index) ?qbe.AbiTy {
    return switch (index) {
        .void_ty => null,
        .uint8_ty => .{.subw_ty = .u_byte},
        .uint32_ty => .{.subw_ty = .word},
        .vararg_ty => @panic("todo"),
        _ => {
            _ = program;
            @panic("type from TyList is not implemented");
        },
    };
}

fn tyToQbeBase(index: ir.TyList.Index) qbe.BaseTy {
    return switch (index) {
        .void_ty, .uint8_ty, .vararg_ty => std.debug.panic("{} is not a base type", .{index}),
        .uint32_ty => .word,
        _ => @panic("type from TyList is not implemented"),
    };
}

