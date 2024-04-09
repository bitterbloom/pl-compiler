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
                                .uint32 => std.debug.assert(false),
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
                };
                try emit.emitBlockEndJnz(
                    out, val,
                    program.insts.get(branch.block_t).block.ident,
                    program.insts.get(branch.block_f).block.ident,
                );
            },
            .ret => |ret| {
                const val: qbe.Val = switch (ret.expr) {
                    .local => |index| switch (index) {
                        _ => qbe.Val{
                            .tmp = program.insts.get(index).local.ident,
                        },
                    },
                    .global => @panic("todo"),
                    .uint32 => |uint32| qbe.Val{.int = uint32},
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
                            else => {
                                std.debug.panic("A local argument should only either be a local or param instruction", .{});
                            }
                        };

                        const arg_ty = if (tyToQbeAbi(program, local.ty)) |ty| ty else @panic("todo");

                        try emit.emitInstCallArg(out, arg_ty, .{.tmp = local.ident});
                    },
                    .global => |index| {
                        try emit.emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = mod.globals.get(index).ident});
                    },
                    .uint32 => @panic("todo"),
                }
            },
            .internal => |_| {
                switch (arg) {
                    .local => |index| {
                        const local: ir.Inst.Local = switch (program.insts.get(index).*) {
                            .param => |param| param,
                            .local => |local| local,
                            else => {
                                std.debug.panic("A local argument should only either be a local or param instruction", .{});
                            }
                        };

                        const arg_ty = if (tyToQbeAbi(program, local.ty)) |ty| ty else @panic("todo");

                        try emit.emitInstCallArg(out, arg_ty, .{.tmp = local.ident});
                    },
                    .global => |index| {
                        try emit.emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = mod.globals.get(index).ident});
                    },
                    .uint32 => @panic("todo"),
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

test "ret 0" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    //  module _ {
    //      export def $main(): UInt32 {
    //          return 0
    //      }
    //  }

    var program: ir.Program = .{
        .tys = .{.list = try std.ArrayList(ir.Ty).initCapacity(alloc, 100)},
        .mods = .{.list = try std.ArrayList(ir.Mod).initCapacity(alloc, 1)},
        .insts = .{.list = try std.ArrayList(ir.Inst).initCapacity(alloc, 100)},
    };
    defer {
        program.tys.list.deinit();
        program.mods.list.deinit();
        program.insts.list.deinit();
    }

    const mod = program.mods.list.addOneAssumeCapacity();
    mod.* = ir.Mod{
        .ident = "Main",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 100)},
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 1)},
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    const main: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .exported = true,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
    }};
    defer main.internal.indices.deinit();

    const ret: ir.InstList.Index = program.insts.addNoAlloc(.{.ret = ir.Inst.Ret{
        .expr = .{.uint32 = 0},
    }});
    main.internal.indices.addOneAssumeCapacity().* = ret;

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    var out = array_list.writer();

    const expected_qbe =
      \\export function w $main() {
      \\@.0
      \\    ret 0
      \\}
      \\
    ;

    try emitQbe(out, program);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "";

    try test_util.testCompileAndRun(expected_exe, source);
}

test "hello world" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    //  module _ {
    //      let $fmt: []UInt8 = "Hello, World!\0"
    //      extern $puts([]UInt8): UInt32
    //      export def $main(): UInt32 {
    //          var %r: UInt32
    //          %r = $puts($fmt)
    //          return %r
    //      }
    //  }

    var program: ir.Program = .{
        .tys = .{.list = try std.ArrayList(ir.Ty).initCapacity(alloc, 2)},
        .mods = .{.list = try std.ArrayList(ir.Mod).initCapacity(alloc, 1)},
        .insts = .{.list = try std.ArrayList(ir.Inst).initCapacity(alloc, 100)},
    };
    defer {
        program.tys.list.deinit();
        program.mods.list.deinit();
        program.insts.list.deinit();
    }

    const arr_of_u8_ty: *ir.Ty = program.tys.list.addOneAssumeCapacity();
    arr_of_u8_ty.* = ir.Ty{.arr_ty = ir.TyList.Index.uint8_ty};
    const ptr_to_arr_of_u8_ty: *ir.Ty = program.tys.list.addOneAssumeCapacity();
    ptr_to_arr_of_u8_ty.* = ir.Ty{.ptr_ty = program.tys.indexOf(arr_of_u8_ty)};

    const mod: *ir.Mod = program.mods.list.addOneAssumeCapacity();
    mod.* = ir.Mod{
        .ident = "Main",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 2) },
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 2) },
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    const fmt: *ir.Global = mod.globals.list.addOneAssumeCapacity();
    fmt.* = ir.Global{
        .ident = "fmt",
        .ty = program.tys.indexOf(arr_of_u8_ty),
        .expr = .{.str = "Hello, World!"},
    };

    const puts: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    puts.* = ir.Func{.external = .{
        .ident = "puts",
        .ret_ty = .uint32_ty,
        .params = &.{program.tys.indexOf(ptr_to_arr_of_u8_ty)},
    }};

    const main: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .exported = true,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 3),
    }};
    defer main.internal.indices.deinit();

    const r = program.insts.addNoAlloc(.{.local = ir.Inst.Local{
        .ident = "r",
        .ty = .uint32_ty,
        .data = ir.Inst.Local.Data.d_tmp,
    }});
    main.internal.indices.addOneAssumeCapacity().* = r;

    const call = program.insts.addNoAlloc(.{.set = ir.Inst.Set{
        .target = r,
        .expr = .{.call1 = .{
            .index = mod.funcs.indexOf(puts),
            // TODO: Should global data always be used as addresses?
            .arg = ir.Inst.Expr.Arg{.global = mod.globals.indexOf(fmt)},
        }},
    }});
    main.internal.indices.addOneAssumeCapacity().* = call;

    const ret = program.insts.addNoAlloc(.{.ret = ir.Inst.Ret{
        .expr = .{.local = r},
    }});
    main.internal.indices.addOneAssumeCapacity().* = ret;

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    const out = array_list.writer();

    const expected_qbe =
      \\data $fmt = { b "Hello, World!", b 0, }
      \\export function w $main() {
      \\@.0
      \\    %r =w call $puts(l $fmt, )
      \\    ret %r
      \\}
      \\
    ;

    try emitQbe(out, program);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "Hello, World!\n";

    try test_util.testCompileAndRun(expected_exe, source);
}

test "factorial" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    //  module _ {
    //      data $fmt = "%u\n\0"
    //      extern $printf([]UInt8, ...): UInt32
    //      def $factorial(%n: UInt32): UInt32 {
    //          var %r: UInt32
    //          %r = 1
    //          jump @while
    //          @while {
    //              let %c: UInt32
    //              %c = greater %n, 1
    //              branch @body if %c else @end
    //          }
    //          @body {
    //              %r = mul_ub %r, %n
    //              %n = sub_ub %n, 1
    //              jump @while
    //          }
    //          @end {
    //              ret %r
    //          }
    //      }
    //      export def $main(): UInt32 {
    //          var %n: UInt32
    //          %n = 8
    //          var %f: UInt32
    //          %f = call $factorial(%n)
    //          var %r: UInt32
    //          %r = call $printf(%fmt, %f)
    //          ret %r
    //      }
    //  }

    //  data $fmt = { b "%u", b 0, }
    //  function w $factorial(w %n, ) {
    //  @.0
    //      %r =w copy 1
    //      jmp @while
    //  @while
    //      %c =w cugtw %n, 1
    //      jnz %c, @body, @end
    //  @body
    //      %r =w mul %r, %n
    //      %n =w sub %n, 1
    //      jmp @while
    //  @end
    //      ret %r
    //  }
    //  export function w $main() {
    //  @.0
    //      %n =w copy 8
    //      %f =w call $factorial(w %n, )
    //      %r =w call $printf(l $fmt, w %f, )
    //      ret %r
    //  }

    var program: ir.Program = .{
        .tys = .{.list = try std.ArrayList(ir.Ty).initCapacity(alloc, 100)},
        .mods = .{.list = try std.ArrayList(ir.Mod).initCapacity(alloc, 100)},
        .insts = .{.list = try std.ArrayList(ir.Inst).initCapacity(alloc, 100)},
    };
    defer {
        program.tys.list.deinit();
        program.mods.list.deinit();
        program.insts.list.deinit();
    }

    const arr_of_u8_ty = program.tys.list.addOneAssumeCapacity();
    arr_of_u8_ty.* = .{.arr_ty = ir.TyList.Index.uint8_ty};
    const ptr_to_arr_of_u8_ty: *ir.Ty = program.tys.list.addOneAssumeCapacity();
    ptr_to_arr_of_u8_ty.* = .{.ptr_ty = program.tys.indexOf(arr_of_u8_ty)};

    const mod: *ir.Mod = program.mods.list.addOneAssumeCapacity();
    mod.* = ir.Mod{
        .ident = "Main",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 100)},
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 100)},
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    // data $fmt = "%u\n\0"
    const fmt: *ir.Global = mod.globals.list.addOneAssumeCapacity();
    fmt.* = ir.Global{
        .ident = "fmt",
        .ty = program.tys.indexOf(arr_of_u8_ty),
        .expr = .{.str = "%u" },
    };

    // extern $printf([]UInt8, ...): UInt32
    const printf: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    printf.* = ir.Func{.external = .{
        .ident = "printf",
        .ret_ty = .uint32_ty,
        .params = &.{program.tys.indexOf(ptr_to_arr_of_u8_ty)},
    }};

    // def $factorial(...): UInt32
    const factorial: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    factorial.* = ir.Func{.internal = .{
        .ident = "factorial",
        .ret_ty = .uint32_ty,
        .exported = false,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
    }};
    defer factorial.internal.indices.deinit();

    // (%n: UInt32)
    const f_n: *ir.Inst = program.insts.list.addOneAssumeCapacity();
    f_n.* = .{
        .param = ir.Inst.Local{
            .ident = "n",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_var,
        }
    };
    const f_n_index = program.insts.indexOf(f_n);
    factorial.internal.indices.addOneAssumeCapacity().* = f_n_index;

    // var %r: UInt32
    const f_r_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "r",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_var,
        }
    });
    factorial.internal.indices.addOneAssumeCapacity().* = f_r_index;

    // %r = 1
    factorial.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = f_r_index,
            .expr = .{.uint32 = 1},
        }
    });

    // jump @while
    const f_jump_while_index = program.insts.addUninitNoAlloc();
    // Initialize inst after adding block
    factorial.internal.indices.addOneAssumeCapacity().* = f_jump_while_index;

    // @while
    const f_while: *ir.Inst = program.insts.list.addOneAssumeCapacity();
    f_while.* = .{
        .block = ir.Inst.Block{
            .ident = "while",
            .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
        }
    };
    defer f_while.block.indices.deinit();
    const f_while_index = program.insts.indexOf(f_while);
    factorial.internal.indices.addOneAssumeCapacity().* = f_while_index;

    program.insts.init(f_jump_while_index, .{.jump = ir.Inst.Jump{
        .block = f_while_index,
    }});

    // let %c: UInt32
    const f_c_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "c",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_let,
        }
    });
    f_while.block.indices.addOneAssumeCapacity().* = f_c_index;

    // %c = greater %n, 1
    f_while.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = f_c_index,
            .expr = .{.bi_op = .{
                .op = ir.Inst.Expr.BiOp.Op.ugt,
                .lhs = .{.local = f_n_index},
                .rhs = .{.uint32 = 1},
            }},
        }
    });

    // branch @body if %c else @end
    const f_branch_index = program.insts.addUninitNoAlloc();
    // Initialize inst after adding blocks
    f_while.block.indices.addOneAssumeCapacity().* = f_branch_index;

    // @body
    const f_body: *ir.Inst = program.insts.list.addOneAssumeCapacity();
    f_body.* = .{
        .block = ir.Inst.Block{
            .ident = "body",
            .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
        }
    };
    defer f_body.block.indices.deinit();
    const f_body_index = program.insts.indexOf(f_body);
    factorial.internal.indices.addOneAssumeCapacity().* = f_body_index;

    // %r = mul_ub %r, %n
    f_body.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = f_r_index,
            .expr = .{.bi_op = .{
                .op = ir.Inst.Expr.BiOp.Op.mul,
                .lhs = .{.local = f_r_index},
                .rhs = .{.local = f_n_index},
            }},
        }
    });

    // %n = sub_ub %n, 1
    f_body.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = f_n_index,
            .expr = .{.bi_op = .{
                .op = ir.Inst.Expr.BiOp.Op.sub,
                .lhs = .{.local = f_n_index},
                .rhs = .{.uint32 = 1},
            }},
        }
    });

    // jump @while
    f_body.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .jump = ir.Inst.Jump{.block = f_while_index},
    });

    // @end
    const f_end: *ir.Inst = program.insts.list.addOneAssumeCapacity();
    f_end.* = .{
        .block = ir.Inst.Block{
            .ident = "end",
            .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
        }
    };
    defer f_end.block.indices.deinit();
    const f_end_index = program.insts.indexOf(f_end);
    factorial.internal.indices.addOneAssumeCapacity().* = f_end_index;

    // ret %r
    f_end.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .ret = ir.Inst.Ret{.expr = .{.local = f_r_index}},
    });

    program.insts.init(f_branch_index, .{.branch = ir.Inst.Branch{
        .cond = .{.local = f_c_index},
        .block_t = f_body_index,
        .block_f = f_end_index,
    }});
    
    // export def $main(): UInt32
    const main: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .exported = true,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
    }};
    defer main.internal.indices.deinit();

    // var %n: UInt32
    const main_n_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "n",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_let,
        }
    });
    main.internal.indices.addOneAssumeCapacity().* = main_n_index;

    // %n = 8
    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_n_index,
            .expr = .{.uint32 = 8},
        }
    });

    // var %f: UInt32
    const main_f_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "f",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_var,
        }
    });
    main.internal.indices.addOneAssumeCapacity().* = main_f_index;

    // %f = call $factorial(%n)
    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_f_index,
            .expr = .{.call1 = .{
                .index = mod.funcs.indexOf(factorial),
                .arg = ir.Inst.Expr.Arg{.local = main_n_index},
            }},
        }
    });

    // var %r: UInt32
    const main_r_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "r",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_var,
        }
    });

    // %r = call $printf(%fmt, %f)
    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_r_index,
            .expr = .{.call2 = .{
                .index = mod.funcs.indexOf(printf),
                .arg0 = ir.Inst.Expr.Arg{.global = mod.globals.indexOf(fmt)},
                .arg1 = ir.Inst.Expr.Arg{.local = main_f_index},
            }},
        }
    });

    // ret %r
    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .ret = ir.Inst.Ret{.expr = .{.local = main_r_index}},
    });

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    const out = array_list.writer();

    const expected_qbe =
      \\data $fmt = { b "%u", b 0, }
      \\function w $factorial(w %n, ) {
      \\@.0
      \\    %r =w copy 1
      \\    jmp @while
      \\@while
      \\    %c =w cugtw %n, 1
      \\    jnz %c, @body, @end
      \\@body
      \\    %r =w mul %r, %n
      \\    %n =w sub %n, 1
      \\    jmp @while
      \\@end
      \\    ret %r
      \\}
      \\export function w $main() {
      \\@.0
      \\    %n =w copy 8
      \\    %f =w call $factorial(w %n, )
      \\    %r =w call $printf(l $fmt, w %f, )
      \\    ret %r
      \\}
      \\
    ;

    try emitQbe(out, program);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);
    
    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "40320";

    try test_util.testCompileAndRun(expected_exe, source);
}

