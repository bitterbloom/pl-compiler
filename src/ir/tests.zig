const std = @import("std");
const testing = std.testing;
const test_util = @import("../test_util.zig");

const ir = @import("./types.zig");
const to_qbe = @import("./to_qbe.zig");
const parse = @import("./parse.zig");
const print = @import("./print.zig");

test "tokenize" {
    var buffer: [100_000]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&buffer);
    //const alloc = testing.allocator;

    const string =
      \\module {
      \\    def $fmt []UInt8 = "Hello, World!\0"
      \\    extern $puts(^[]UInt8, ) UInt32
      \\    export def $main() UInt32 {
      \\        tmp %r UInt32
      \\        %r = $puts($fmt, )
      \\        return %r
      \\    }
      \\}
      \\
    ;

    const expected = [_]parse.TokenKind{
        .{.module    = void{}},
        .{.@"{"      = void{}},
        .{.def       = void{}},
        .{.gbl_id    = ""    },
        .{.@"["      = void{}},
        .{.@"]"      = void{}},
        .{.word      = ""    },
        .{.@"="      = void{}},
        .{.str       = ""    }, 
        .{.@"extern" = void{}},
        .{.gbl_id    = ""    },
        .{.@"("      = void{}},
        .{.word      = ""    },
        .{.@"["      = void{}},
        .{.@"]"      = void{}},
        .{.word      = ""    },
        .{.@","      = void{}},
        .{.@")"      = void{}},
        .{.word      = ""    },
        .{.@"export" = void{}},
        .{.def       = void{}},
        .{.gbl_id    = ""    },
        .{.@"("      = void{}},
        .{.@")"      = void{}},
        .{.word      = ""    },
        .{.@"{"      = void{}},
        .{.tmp       = void{}},
        .{.lcl_id    = ""    },
        .{.word      = ""    },
        .{.lcl_id    = ""    },
        .{.@"="      = void{}},
        .{.gbl_id    = ""    },
        .{.@"("      = void{}},
        .{.gbl_id    = ""    },
        .{.@","      = void{}},
        .{.@")"      = void{}},
        .{.word      = ""    },
        .{.lcl_id    = ""    },
        .{.@"}"      = void{}},
        .{.@"}"      = void{}},
        .{.eof       = void{}},
    };

    var tokens = try std.ArrayList(parse.Token).initCapacity(alloc.allocator(), 100);

    var tokenizer = parse.Tokenizer.init(string, alloc.allocator());
    try tokens.append(try tokenizer.tokenize());

    while (tokens.getLast().kind != parse.TokenKind.eof) {
        try tokens.append(try tokenizer.tokenize());
    }

    //try testing.expectEqualSlices(parse.Token, &expected, try tokens.toOwnedSlice());

    const result: []parse.Token = try tokens.toOwnedSlice();
    for (expected, result[0..expected.len], 0..) |e, r, i| {
        const e_name = @tagName(e);
        const r_name = @tagName(r.kind);
        if (!std.mem.eql(u8, e_name, r_name)) {
            std.debug.print("At i: {d}, expected: {s}, found: {s} from: {}\n", .{i, e_name, r_name, r});
            try testing.expect(false);
        }
    }

    if (expected.len != result.len) {
        std.debug.print("Expected length: {d}, found: {d}\n", .{expected.len, result.len});
        try testing.expect(false);
    }

    alloc.reset();
}

test "ret 0" {
    const alloc = testing.allocator;

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
        .ident = "",
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

    const expected_ir =
      \\module {
      \\    export def $main() UInt32 {
      \\        return 0
      \\    }
      \\}
      \\
    ;

    try print.printProgram(out, program);

    var actual_ir = try array_list.toOwnedSlice();
    defer alloc.free(actual_ir);

    try testing.expectEqualStrings(expected_ir, actual_ir);

    const expected_qbe =
      \\export function w $main() {
      \\@.0
      \\    ret 0
      \\}
      \\
    ;

    try to_qbe.emitQbe(out, program);

    var actual_qbe = try array_list.toOwnedSlice();
    defer alloc.free(actual_qbe);

    try testing.expectEqualStrings(expected_qbe, actual_qbe);

    const expected_exe = "";

    try test_util.testCompileAndRun(expected_exe, actual_qbe);
}

test "hello world" {
    const alloc = testing.allocator;

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
        .ident = "",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 2)},
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 2)},
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

    const expected_ir =
      \\module {
      \\    def $fmt []UInt8 = "Hello, World!\0"
      \\    extern $puts(^[]UInt8, ) UInt32
      \\    export def $main() UInt32 {
      \\        tmp %r UInt32
      \\        %r = $puts($fmt, )
      \\        return %r
      \\    }
      \\}
      \\
    ;

    try print.printProgram(out, program);

    var actual_ir = try array_list.toOwnedSlice();
    defer alloc.free(actual_ir);

    try testing.expectEqualStrings(expected_ir, actual_ir);

    const expected_qbe =
      \\data $fmt = { b "Hello, World!", b 0, }
      \\export function w $main() {
      \\@.0
      \\    %r =w call $puts(l $fmt, )
      \\    ret %r
      \\}
      \\
    ;

    try to_qbe.emitQbe(out, program);

    var actual_qbe = try array_list.toOwnedSlice();
    defer alloc.free(actual_qbe);

    try testing.expectEqualStrings(expected_qbe, actual_qbe);

    const expected_exe = "Hello, World!\n";

    try test_util.testCompileAndRun(expected_exe, actual_qbe);
}

test "factorial" {
    const alloc = testing.allocator;

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
        .ident = "",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 100)},
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 100)},
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    const fmt: *ir.Global = mod.globals.list.addOneAssumeCapacity();
    fmt.* = ir.Global{
        .ident = "fmt",
        .ty = program.tys.indexOf(arr_of_u8_ty),
        .expr = .{.str = "%u" },
    };

    const printf: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    printf.* = ir.Func{.external = .{
        .ident = "printf",
        .ret_ty = .uint32_ty,
        .params = &.{
            program.tys.indexOf(ptr_to_arr_of_u8_ty),
            ir.TyList.Index.vararg_ty,
        },
    }};

    const factorial: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    factorial.* = ir.Func{.internal = .{
        .ident = "factorial",
        .ret_ty = .uint32_ty,
        .exported = false,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
    }};
    defer factorial.internal.indices.deinit();

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

    const f_r_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "r",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_var,
        }
    });
    factorial.internal.indices.addOneAssumeCapacity().* = f_r_index;

    factorial.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = f_r_index,
            .expr = .{.uint32 = 1},
        }
    });

    const f_jump_while_index = program.insts.addUninitNoAlloc();
    // Initialize inst after adding block
    factorial.internal.indices.addOneAssumeCapacity().* = f_jump_while_index;

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

    const f_c_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "c",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_tmp,
        }
    });
    f_while.block.indices.addOneAssumeCapacity().* = f_c_index;

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

    const f_branch_index = program.insts.addUninitNoAlloc();
    // Initialize inst after adding blocks
    f_while.block.indices.addOneAssumeCapacity().* = f_branch_index;

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

    f_body.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .jump = ir.Inst.Jump{.block = f_while_index},
    });

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

    f_end.block.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .ret = ir.Inst.Ret{.expr = .{.local = f_r_index}},
    });

    program.insts.init(f_branch_index, .{.branch = ir.Inst.Branch{
        .cond = .{.local = f_c_index},
        .block_t = f_body_index,
        .block_f = f_end_index,
    }});
    
    const main: *ir.Func = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .exported = true,
        .indices = try std.ArrayList(ir.InstList.Index).initCapacity(alloc, 100),
    }};
    defer main.internal.indices.deinit();

    const main_n_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "n",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_tmp,
        }
    });
    main.internal.indices.addOneAssumeCapacity().* = main_n_index;

    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_n_index,
            .expr = .{.uint32 = 8},
        }
    });

    const main_f_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "f",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_tmp,
        }
    });
    main.internal.indices.addOneAssumeCapacity().* = main_f_index;

    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_f_index,
            .expr = .{.call1 = .{
                .index = mod.funcs.indexOf(factorial),
                .arg = ir.Inst.Expr.Arg{.local = main_n_index},
            }},
        }
    });

    const main_r_index = program.insts.addNoAlloc(.{
        .local = ir.Inst.Local{
            .ident = "r",
            .ty = .uint32_ty,
            .data = ir.Inst.Local.Data.d_tmp,
        }
    });
    main.internal.indices.addOneAssumeCapacity().* = main_r_index;

    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .set = ir.Inst.Set{
            .target = main_r_index,
            .expr = .{.calln = .{
                .index = mod.funcs.indexOf(printf),
                .arg_len = 3,
                .args = &[_]ir.Inst.Expr.Arg{
                    ir.Inst.Expr.Arg{.global = mod.globals.indexOf(fmt)},
                    ir.Inst.Expr.Arg{.vararg = void{}},
                    ir.Inst.Expr.Arg{.local = main_f_index},
                },
            }},
        }
    });

    main.internal.indices.addOneAssumeCapacity().* = program.insts.addNoAlloc(.{
        .ret = ir.Inst.Ret{.expr = .{.local = main_r_index}},
    });

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    const out = array_list.writer();

    const expected_ir =
      \\module {
      \\    def $fmt []UInt8 = "%u\0"
      \\    extern $printf(^[]UInt8, ..., ) UInt32
      \\    def $factorial(var %n UInt32, ) UInt32 {
      \\        var %r UInt32
      \\        %r = 1
      \\        jump @while
      \\        @while {
      \\            tmp %c UInt32
      \\            %c = ugreater %n, 1
      \\            branch @body if %c else @end
      \\        }
      \\        @body {
      \\            %r = mul_ub %r, %n
      \\            %n = sub_ub %n, 1
      \\            jump @while
      \\        }
      \\        @end {
      \\            return %r
      \\        }
      \\    }
      \\    export def $main() UInt32 {
      \\        tmp %n UInt32
      \\        %n = 8
      \\        tmp %f UInt32
      \\        %f = $factorial(%n, )
      \\        tmp %r UInt32
      \\        %r = $printf($fmt, ..., %f, )
      \\        return %r
      \\    }
      \\}
      \\
    ;

    try print.printProgram(out, program);

    var actual_ir = try array_list.toOwnedSlice();
    defer alloc.free(actual_ir);

    try testing.expectEqualStrings(expected_ir, actual_ir);

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
      \\    %r =w call $printf(l $fmt, ..., w %f, )
      \\    ret %r
      \\}
      \\
    ;

    try to_qbe.emitQbe(out, program);

    var actual_qbe = try array_list.toOwnedSlice();
    defer alloc.free(actual_qbe);
    
    try testing.expectEqualStrings(expected_qbe, actual_qbe);

    const expected_exe = "40320";

    try test_util.testCompileAndRun(expected_exe, actual_qbe);
}

