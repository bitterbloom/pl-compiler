const std = @import("std");
const ir = @import("types.zig");
const qbe = @import("../qbe/types.zig");
const emit = @import("../qbe/emit.zig");

pub fn emitQbe(out: anytype, program: ir.Program) !void {
    var buffer: [1024]u8 = undefined;
    var str_alloc = std.heap.FixedBufferAllocator.init(&buffer);

    for (program.mods.list.items) |mod| {
        for (mod.globals.list.items) |globals| {
            switch (globals.ty) {
                .void_ty, .uint8_ty, .uint32_ty => @panic("todo"),
                _ => |index| switch (program.tys.get(index).*) {
                    .arr_ty => |of| {
                        if (of == .uint8_ty) {
                            try emit.emitDataStr(out, globals.ident, switch (globals.expr) {
                                .uint32 => unreachable,
                                .str => |str| str,
                            });
                        }
                    },
                    else => @panic("todo"),
                },
            }
        }

        for (mod.funcs.list.items) |item| switch (item) {
            .external => {}, // We don't need to emit anything for external functions
            .internal => |func| {
                const ret_ty = tyToQbeAbi(&program, func.ret_ty);
                var next_unnamed_label: u32 = 0;
                //var next_unnamed_variable = 0;

                try emit.emitFuncBegin(out, func.ident, ret_ty, &.{}, func.exported);

                try emit.emitBlockBegin(out, try std.fmt.allocPrint(str_alloc.allocator(), ".{d}", .{next_unnamed_label}));
                str_alloc.reset();

                for (func.insts.list.items) |inst| {
                    switch (inst) {
                        .local => |local| {
                            try emit.emitInstUn(out, qbe.InstUn.copy, local.ident, tyToQbeBase(local.ty), qbe.Val{.int = 0});
                        },
                        .set => |set| switch (set.expr) {
                            .local => @panic("todo"),
                            .constant => @panic("todo"),
                            .call0 => @panic("todo"),
                            .call1 => |call| {
                                var ident: []const u8 = undefined;
                                var ty: ir.TyList.Index = undefined;

                                switch (set.target) {
                                    .local => |index| {
                                        var local = func.insts.get(index).*.local;
                                        ident = local.ident;
                                        ty = local.ty;
                                    },
                                }

                                switch (mod.funcs.get(call.index).*) {
                                    .external => |external| {
                                        var arg: qbe.Arg = undefined;

                                        switch (call.arg) {
                                            .local => @panic("todo"),
                                            .global => |index| {
                                                arg.abi_ty = .{.subw_ty = .long};
                                                arg.val = .{.gbl = mod.globals.get(index).*.ident};
                                            },
                                        }

                                        try emit.emitInstCall(out, ident, tyToQbeAbi(null, ty), external.ident, &.{arg});
                                    },
                                    .internal => @panic("todo"),
                                }
                            },
                        },
                        .ret => |ret| {
                            const val: qbe.Val = switch (ret.expr) {
                                .local => |index| switch (index) {
                                    _ => .{.tmp = func.insts.get(index).*.local.ident},
                                },
                                .constant => |constant| switch (constant) {
                                    .uint32 => |i| .{.int = i},
                                    .str => unreachable,
                                },
                                .call0 => @panic("todo"),
                                .call1 => @panic("todo"),
                            };
                            try emit.emitBlockEndRet(out, val);
                        },
                    }
                }

                try emit.emitFuncEnd(out);
            },
        };
    }
}

pub fn tyToQbeAbi(program: ?*const ir.Program, index: ir.TyList.Index) qbe.AbiTy {
    return switch (index) {
        .void_ty => .{.none_env = void{}},
        .uint8_ty => .{.subw_ty = .u_byte},
        .uint32_ty => .{.subw_ty = .word},
        _ => {
            _ = program;
            @panic("type from TyList is not implemented");
        },
    };
}

pub fn tyToQbeBase(index: ir.TyList.Index) qbe.BaseTy {
    return switch (index) {
        .void_ty, .uint8_ty => std.debug.panic("{} is not a base type", .{index}),
        .uint32_ty => .word,
        _ => @panic("type from TyList is not implemented"),
    };
}

test "ret 0" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    var program: ir.Program = .{
        .tys = .{.list = std.ArrayList(ir.Ty).init(alloc)},
        .mods = .{.list = try std.ArrayList(ir.Mod).initCapacity(alloc, 1)},
    };
    defer {
        program.tys.list.deinit();
        program.mods.list.deinit();
    }

    var mod = program.mods.list.addOneAssumeCapacity();
    mod.* = ir.Mod{
        .ident = "Main",
        .globals = .{.list = std.ArrayList(ir.Global).init(alloc)},
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 1)},
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    var main = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .params = &.{},
        .exported = true,
        .insts = .{.list = try std.ArrayList(ir.Inst).initCapacity(alloc, 1)},
    }};
    defer main.internal.insts.list.deinit();

    main.internal.insts.list.appendSliceAssumeCapacity(&.{
        .{.ret = .{.expr = .{.constant = .{.uint32 = 0}}}},
    });

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

    var program: ir.Program = .{
        .tys = .{.list = try std.ArrayList(ir.Ty).initCapacity(alloc, 2)},
        .mods = .{.list = try std.ArrayList(ir.Mod).initCapacity(alloc, 1)},
    };
    defer {
        program.tys.list.deinit();
        program.mods.list.deinit();
    }

    const arr_of_u8_ty = program.tys.list.addOneAssumeCapacity();
    arr_of_u8_ty.* = .{.arr_ty = ir.TyList.Index.uint8_ty};
    const ptr_to_arr_of_u8_ty: *ir.Ty = program.tys.list.addOneAssumeCapacity();
    ptr_to_arr_of_u8_ty.* = .{.ptr_ty = program.tys.indexOf(arr_of_u8_ty)};

    var mod = program.mods.list.addOneAssumeCapacity();
    mod.* = ir.Mod{
        .ident = "Main",
        .globals = .{.list = try std.ArrayList(ir.Global).initCapacity(alloc, 2) },
        .funcs = .{.list = try std.ArrayList(ir.Func).initCapacity(alloc, 2) },
    };
    defer {
        mod.globals.list.deinit();
        mod.funcs.list.deinit();
    }

    var fmt = mod.globals.list.addOneAssumeCapacity();
    fmt.* = ir.Global{
        .ident = "fmt",
        .ty = program.tys.indexOf(arr_of_u8_ty),
        .expr = .{.str = "Hello, World!"},
    };

    var puts = mod.funcs.list.addOneAssumeCapacity();
    puts.* = ir.Func{.external = .{
        .ident = "puts",
        .ret_ty = .uint32_ty,
        .params = &.{program.tys.indexOf(ptr_to_arr_of_u8_ty)},
    }};

    var main = mod.funcs.list.addOneAssumeCapacity();
    main.* = ir.Func{.internal = .{
        .ident = "main",
        .ret_ty = .uint32_ty,
        .params = &.{},
        .exported = true,
        .insts = .{.list = try std.ArrayList(ir.Inst).initCapacity(alloc, 3)},
    }};
    defer main.internal.insts.list.deinit();

    var r = main.internal.insts.list.addOneAssumeCapacity();
    r.* = .{.local = .{
        .ident = "r",
        .ty = .uint32_ty,
        .data = ir.Inst.Local.Data.d_tmp,
    }};

    var call = main.internal.insts.list.addOneAssumeCapacity();
    call.* = .{.set = .{
        .target = .{.local = main.internal.insts.indexOf(r)},
        .expr = .{.call1 = .{
            .index = mod.funcs.indexOf(puts),
            .arg = ir.Inst.Expr.Arg{.global = mod.globals.indexOf(fmt)},
        }},
    }};

    var ret = main.internal.insts.list.addOneAssumeCapacity();
    ret.* = .{.ret = .{
        .expr = .{.local = main.internal.insts.indexOf(r)},
    }};

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    var out = array_list.writer();

    const expected_qbe =
      \\data $fmt = { b "Hello, World!", b 0, }
      \\export function w $main() {
      \\@.0
      \\    %r =w copy 0
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

