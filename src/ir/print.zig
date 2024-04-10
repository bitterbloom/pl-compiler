const std = @import("std");
const ir = @import("./types.zig");

pub fn printProgram(out: anytype, program: ir.Program) !void {
    for (program.mods.list.items) |mod| {
        try out.writeAll("module ");

        if (mod.ident.len >= 1) {
            try out.writeAll(mod.ident);
            try out.writeAll(" ");
        }

        try out.writeAll("{\n");

        for (mod.globals.list.items) |global| {
            try printGlobal(out, program, global);
        }

        for (mod.funcs.list.items) |item| switch (item) {
            .external => |func| try printExternFunc(out, program, func),
            .internal => |func| try printFunc(out, program, mod, func),
        };

        try out.writeAll("}\n");
    }
}

fn printGlobal(out: anytype, program: ir.Program, global: ir.Global) !void {
    switch (global.ty) {
        .void_ty, .uint8_ty, .uint32_ty, .vararg_ty => @panic("todo"),
        _ => |index| switch (program.tys.get(index).*) {
            .arr_ty => |of| {
                if (of == .uint8_ty) {
                    switch (global.expr) {
                        .uint32 => std.debug.panic("A global of type array of UInt8 should not be initialized with a UInt32", .{}),
                        .str => |str| {
                            try printDataStr(out, global.ident, str);
                        },
                    }
                }
            },
            else => @panic("todo"),
        },
    }
}

fn printExternFunc(out: anytype, program: ir.Program, func: ir.Func.External) !void {
    try out.writeAll("    extern $");
    try out.writeAll(func.ident);
    try out.writeAll("(");
    for (func.params) |param| {
        try printType(out, program, param);
        try out.writeAll(", ");
    }
    try out.writeAll(") ");
    try printType(out, program, func.ret_ty);
    try out.writeAll("\n");
}

fn printFunc(out: anytype, program: ir.Program, mod: ir.Mod, func: ir.Func.Internal) !void {
    try out.writeAll("    ");
    if (func.exported) {
        try out.writeAll("export ");
    }
    try out.writeAll("def $");
    try out.writeAll(func.ident);
    try out.writeAll("(");

    var param_count: u32 = 0;
    params: for (func.indices.items) |index| {
        switch (program.insts.get(index).*) {
            .param => |param| {
                try printLocal(out, program, param);
                try out.writeAll(", ");
            },
            else => break :params,
        }
        param_count += 1;
    }

    try out.writeAll(") ");
    try printType(out, program, func.ret_ty);
    try out.writeAll(" {\n");

    for (func.indices.items[param_count..]) |index| {
        try printInst(out, program, mod, index, 2);
        try out.writeAll("\n");
    }

    try out.writeAll("    }\n");
}

fn printInst(out: anytype, program: ir.Program, mod: ir.Mod, index: ir.InstList.Index, nesting: u32) !void {
    for (0..nesting) |_| {
        try out.writeAll("    ");
    }
    switch (program.insts.get(index).*) {
        .param => std.debug.panic("Params should only be at the beginning of the function", .{}),
        .local => |local| {
            try printLocal(out, program, local);
        },
        .set => |set| {
            switch (program.insts.get(set.target).*) {
                .param => |param| {
                    try out.writeAll("%");
                    try out.writeAll(param.ident);
                },
                .local => |local| {
                    try out.writeAll("%");
                    try out.writeAll(local.ident);
                },
                else => @panic("todo"),
            }

            try out.writeAll(" = ");

            switch (set.expr) {
                .local => |local| {
                    try out.writeAll("%");
                    try out.writeAll(program.insts.get(local).local.ident);
                },
                .uint32 => |uint32| {
                    try out.print("{d}", .{uint32});
                },
                .bi_op => |bi_op| {
                    try out.writeAll(switch (bi_op.op) {
                        .add => "add_ub",
                        .sub => "sub_ub",
                        .mul => "mul_ub",
                        .ugt => "ugreater",
                    });
                    try out.writeAll(" ");

                    try printArg(out, program, mod, bi_op.lhs);
                    try out.writeAll(", ");
                    try printArg(out, program, mod, bi_op.rhs);
                },
                .call0 => |call0| {
                    try printCall(out, program, mod, call0.index, &.{});
                },
                .call1 => |call1| {
                    try printCall(out, program, mod, call1.index, &.{call1.arg});
                },
                .call2 => |call2| {
                    try printCall(out, program, mod, call2.index, &.{call2.arg0, call2.arg1});
                },
                .calln => |calln| {
                    var args: []const ir.Inst.Expr.Arg = undefined;
                    args.ptr = calln.args;
                    args.len = calln.arg_len;
                    try printCall(out, program, mod, calln.index, args);
                },
            }
        },
        .jump => |jump| {
            try out.writeAll("jump @");

            const block = program.insts.get(jump.block).block;
            try out.writeAll(block.ident);
        },
        .branch => |branch| {
            try out.writeAll("branch @");

            const block_t = program.insts.get(branch.block_t).block;
            try out.writeAll(block_t.ident);

            try out.writeAll(" if ");

            try printArg(out, program, mod, branch.cond);

            try out.writeAll(" else @");

            const block_f = program.insts.get(branch.block_f).block;
            try out.writeAll(block_f.ident);
        },
        .ret => |ret| {
            try out.writeAll("return ");
            try printArg(out, program, mod, ret.expr);
        },
        .block => |block| {
            try out.writeAll("@");
            try out.writeAll(block.ident);
            try out.writeAll(" {\n");
            for (block.indices.items) |item| {
                try printInst(out, program, mod, item, nesting + 1);
                try out.writeAll("\n");
            }
            for (0..nesting) |_| {
                try out.writeAll("    ");
            }
            try out.writeAll("}");
        },
    }
}

fn printLocal(out: anytype, program: ir.Program, local: ir.Inst.Local) !void {
    if (local.data.on_stack) {
        if (local.data.is_const)
            try out.writeAll("let ")
        else
            try out.writeAll("var ");
    }
    else {
        if (local.data.is_const)
            try out.writeAll("tmp ")
        else
            @panic("todo");
    }

    try out.writeAll("%");
    try out.writeAll(local.ident);
    try out.writeAll(" ");
    try printType(out, program, local.ty);
}

fn printCall(out: anytype, program: ir.Program, mod: ir.Mod, func: ir.FuncList.Index, args: []const ir.Inst.Expr.Arg) !void {
    try out.writeAll("$");
    switch (mod.funcs.get(func).*) {
        .external => |external| try out.writeAll(external.ident),
        .internal => |internal| try out.writeAll(internal.ident),
    }
    try out.writeAll("(");
    for (args) |arg| {
        try printArg(out, program, mod, arg);
        try out.writeAll(", ");
    }
    try out.writeAll(")");
}

fn printArg(out: anytype, program: ir.Program, mod: ir.Mod, arg: ir.Inst.Expr.Arg) !void {
    switch (arg) {
        .local => |index| {
            try out.writeAll("%");

            const local = switch (program.insts.get(index).*) {
                .local => |local| local,
                .param => |param| param,
                else => std.debug.panic("Expected local or param", .{}),
            };
            try out.writeAll(local.ident);
        },
        .global => |index| {
            try out.writeAll("$");

            const global = mod.globals.get(index).*;
            try out.writeAll(global.ident);
        },
        .uint32 => |uint32| {
            try out.print("{d}", .{uint32});
        },
        .vararg => {
            try out.writeAll("...");
        },
    }
}

fn printType(out: anytype, program: ir.Program, ty: ir.TyList.Index) !void {
    switch (ty) {
        .void_ty => try out.writeAll("Void"),
        .uint8_ty => try out.writeAll("UInt8"),
        .uint32_ty => try out.writeAll("UInt32"),
        .vararg_ty => try out.writeAll("..."),
        else => |index| switch (program.tys.get(index).*) {
            .arr_ty => |of| {
                try out.writeAll("[");
                try out.writeAll("]");
                try printType(out, program, of);
            },
            .ptr_ty => |to| {
                try out.writeAll("^");
                try printType(out, program, to);
            },
        },
    }
}

fn printDataStr(out: anytype, ident: []const u8, str: []const u8) !void {
    try out.writeAll("    def $");
    try out.writeAll(ident);
    try out.writeAll(" []UInt8 = \"");
    try out.writeAll(str);
    try out.writeAll("\\0\"\n");
}
