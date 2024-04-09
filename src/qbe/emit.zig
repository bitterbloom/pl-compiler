const std = @import("std");
const types = @import("types.zig");

// TODO: Add variants of comment emitting functions that uses format strings.

pub fn emitComment(out: anytype, comment: []const u8) !void {
    try out.writeAll("# ");
    try out.writeAll(comment);
    try out.writeAll("\n");
}

pub fn emitInstComment(out: anytype, comment: []const u8) !void {
    try out.writeAll("    # ");
    try out.writeAll(comment);
    try out.writeAll("\n");
}

pub fn emitDataStr(out: anytype, ident: []const u8, string: []const u8) !void {
    try out.writeAll("data ");
    try emitGblIdent(out, ident);
    try out.writeAll(" = { b \"");
    try out.writeAll(string);
    try out.writeAll("\", b 0, }\n");
}

pub fn emitFuncSigBegin(out: anytype, ident: []const u8, ret_ty: types.AbiTy, exported: bool) !void {
    if (exported) try out.writeAll("export ");
    try out.writeAll("function ");

    try emitAbiTy(out, ret_ty);
    try emitGblIdent(out, ident);

    try out.writeAll("(");
}

pub fn emitFuncVoidSigBegin(out: anytype, ident: []const u8, exported: bool) !void {
    if (exported) try out.writeAll("export ");
    try out.writeAll("function ");

    try emitGblIdent(out, ident);

    try out.writeAll("(");
}

pub fn emitFuncParam(out: anytype, ident: []const u8, abi_ty: types.AbiTy) !void {
    try emitAbiTy(out, abi_ty);
    try emitTmpIdent(out, ident);
    try out.writeAll(", ");
}

pub fn emitFuncParamEnv(out: anytype, ident: []const u8) !void {
    try out.writeAll("env ");
    try emitTmpIdent(out, ident);
    try out.writeAll(", ");
}

pub fn emitFuncParamElipsis(out: anytype) !void {
    try out.writeAll("...");
}

pub fn emitFuncSigEnd(out: anytype) !void {
    try out.writeAll(") {\n");
}

pub fn emitFuncEnd(out: anytype) !void {
    try out.writeAll("}\n");
}

pub fn emitBlockBegin(out: anytype, ident: []const u8) !void {
    try emitLblIdent(out, ident);
    try out.writeAll("\n");
}

pub fn emitBlockEndJmp(out: anytype, ident: []const u8) !void {
    try out.writeAll("    jmp ");
    try emitLblIdent(out, ident);
    try out.writeAll("\n");
}

pub fn emitBlockEndJnz(out: anytype, cond: types.Val, block_t: []const u8, block_f: []const u8) !void {
    try out.writeAll("    jnz ");
    try emitVal(out, cond);
    try out.writeAll(", ");
    try emitLblIdent(out, block_t);
    try out.writeAll(", ");
    try emitLblIdent(out, block_f);
    try out.writeAll("\n");
}

pub fn emitBlockEndRet(out: anytype, val: types.Val) !void {
    try out.writeAll("    ret ");
    try emitVal(out, val);
    try out.writeAll("\n");
}

pub fn emitBlockEndRetVoid(out: anytype) !void {
    try out.writeAll("    ret\n");
}

pub fn emitInstUn(out: anytype, tmp_ident: []const u8, tmp_type: types.BaseTy, inst: types.InstUn, arg: types.Val) !void {
    try out.writeAll("    ");
    try emitTmpIdent(out, tmp_ident);
    try out.writeAll(" =");
    try emitBaseTy(out, tmp_type);

    switch (inst) {
        .exts => try out.writeAll("exts "),
        .copy => try out.writeAll("copy "),
    }

    try emitVal(out, arg);
    try out.writeAll("\n");
}

pub fn emitInstBi(out: anytype, tmp_ident: []const u8, tmp_type: types.BaseTy, inst: types.InstBi, op_type: types.BaseTy, lhs: types.Val, rhs: types.Val) !void {
    try out.writeAll("    ");
    try emitTmpIdent(out, tmp_ident);
    try out.writeAll(" =");
    try emitBaseTy(out, tmp_type);

    switch (inst) {
        .add => try out.writeAll("add "),
        .sub => try out.writeAll("sub "),
        .mul => try out.writeAll("mul "),

        // TODO: Include all comparisons
        .ugt => {
            try out.writeAll("c");
            try out.writeAll(switch (inst) {
                .ugt => "ugt",
                else => @panic("todo"),
            });
            try emitBaseTy(out, op_type);
        },
    }

    try emitVal(out, lhs);
    try out.writeAll(", ");
    try emitVal(out, rhs);
    try out.writeAll("\n");
}

pub fn emitInstCallBegin(out: anytype, tmp_ident: []const u8, tmp_type: types.AbiTy, func_ident: []const u8) !void {
    try out.writeAll("    ");
    try emitTmpIdent(out, tmp_ident);
    try out.writeAll(" =");
    try emitAbiTy(out, tmp_type);

    try out.writeAll("call ");
    try emitGblIdent(out, func_ident);
    try out.writeAll("(");
}

pub fn emitInstVoidCallBegin(out: anytype, func_ident: []const u8) !void {
    try out.writeAll("    call ");
    try emitGblIdent(out, func_ident);
    try out.writeAll("(");
}

/// Variadic arguments must follow C's default argument promotion rules.
/// (i.e. float arguments are promoted to double, and all small integer arguments are promoted to word)
pub fn emitInstCallArg(out: anytype, abi_ty: types.AbiTy, val: types.Val) !void {
    try emitAbiTy(out, abi_ty);
    try emitVal(out, val);
    try out.writeAll(", ");
}

pub fn emitInstCallArgEnv(out: anytype, val: types.Val) !void {
    try out.writeAll("env ");
    try emitVal(out, val);
    try out.writeAll(", ");
}

pub fn emitInstCallArgElipsis(out: anytype) !void {
    try out.writeAll("...");
    try out.writeAll(", ");
}

pub fn emitInstCallEnd(out: anytype) !void {
    try out.writeAll(")\n");
}

pub fn emitVal(out: anytype, val: types.Val) !void {
    switch (val) {
        .gbl => |gbl| try emitGblIdent(out, gbl),
        .tmp => |tmp| try emitTmpIdent(out, tmp),
        .int => |i| try out.print("{d}", .{i}),
        .sin => |s| try out.print("s_{d}", .{s}),
        .dou => |d| try out.print("d_{d}", .{d}),
    }
}

pub fn emitAbiTy(out: anytype, abi_ty: types.AbiTy) !void {
    switch (abi_ty) {
        .subw_ty => |subw_ty| try emitSubwTy(out, subw_ty),
        .agg_ty => |agg_ty| try emitAggIdent(out, agg_ty),
    }
}

pub fn emitSubwTy(out: anytype, subw_ty: types.SubwTy) !void {
    switch (subw_ty) {
        .s_byte => try out.writeAll("sb "),
        .s_half => try out.writeAll("sh "),
        .u_byte => try out.writeAll("ub "),
        .u_half => try out.writeAll("uh "),

        .word => try out.writeAll("w "),
        .long => try out.writeAll("l "),
        .single => try out.writeAll("s "),
        .double => try out.writeAll("d "),
    }
}

pub fn emitBaseTy(out: anytype, base_ty: types.BaseTy) !void {
    switch (base_ty) {
        .word => try out.writeAll("w "),
        .long => try out.writeAll("l "),
        .single => try out.writeAll("s "),
        .double => try out.writeAll("d "),
    }
}

pub fn emitAggIdent(out: anytype, ident: []const u8) !void {
    try out.writeAll(":");
    try out.writeAll(ident);
}

pub fn emitGblIdent(out: anytype, ident: []const u8) !void {
    try out.writeAll("$");
    try out.writeAll(ident);
}

pub fn emitTmpIdent(out: anytype, ident: []const u8) !void {
    try out.writeAll("%");
    try out.writeAll(ident);
}

pub fn emitLblIdent(out: anytype, ident: []const u8) !void {
    try out.writeAll("@");
    try out.writeAll(ident);
}

test "hello world" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    var out = array_list.writer();

    const expected_qbe =
      \\# Define the string constant.
      \\data $str = { b "hello world", b 0, }
      \\export function w $main() {
      \\@start
      \\    # Call the puts function with $str as argument.
      \\    %r =w call $puts(l $str, )
      \\    ret 0
      \\}
      \\
    ;

    try emitComment(out, "Define the string constant.");
    try emitDataStr(out, "str", "hello world");
    try emitFuncSigBegin(out, "main", .{.subw_ty = .word}, true);
    try emitFuncSigEnd(out);
    try emitBlockBegin(out, "start");
    try emitInstComment(out, "Call the puts function with $str as argument.");
    try emitInstCallBegin(out, "r", .{.subw_ty = .word}, "puts");
    try emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = "str"});
    try emitInstCallEnd(out);
    try emitBlockEndRet(out, .{.int = 0});
    try emitFuncEnd(out);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "hello world\n";

    try test_util.testCompileAndRun(expected_exe, source);
}

test "add pi" {
    const testing = std.testing;
    const alloc = testing.allocator;
    const test_util = @import("../test_util.zig");

    var array_list = std.ArrayList(u8).init(alloc);
    defer array_list.deinit();
    var out = array_list.writer();

    const expected_qbe =
      \\data $fmt = { b "10 + pi = %.4f", b 0, }
      \\function s $add_pi(s %a, ) {
      \\@start
      \\    %b =s add %a, s_3.1415
      \\    ret %b
      \\}
      \\export function w $main() {
      \\@start
      \\    %a =s call $add_pi(s s_10, )
      \\    %b =d exts %a
      \\    %c =w call $printf(l $fmt, ..., d %b, )
      \\    ret %c
      \\}
      \\
    ;

    try emitDataStr(out, "fmt", "10 + pi = %.4f");

    try emitFuncSigBegin(out, "add_pi", .{.subw_ty = .single}, false);
    try emitFuncParam(out, "a", .{.subw_ty = .single});
    try emitFuncSigEnd(out);
    try emitBlockBegin(out, "start");
    try emitInstBi(out, "b", .single, .add, .single, .{.tmp = "a"}, .{.sin = 3.1415});
    try emitBlockEndRet(out, .{.tmp = "b"});
    try emitFuncEnd(out);

    try emitFuncSigBegin(out, "main", .{.subw_ty = .word}, true);
    try emitFuncSigEnd(out);
    try emitBlockBegin(out, "start");
    try emitInstCallBegin(out, "a", .{.subw_ty = .single}, "add_pi");
    try emitInstCallArg(out, .{.subw_ty = .single}, .{.sin = 10});
    try emitInstCallEnd(out);
    try emitInstUn(out, "b", .double, .exts, .{.tmp = "a"});
    try emitInstCallBegin(out, "c", .{.subw_ty = .word}, "printf");
    try emitInstCallArg(out, .{.subw_ty = .long}, .{.gbl = "fmt"});
    try emitInstCallArgElipsis(out);
    try emitInstCallArg(out, .{.subw_ty = .double}, .{.tmp = "b"});
    try emitInstCallEnd(out);
    try emitBlockEndRet(out, .{.tmp = "c"});
    try emitFuncEnd(out);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "10 + pi = 13.1415";

    try test_util.testCompileAndRun(expected_exe, source);
}

