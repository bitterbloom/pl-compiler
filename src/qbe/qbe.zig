const std = @import("std");
const types = @import("types.zig");

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

pub fn emitFuncBegin(out: anytype, ident: []const u8, ret_ty: types.AbiTy, params: []const ?types.Param, exported: bool) !void {
    if (exported) try out.writeAll("export ");
    try out.writeAll("function ");

    switch (ret_ty) {
        .subw_ty => |subw_ty| try emitSubwTy(out, subw_ty),
        .agg_ty => |agg_ty| try emitAggIdent(out, agg_ty),
        .none_env => {},
    }
    try emitGblIdent(out, ident);

    try out.writeAll("(");
    for (params) |param_opt| {
        if (param_opt) |param| {
            switch (param.abi_ty) {
                .subw_ty => |subw_ty| try emitSubwTy(out, subw_ty),
                .agg_ty => |agg_ty| try emitAggIdent(out, agg_ty),
                .none_env => try out.writeAll("env "),
            }
            try emitTmpIdent(out, param.ident);
        }
        else try out.writeAll("...");
        try out.writeAll(", ");
    }
    try out.writeAll(") {\n");
}

pub fn emitFuncEnd(out: anytype) !void {
    try out.writeAll("}\n");
}

pub fn emitBlockBegin(out: anytype, ident: []const u8) !void {
    try emitLblIdent(out, ident);
    try out.writeAll("\n");
}

pub fn emitBlockEndRet(out: anytype, val: ?types.Val) !void {
    try out.writeAll("    ret ");
    if (val) |v| try emitVal(out, v);
    try out.writeAll("\n");
}

pub fn emitInstUn(out: anytype, inst: types.InstUn, tmp_ident: []const u8, tmp_type: types.BaseTy, arg: types.Val) !void {
    try out.writeAll("    ");
    try emitTmpIdent(out, tmp_ident);

    try out.writeAll(" =");
    try emitBaseTy(out, tmp_type);

    switch (inst) {
        .exts => try out.writeAll("exts "),
    }

    try emitVal(out, arg);
    try out.writeAll("\n");
}

pub fn emitInstBi(out: anytype, inst: types.InstBi, tmp_ident: []const u8, tmp_type: types.BaseTy, lhs: types.Val, rhs: types.Val) !void {
    try out.writeAll("    ");
    try emitTmpIdent(out, tmp_ident);

    try out.writeAll(" =");
    try emitBaseTy(out, tmp_type);

    switch (inst) {
        .add => try out.writeAll("add "),
        .sub => try out.writeAll("sub "),
    }

    try emitVal(out, lhs);
    try out.writeAll(", ");
    try emitVal(out, rhs);
    try out.writeAll("\n");
}

pub fn emitInstCall(out: anytype, tmp_ident: ?[]const u8, tmp_type: types.AbiTy, callee: []const u8, args: []const ?types.Arg) !void {
    try out.writeAll("    ");
    if (tmp_ident) |ident| {
        try emitTmpIdent(out, ident);
        try out.writeAll(" =");
        switch (tmp_type) {
            .subw_ty => |subw_ty| try emitSubwTy(out, subw_ty),
            .agg_ty => |agg_ty| try emitAggIdent(out, agg_ty),
            .none_env => try out.writeAll("env"),
        }
    }

    try out.writeAll("call ");
    try emitGblIdent(out, callee);
    try emitArgs(out, args);
    try out.writeAll("\n");
}

pub fn emitArgs(out: anytype, args: []const ?types.Arg) !void {
    try out.writeAll("(");
    for (args) |arg_opt| {
        if (arg_opt) |arg| {
            switch (arg.abi_ty) {
                .subw_ty => |subw_ty| try emitSubwTy(out, subw_ty),
                .agg_ty => |agg_ty| try emitAggIdent(out, agg_ty),
                .none_env => try out.writeAll("env "),
            }
            try emitVal(out, arg.val);
        }
        else try out.writeAll("...");
        try out.writeAll(", ");
    }
    try out.writeAll(")");
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
    try emitFuncBegin(out, "main", .{.subw_ty = .word }, &.{}, true);
    try emitBlockBegin(out, "start");
    try emitInstComment(out, "Call the puts function with $str as argument.");
    try emitInstCall(out, "r", .{.subw_ty = .word}, "puts", &.{.{.abi_ty = .{.subw_ty = .long}, .val = .{.gbl = "str"}}});
    try emitBlockEndRet(out, .{.int = 0});
    try emitFuncEnd(out);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "hello world\n";

    try testCompileAndRun(expected_exe, source);
}

test "add pi" {
    const testing = std.testing;
    const alloc = testing.allocator;

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

    try emitFuncBegin(out, "add_pi", .{.subw_ty = .single}, &.{.{.abi_ty = .{.subw_ty = .single}, .ident = "a"}}, false);
    try emitBlockBegin(out, "start");
    try emitInstBi(out, .add, "b", .single, .{.tmp = "a"}, .{.sin = 3.1415});
    try emitBlockEndRet(out, .{.tmp = "b"});
    try emitFuncEnd(out);

    try emitFuncBegin(out, "main", .{.subw_ty = .word}, &.{}, true);
    try emitBlockBegin(out, "start");
    try emitInstCall(out, "a", .{.subw_ty = .single}, "add_pi", &.{.{.abi_ty = .{.subw_ty = .single}, .val = .{.sin = 10}}});
    try emitInstUn(out, .exts, "b", .double, .{.tmp = "a"});
    try emitInstCall(out, "c", .{.subw_ty = .word}, "printf", &.{.{.abi_ty = .{.subw_ty = .long}, .val = .{.gbl = "fmt"}}, null, .{.abi_ty = .{.subw_ty = .double}, .val = .{.tmp = "b"}}});
    try emitBlockEndRet(out, .{.tmp = "c"});
    try emitFuncEnd(out);

    var source = try array_list.toOwnedSlice();
    defer alloc.free(source);

    try testing.expectEqualStrings(expected_qbe, source);

    const expected_exe = "10 + pi = 13.1415";

    try testCompileAndRun(expected_exe, source);
}

fn testCompileAndRun(expected: []const u8, source: []const u8) !void {
    const testing = std.testing;
    const alloc = testing.allocator;

    const dir = testing.tmpDir(.{}).dir;
    const qbe_file_name = "./test.qbe";
    const asm_file_name = "./test.s";
    const exe_file_name = "./test";

    // Run qbe
    {
        var qbe_file = try dir.createFile(qbe_file_name, .{});
        defer qbe_file.close();
        try qbe_file.writeAll(source);

        // TODO: We could also use `sh -c` to run qbe, cc, and the executable instead of spawning a new process for each?

        var qbe = try std.ChildProcess.exec(.{.argv = &.{"qbe", qbe_file_name, "-o", asm_file_name}, .cwd_dir = dir, .allocator = alloc, .max_output_bytes = 10 * 1024}); // fails if qbe was not found
        //try std.testing.expectEqual(qbe.term, std.ChildProcess.Term{.Exited = 0}); // fails if qbe failed
        defer { alloc.free(qbe.stdout); alloc.free(qbe.stderr); }
        try testing.expectEqualStrings("", qbe.stderr); // fails if qbe printed to stderr
        try testing.expectEqualStrings("", qbe.stdout); // fails if qbe printed to stdout
        try dir.access(asm_file_name, .{ .mode = .read_only }); // fails if asm file was not created
    }

    // Run cc (links with libc)
    var cc = try std.ChildProcess.exec(.{.argv = &.{"cc", asm_file_name, "-o", exe_file_name}, .cwd_dir = dir, .allocator = alloc, .max_output_bytes = 10 * 1024}); // fails if cc was not found
    //try std.testing.expectEqual(cc.term, std.ChildProcess.Term{.Exited = 0}); // fails if cc failed
    defer { alloc.free(cc.stdout); alloc.free(cc.stderr); }
    try testing.expectEqualStrings("", cc.stderr); // fails if cc printed to stderr
    try testing.expectEqualStrings("", cc.stdout); // fails if cc printed to stdout
    try dir.access(exe_file_name, .{ .mode = .read_only }); // fails if exe file was not created

    // Run the executable
    var exe = try std.ChildProcess.exec(.{.argv = &.{exe_file_name}, .cwd_dir = dir, .allocator = alloc, .max_output_bytes = 10 * 1024}); // fails if exe was not found
    //try std.testing.expect(exe.term, std.ChildProcess.Term{.Exited = 0});
    defer { alloc.free(exe.stdout); alloc.free(exe.stderr); }
    try testing.expectEqualStrings("", exe.stderr); // fails if exe printed to stderr
    try testing.expectEqualStrings(expected, exe.stdout); // fails if exe printed incorrect output
}

