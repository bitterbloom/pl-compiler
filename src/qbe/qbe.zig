const std = @import("std");

pub fn emitHelloWorld(out: anytype) !void {
    try out.writeAll(
      \\# Define the string constant.
      \\data $str = { b "hello world", b 0 }
      \\
      \\export function w $main() {
      \\@start
      \\    # Call the puts function with $str as argument.
      \\    %r =w call $puts(l $str)
      \\    ret 0
      \\}
    );
}

test "hello world" {
    try testCompileAndRun(emitHelloWorld, "hello world\n");
}

fn testCompileAndRun(emit: fn(out: anytype) anyerror!void, expectedOutput: []const u8) !void {
    const fs = std.fs;
    const io = std.io;
    const mem = std.mem;
    const ChildProcess = std.ChildProcess;
    const testing = std.testing;
    const alloc = testing.allocator;

    const tmp = testing.tmpDir(.{}).dir;
    var tmp_dir_name = try tmp.realpathAlloc(alloc, ".");
    defer alloc.free(tmp_dir_name);

    // Alloc file path strings
    var qbe_file_name = "test.qbe";
    var qbe_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, qbe_file_name});
    defer alloc.free(qbe_file_path);

    var asm_file_name = "test.s";
    var asm_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, asm_file_name});
    defer alloc.free(asm_file_path);

    var exe_file_name = "test";
    var exe_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, exe_file_name});
    defer alloc.free(exe_file_path);

    // Create qbe file
    var qbe_file = try tmp.createFile(qbe_file_name, .{});
    defer qbe_file.close();
    var buf_writer = io.bufferedWriter(qbe_file.writer());
    try emit(buf_writer.writer());
    try buf_writer.flush();

    // TODO: We could also use `sh -c` to run qbe, cc, and the executable instead of spawning a new process for each?

    // Run qbe
    {
        var process = try ChildProcess.exec( // will fail if qbe cannot be found
            .{.argv = &.{"qbe", qbe_file_path, "-o", asm_file_path}, .allocator = alloc, .max_output_bytes = 50 * 1024}
        );
        try testing.expectEqual(process.term, ChildProcess.Term{.Exited = 0}); // will fail if qbe fails
        defer alloc.free(process.stdout); // memory is only owned if process.term is .Exited
        defer alloc.free(process.stderr);
        try fs.accessAbsolute(asm_file_path, .{.mode = .read_only}); // will fail if asm file was not created
    }

    // Run assembler (simply using cc to easily link with libc)
    {
        var process = try ChildProcess.exec( // will fail if cc cannot be found
            .{.argv = &.{"cc", asm_file_path, "-o", exe_file_path}, .allocator = alloc, .max_output_bytes = 50 * 1024}
        );
        try testing.expectEqual(process.term, ChildProcess.Term{.Exited = 0}); // will fail if cc fails
        defer alloc.free(process.stdout); // memory is only owned if process.term is .Exited
        defer alloc.free(process.stderr);
        try fs.accessAbsolute(exe_file_path, .{.mode = .read_only}); // will fail if exe file was not created
    }

    // Run executable
    {
        var process = try ChildProcess.exec( // will fail if executable cannot be found
            .{.argv = &.{exe_file_path}, .allocator = alloc, .max_output_bytes = 50 * 1024}
        );
        try testing.expectEqual(process.term, ChildProcess.Term{.Exited = 0}); // will fail if executable fails
        defer alloc.free(process.stdout); // memory is only owned if process.term is .Exited
        defer alloc.free(process.stderr);
        try testing.expectEqualStrings(process.stdout, expectedOutput); // will fail if executable does not write expected output
    }
}
