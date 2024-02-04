const std = @import("std");
//const qbe = @import("qbe/qbe.zig");

pub fn main() !void {
    try std.io.getStdOut().writeAll("Hello, world!\n");


    const fs = std.fs;
    const io = std.io;
    const mem = std.mem;
    const process = std.process;
    const testing = std.testing;
    const alloc = std.heap.page_allocator;

    const tmp = testing.tmpDir(.{}).dir;
    var tmp_dir_name = try tmp.realpathAlloc(alloc, ".");
    defer alloc.free(tmp_dir_name);

    // Create qbe file
    var qbe_file_name = "hello_world.qbe";
    var qbe_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, qbe_file_name});
    defer alloc.free(qbe_file_path);
    var qbe_file = try tmp.createFile(qbe_file_name, .{});
    defer qbe_file.close();

    // Write to qbe file
    var buf_writer = io.bufferedWriter(qbe_file.writer());
    try @import("qbe/qbe.zig").emitHelloWorld(buf_writer.writer());
    try buf_writer.flush();

    // Alloc full name for asm file
    var asm_file_name = "hello_world.s";
    var asm_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, asm_file_name});
    defer alloc.free(asm_file_path);

    // Run qbe
    var qbe = process.Child.init(&.{"qbe", qbe_file_path, "-o", asm_file_path}, alloc);
    var qbe_term = try qbe.spawnAndWait();                                  // will fail if qbe cannot be found
    try testing.expectEqual(qbe_term, process.Child.Term{.Exited = 0});     // will fail if qbe fails
    try fs.accessAbsolute(asm_file_path, .{.mode = .read_only});            // will fail if asm file was not created

    // Alloc full name for executable
    var exe_file_name = "hello_world";
    var exe_file_path = try mem.join(alloc, "/", &.{tmp_dir_name, exe_file_name});
    defer alloc.free(exe_file_path);

    // Run assembler (gcc to easily link with libc)
    var gcc = process.Child.init(&.{"gcc", asm_file_path, "-o", exe_file_path}, alloc);
    var gcc_term = try gcc.spawnAndWait();                                  // will fail if gcc cannot be found
    try testing.expectEqual(gcc_term, process.Child.Term{.Exited = 0});     // will fail if gcc fails
    try fs.accessAbsolute(exe_file_path, .{.mode = .read_only});            // will fail if exe file was not created

    // Run executable
    var exe = process.Child.init(&.{exe_file_path}, alloc);
    var exe_term = try exe.spawnAndWait();                                  // will fail if exe cannot be found
    try testing.expectEqual(exe_term, process.Child.Term{.Exited = 0});     // will fail if exe fails
}

test {
    std.testing.refAllDeclsRecursive(@import("qbe/qbe.zig"));
}

