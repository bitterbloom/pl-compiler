const std = @import("std");
const testing = std.testing;
const alloc = testing.allocator;

pub fn testCompileAndRun(expected: []const u8, source: []const u8) !void {
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
