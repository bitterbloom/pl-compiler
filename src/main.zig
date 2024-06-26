const std = @import("std");
const ir = @import("ir/types.zig");
const parse = @import("ir/parse.zig");
const print = @import("ir/print.zig");
const to_qbe = @import("ir/to_qbe.zig");

// TODO: Add '--verbose' option
const CliArgs = struct {
    compiler_path: ?[]const u8 = null,
    input_files: std.ArrayListUnmanaged([]const u8) = .{},
    output_file: ?[]const u8 = null,
    print_ir: bool = false,
    print_qbe: bool = false,
    print_asm: bool = false,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

    var cli_args = CliArgs{};
    defer cli_args.input_files.deinit(alloc);

    var args = try std.process.argsWithAllocator(alloc);
    defer args.deinit();

    args: {
        cli_args.compiler_path = args.next() orelse break :args;

        while (true) {
            const arg = args.next() orelse break :args;
            
            if      (std.mem.eql(u8, arg, "--print-ir")) cli_args.print_ir = true
            else if (std.mem.eql(u8, arg, "--print-qbe")) cli_args.print_qbe = true
            else if (std.mem.eql(u8, arg, "--print-asm")) cli_args.print_asm = true
            else if (std.mem.eql(u8, arg, "--output") or std.mem.eql(u8, arg, "-o")) {
                cli_args.output_file = args.next() orelse {
                    try std.io.getStdErr().writeAll("Expected argument after --output or -o\n");
                    return;
                };
            }
            else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
                try std.io.getStdOut().writeAll("Usage: ir [options] [files]\n");
                try std.io.getStdOut().writeAll("Options:\n");
                try std.io.getStdOut().writeAll("  --print-ir\n");
                try std.io.getStdOut().writeAll("  --print-qbe\n");
                try std.io.getStdOut().writeAll("  --print-asm\n");
                try std.io.getStdOut().writeAll("  --output <file> | -o <file>\n");
                try std.io.getStdOut().writeAll("  --help | -h\n");
                return;
            }
            else if (arg[0] == '-') {
                try std.io.getStdErr().writeAll("Unknown option: ");
                try std.io.getStdErr().writeAll(arg);
                try std.io.getStdErr().writeAll("\n");
                try std.io.getStdErr().writeAll("Use --help to see available options\n");
                return;
            }
            else {
                try cli_args.input_files.append(alloc, arg);
            }
        }
    }

    const cwd = std.fs.cwd();

    const source: []const u8 = if (cli_args.input_files.items.len >= 1) src: {
        var source_list = std.ArrayList(u8).init(alloc);
        for (cli_args.input_files.items) |file_name| {
            const file = try cwd.openFile(file_name, .{});
            defer file.close();

            try file.reader().readAllArrayList(&source_list, 1024 * 1024 * 1024);
        }
        break :src try source_list.toOwnedSlice();
    }
    else src: {
        var reader = std.io.bufferedReader(std.io.getStdIn().reader());
        break :src try reader.reader().readAllAlloc(alloc, 1024 * 1024 * 1024);
    };
    defer alloc.free(source);

    const tty_config = std.io.tty.detectConfig(std.io.getStdOut());

    // Parse
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    try std.io.getStdOut().writeAll("\nParsing input...\n");
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);

    var parse_result = try parse.parseProgram(source, alloc);
    var parse_errors: []const parse.ParseError = parse_result[0];
    var parsed_program: ir.Program = parse_result[1];
    defer alloc.free(parse_errors);
    defer parsed_program.deinit(alloc);

    if (parse_errors.len >= 1) {
        for (parse_errors) |parse_error| {
            try std.io.getStdErr().writeAll(try parse_error.fmt(alloc));
        }
        return;
    }

    if (cli_args.print_ir) try print.printProgram(std.io.getStdOut().writer(), parsed_program);

    // Output qbe
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    std.debug.print("\nEmitting qbe...\n", .{});
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);

    const qbe_file_path = try std.fmt.allocPrint(alloc, "./{s}.qbe", .{cli_args.output_file orelse "out"});
    defer alloc.free(qbe_file_path);

    const qbe_file = try cwd.createFile(qbe_file_path, .{.exclusive = true});
    defer qbe_file.close();
    defer cwd.deleteFile(qbe_file_path) catch {};

    try to_qbe.emitQbe(qbe_file.writer(), parsed_program);
    if (cli_args.print_qbe) try to_qbe.emitQbe(std.io.getStdOut().writer(), parsed_program);

    // Output asm
    const asm_file_path = try std.fmt.allocPrint(alloc, "./{s}.s", .{cli_args.output_file orelse "out"});
    defer alloc.free(asm_file_path);

    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    std.debug.print("\nRunning qbe...\n", .{});
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);

    const qbe = try std.process.Child.exec(.{.argv = &.{"qbe", "-o", asm_file_path, qbe_file_path}, .allocator = alloc});
    switch (qbe.term) {
        .Exited => |exit_code| if (exit_code != 0) return error.QbeFailed,
        else => return error.QbeFailed,
    }
    defer { alloc.free(qbe.stdout); alloc.free(qbe.stderr); }
    try std.io.getStdOut().writeAll(qbe.stderr);
    try std.io.getStdOut().writeAll(qbe.stdout);

    try cwd.access(asm_file_path, .{.mode = .read_only});
    if (cli_args.print_asm) {
        const asm_file = try cwd.openFile(asm_file_path, .{.mode = .read_only});
        defer asm_file.close();

        var buffer: [4096]u8 = undefined;
        while (true) {
            const read_result = try asm_file.reader().read(buffer[0..]);
            if (read_result == 0) break;
            try std.io.getStdOut().writeAll(buffer[0..read_result]);
        }
    }
    defer cwd.deleteFile(asm_file_path) catch {};

    // Output executable
    const exe_file_path = try std.fmt.allocPrint(alloc, "./{s}", .{cli_args.output_file orelse "out"});
    defer alloc.free(exe_file_path);

    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    std.debug.print("\nRunning cc...\n", .{});
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);

    const cc = try std.process.Child.exec(.{.argv = &.{"cc", "-o", exe_file_path, asm_file_path}, .allocator = alloc});

    switch (cc.term) {
        .Exited => |exit_code| if (exit_code != 0) return error.CcFailed,
        else => return error.CcFailed,
    }
    defer { alloc.free(cc.stdout); alloc.free(cc.stderr); }
    try std.io.getStdOut().writeAll(cc.stderr);
    try std.io.getStdOut().writeAll(cc.stdout);

    try cwd.access(exe_file_path, .{.mode = .read_only});
    defer if (cli_args.output_file) |_| {} else cwd.deleteFile(exe_file_path) catch {};

    // Run the executable
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    std.debug.print("\nRunning executable...\n", .{});
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);

    var exe = try std.process.Child.exec(.{.argv = &.{exe_file_path}, .allocator = alloc});
    switch (exe.term) {
        .Exited => {},
        else => return error.ExeFailed,
    }
    defer { alloc.free(exe.stdout); alloc.free(exe.stderr); }
    try std.io.getStdOut().writeAll(exe.stderr);
    try std.io.getStdOut().writeAll(exe.stdout);
    
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.dim);
    try std.io.getStdOut().writer().print("Executable finished with exit code {}\n", .{exe.term.Exited});
    try tty_config.setColor(std.io.getStdOut(), std.io.tty.Color.reset);
}

test {
    std.testing.refAllDeclsRecursive(@import("qbe/emit.zig"));
    std.testing.refAllDeclsRecursive(@import("ir/tests.zig"));
    std.testing.refAllDeclsRecursive(@import("ir/parse.zig"));
}

