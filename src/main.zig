const std = @import("std");

pub fn main() !void {
    try std.io.getStdOut().writeAll("Hello, world!\n");
}

test {
    std.testing.refAllDeclsRecursive(@import("qbe/emit.zig"));
    std.testing.refAllDeclsRecursive(@import("ir/to_qbe.zig"));
}

