const std = @import("std");

pub fn main() !void {
    try std.io.getStdOut().writeAll("Hello, world!\n");
}

test {
    std.testing.refAllDeclsRecursive(@import("qbe/qbe.zig"));
}

