const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("examples/test.at", .{});
    defer file.close();

    var buffer: [1 << 16]u8 = undefined;
    const input = buffer[0..try file.readAll(&buffer)];

    var token_buffer: [1 << 16]lex.Sp(lex.Token) = undefined;
    const token_len = lex.lex(input, &token_buffer);
    const tokens = token_buffer[0..token_len];

    var compiler = compile.Compiler.init();
    compiler.compile(tokens) catch |err| compiler.debugError(err);
}
