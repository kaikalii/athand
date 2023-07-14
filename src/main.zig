const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("examples/test.at", .{});
    defer file.close();

    var buffer: [1 << 16]u8 = undefined;
    const input = buffer[0..try file.readAll(&buffer)];

    std.debug.print("input:\n{s}\n\n", .{input});

    var token_buffer: [1 << 16]lex.Sp(lex.Token) = undefined;
    const token_len = lex.lex(input, &token_buffer);
    std.debug.print("tokens: ", .{});
    const tokens = token_buffer[0..token_len];
    for (tokens) |token| {
        std.debug.print("{s} ", .{token.val.toStr()});
    }
    std.debug.print("\n\n", .{});

    var compiler = compile.Compiler.init();
    compiler.compile(tokens) catch |err| compiler.debugError(err);
}
