const std = @import("std");
const lex = @import("lex.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("examples/test.at", .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    const input = buffer[0..try file.readAll(&buffer)];

    std.debug.print("input:\n{s}\n\n", .{input});

    var tokens: [1024]lex.Token = undefined;
    const len = lex.lex(input, &tokens);
    std.debug.print("tokens: ", .{});
    for (tokens[0..len]) |token| {
        std.debug.print("{s} ", .{token.toStr()});
    }
}
