const std = @import("std");
const lex = @import("lex.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("examples/test.at", .{});
    defer file.close();

    var buffer = [_]u8{0} ** (1 << 16);
    const input = buffer[0..try file.readAll(&buffer)];

    std.debug.print("input:\n{s}\n\n", .{input});

    std.debug.print("tokens:\n", .{});
    var tokens = [_]lex.Token{lex.Token{ .eof = undefined }} ** 1024;
    const len = lex.lex(input, &tokens);
    for (tokens[0..len]) |token| {
        std.debug.print("{s}\n", .{token.toStr()});
    }
}
