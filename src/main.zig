const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");
const runtime = @import("runtime.zig");

pub fn main() !void {
    const file = try std.fs.cwd().openFile("examples/test.at", .{});
    defer file.close();

    var buffer: [1 << 16]u8 = undefined;
    const input = buffer[0..try file.readAll(&buffer)];

    var token_buffer: [1 << 16]lex.Sp(lex.Token) = undefined;
    const token_len = lex.lex(input, &token_buffer);
    const tokens = token_buffer[0..token_len];

    if (compile.compile(tokens, &whenCompiled)) |err| {
        std.debug.print("Error", .{});
        if (err.span) |span| {
            std.debug.print(" at {}", .{span.start});
        }
        std.debug.print(": {}", .{err.kind});
        if (err.span) |span| {
            std.debug.print(" `{s}`", .{input[span.start.pos..span.end.pos]});
        }
    }
}

fn whenCompiled(data: compile.Compiled) void {
    if (runtime.trace_rt) {
        var func = data.functions.head;
        while (func) |f| {
            std.debug.print("{}\n", .{f.val});
            func = f.next;
        }
        std.debug.print("\n", .{});
    }

    var rt = runtime.Runtime.init(data);
    if (rt.start()) |err| std.debug.print("{}", .{err});
}
