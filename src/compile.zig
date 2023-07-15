const std = @import("std");
const lex = @import("lex.zig");
const Token = lex.Token;
const Sp = lex.Sp;
const eql = std.mem.eql;

pub const CompileErrorKind = enum {
    UnexpectedToken,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            CompileErrorKind.UnexpectedToken => writer.print("unexpected token", .{}),
        };
    }
};

pub const CompileError = struct {
    kind: CompileErrorKind,
    span: ?lex.Span,
};

pub fn compile(tokens: []const Sp(Token), then: *const fn (Compiled) void) ?CompileError {
    var comp = Compiler{
        .tokens = tokens,
        .then = then,
        .err = null,
        .data = .{
            .func = null,
        },
    };
    comp.run();
    return comp.err;
}

pub const Compiled = struct {
    func: ?*Node(Func),
};

pub const Compiler = struct {
    tokens: []const Sp(Token),
    then: *const fn (Compiled) void,
    err: ?CompileError,
    data: Compiled,

    fn run(self: *Compiler) void {
        if (self.tokens.len == 0) {
            self.then(self.data);
            return;
        }
        const token = self.tokens[0];
        self.tokens = self.tokens[1..];
        switch (token.val) {
            Token.ident => |ident| {
                inline for (@typeInfo(CBuiltins).Struct.decls) |decl| {
                    if (eql(u8, ident, decl.name)) {
                        @field(CBuiltins, decl.name)(self);
                        return;
                    }
                }
                if (self.data.func) |func| {
                    if (func.val.name.len == 0) {
                        func.val.name = ident;
                    } else {
                        var node = .{
                            .val = .{ .call = ident },
                            .next = null,
                        };
                        func.val.body = &node;
                    }
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = token.span };
                }
            },
            Token.num => |num| {
                _ = num;
            },
        }
    }
};

const CBuiltins = struct {
    pub fn @"fn"(comp: *Compiler) void {
        var func = .{
            .val = .{ .name = "", .body = null },
            .next = comp.data.func,
        };
        comp.data.func = &func;
        comp.run();
    }
};

pub const CTy = enum {
    func,
    word,
};

pub const CVal = union(CTy) {
    func: Node(Func),
    word: Node(Word),

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            CVal.func => |node| {
                try writer.print("fn {s} ", .{node.val.name});
                if (node.val.body) |head| {
                    try writer.print("{}", .{head});
                }
            },
            CVal.word => |node| try writer.print("{}", .{node.val}),
        }
    }
};

pub fn Node(comptime T: type) type {
    return struct {
        val: T,
        next: ?*Node(T),

        pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
            var this = self;
            var curr: ?*@This() = &this;
            try writer.print("[", .{});
            while (curr) |node| {
                try writer.print("{" ++ s ++ "}", .{node.val});
                if (node.next) |_| {
                    try writer.print(", ", .{});
                }
                curr = node.next;
            }
            try writer.print("]", .{});
        }
    };
}

pub fn reverse_list(comptime T: type, head: **Node(T)) void {
    var prev: ?*Node(T) = null;
    var current: ?*Node(T) = head.*;
    while (current) |curr| {
        const next = curr.next;
        curr.next = prev;
        prev = curr;
        current = next;
    }
    head.* = prev.?;
}

pub const Func = struct {
    name: []const u8,
    body: ?*Node(Word),

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("fn {s}", .{self.name});
        if (self.body) |body| {
            try writer.print(" {}", .{body});
        }
    }
};

pub const WordTy = enum {
    num,
    call,
};

pub const Word = union(WordTy) {
    num: []const u8,
    call: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            Word.num => |num| writer.print("{s}", .{num}),
            Word.call => |name| writer.print("{s}", .{name}),
        };
    }
};

const RBuiltins = struct {};
