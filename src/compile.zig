const std = @import("std");
const lex = @import("lex.zig");
const runtime = @import("runtime.zig");
const Token = lex.Token;
const Sp = lex.Sp;

pub const CompileErrorKind = enum {
    UnexpectedToken,
    InvalidNumber,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            CompileErrorKind.UnexpectedToken => writer.print("unexpected token", .{}),
            CompileErrorKind.InvalidNumber => writer.print("invalid number", .{}),
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
            .last_item = .func,
        },
    };
    comp.run();
    return comp.err;
}

pub const Compiled = struct {
    func: ?*Node(Func),
    last_item: enum {
        func,
    },

    pub fn findFunction(self: *const Compiled, name: []const u8) ?*const Func {
        var curr = self.func;
        while (curr) |func| {
            if (std.mem.eql(u8, func.val.name, name)) {
                return &func.val;
            }
            curr = func.next;
        }
        return null;
    }
};

pub const Compiler = struct {
    tokens: []const Sp(Token),
    then: *const fn (Compiled) void,
    err: ?CompileError,
    data: Compiled,

    fn run(self: *Compiler) void {
        if (self.tokens.len == 0) {
            self.finishItem();
            self.then(self.data);
            return;
        }
        const token = self.tokens[0];
        self.tokens = self.tokens[1..];
        switch (token.val) {
            Token.ident => |ident| {
                inline for (@typeInfo(CBuiltins).Struct.decls) |decl| {
                    if (std.mem.eql(u8, ident, decl.name)) {
                        self.finishItem();
                        @field(CBuiltins, decl.name)(self);
                        return;
                    }
                }
                if (self.data.func) |func| {
                    if (func.val.name.len == 0) {
                        func.val.name = ident;
                    } else {
                        var node: ?Node(Word) = null;
                        inline for (@typeInfo(runtime.RBuiltins).Struct.decls) |decl| {
                            if (std.mem.eql(u8, ident, decl.name)) {
                                node = .{
                                    .val = .{ .builtin = .{
                                        .name = ident,
                                        .f = @field(runtime.RBuiltins, decl.name),
                                    } },
                                    .next = func.val.body,
                                };
                                break;
                            }
                        }
                        var n = node orelse Node(Word){
                            .val = .{ .call = ident },
                            .next = func.val.body,
                        };
                        func.val.body = &n;
                    }
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = token.span };
                }
            },
            Token.num => |num| {
                if (self.data.func) |func| {
                    const i = std.fmt.parseInt(i64, num, 10) catch {
                        self.err = .{ .kind = CompileErrorKind.InvalidNumber, .span = token.span };
                        return;
                    };
                    var node = .{
                        .val = .{ .int = i },
                        .next = func.val.body,
                    };
                    func.val.body = &node;
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = token.span };
                }
            },
        }
    }

    fn finishItem(self: *Compiler) void {
        switch (self.data.last_item) {
            .func => if (self.data.func) |func|
                if (func.val.body) |*body|
                    reverse_list(Word, body),
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
        comp.data.last_item = .func;
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
    int,
    builtin,
    call,
};

pub const Word = union(WordTy) {
    int: i64,
    builtin: runtime.BuiltinFn,
    call: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            Word.int => |int| writer.print("{}", .{int}),
            Word.builtin => |f| writer.print("{}", .{f}),
            Word.call => |name| writer.print("{s}", .{name}),
        };
    }
};
