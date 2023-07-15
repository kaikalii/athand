const std = @import("std");
const lex = @import("lex.zig");
const runtime = @import("runtime.zig");
const Token = lex.Token;
const Sp = lex.Sp;
const Span = lex.Span;

pub const CompileErrorKind = enum {
    UnexpectedToken,
    InvalidNumber,
    UnknownFunction,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            CompileErrorKind.UnexpectedToken => writer.print("unexpected token", .{}),
            CompileErrorKind.InvalidNumber => writer.print("invalid number", .{}),
            CompileErrorKind.UnknownFunction => writer.print("unknown function", .{}),
        };
    }
};

pub const CompileError = struct {
    kind: CompileErrorKind,
    span: ?lex.Span,
};

pub fn compile(
    tokens: []const Sp(Token),
    on_success: *const fn (Compiled) void,
) ?CompileError {
    var comp = Compiler{
        .tokens = tokens,
        .on_success = on_success,
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

    pub fn findFunction(self: *const Compiled, name: []const u8) ?*Func {
        var curr = self.func;
        while (curr) |func| {
            if (func.val.name) |fname| {
                if (std.mem.eql(u8, fname, name)) {
                    return &func.val;
                }
            }
            curr = func.next;
        }
        return null;
    }
};

pub const Compiler = struct {
    tokens: []const Sp(Token),
    on_success: *const fn (Compiled) void,
    err: ?CompileError,
    data: Compiled,

    fn run(self: *Compiler) void {
        if (self.tokens.len == 0) {
            self.finish();
            return;
        }
        const token = self.tokens[0];
        self.tokens = self.tokens[1..];
        switch (token.val) {
            Token.ident => |ident| {
                inline for (@typeInfo(CBuiltins).Struct.decls) |decl| {
                    if (std.mem.eql(u8, ident, decl.name)) {
                        @field(CBuiltins, decl.name)(self, token.span);
                        return;
                    }
                }
                if (self.unfinishedFunc()) |func| {
                    if (func.val.name) |_| {
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
                            .val = .{ .call = .{ .name = ident, .span = token.span, .func = null } },
                            .next = func.val.body,
                        };
                        func.val.body = &n;
                    } else {
                        func.val.name = ident;
                    }
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = token.span };
                }
            },
            Token.num => |num| {
                if (self.unfinishedFunc()) |func| {
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
            .func => if (self.unfinishedFunc()) |func|
                func.val.finish(),
        }
    }

    fn finish(self: *Compiler) void {
        // Finish last item
        self.finishItem();

        // Resolve identifiers
        var currFunc = self.data.func;
        while (currFunc) |func| {
            var currWord = func.val.body;
            while (currWord) |word| {
                self.resolveIdentifiers(&func.val);
                currWord = word.next;
            }
            currFunc = func.next;
        }

        // Call continuation
        self.on_success(self.data);
    }

    fn resolveIdentifiers(self: *Compiler, func: *Func) void {
        var currWord = func.body;
        while (currWord) |word| {
            switch (word.val) {
                .call => |*function| {
                    if (function.func) |child| {
                        self.resolveIdentifiers(child);
                    } else if (function.name) |name| {
                        function.func = self.data.findFunction(name) orelse {
                            self.err = .{ .kind = CompileErrorKind.UnknownFunction, .span = function.span };
                            return;
                        };
                    }
                },
                else => {},
            }
            currWord = word.next;
        }
    }

    fn unfinishedFunc(self: *Compiler) ?*Node(Func) {
        var curr = self.data.func;
        while (curr) |func| {
            if (!func.val.is_finished) {
                return curr;
            }
            curr = func.next;
        }
        return null;
    }
};

const CBuiltins = struct {
    pub fn @"fn"(comp: *Compiler, span: Span) void {
        comp.finishItem();
        var func = .{
            .val = .{ .name = null, .span = span, .body = null, .is_finished = false },
            .next = comp.data.func,
        };
        comp.data.func = &func;
        comp.data.last_item = .func;
        comp.run();
    }
    pub fn @"["(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |_| {} else {
            comp.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = span };
            return;
        }
        var func = .{
            .val = .{ .name = null, .span = span, .body = null, .is_finished = false },
            .next = comp.data.func,
        };
        comp.data.func = &func;
        comp.data.last_item = .func;
        comp.run();
    }
    pub fn @"]"(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |func| {
            func.val.finish();
            var parent = comp.unfinishedFunc().?;
            var node: Node(Word) = .{
                .val = .{ .quote = .{ .name = null, .span = span, .func = &func.val } },
                .next = parent.val.body,
            };
            parent.val.body = &node;
            comp.run();
        } else {
            comp.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = span };
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
    name: ?[]const u8,
    span: Span,
    body: ?*Node(Word),
    is_finished: bool,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        if (self.name) |name| {
            try writer.print("fn {s}", .{name});
        } else {
            try writer.print("fn at {}", .{self.span.start});
        }
        if (self.body) |body| {
            try writer.print(" {}", .{body});
        }
    }

    pub fn finish(self: *Func) void {
        self.is_finished = true;
        if (self.body) |*body| {
            reverse_list(Word, body);
        }
    }
};

pub const WordTy = enum {
    int,
    builtin,
    call,
    quote,
};

pub const Word = union(WordTy) {
    int: i64,
    builtin: runtime.BuiltinFunction,
    call: runtime.CodeFunction,
    quote: runtime.CodeFunction,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            .int => |int| writer.print("{}", .{int}),
            .builtin => |f| writer.print("{}", .{f}),
            .call, .quote => |name| writer.print("{s}", .{name}),
        };
    }
};
