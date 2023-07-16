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
            .functions = List(Func).init(),
            .last_item = .func,
        },
    };
    comp.run();
    return comp.err;
}

pub const Compiled = struct {
    functions: List(Func),
    last_item: enum {
        func,
    },

    pub fn findFunction(self: *const Compiled, name: []const u8) ?*Func {
        var curr = self.functions.head;
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
                        var builtin_fn: ?runtime.BuiltinFn = null;
                        inline for (@typeInfo(runtime.RBuiltins).Struct.decls) |decl| {
                            if (std.mem.eql(u8, ident, decl.name)) {
                                builtin_fn = @field(runtime.RBuiltins, decl.name);
                                break;
                            }
                        }
                        var node: Node(Word) = undefined;
                        if (builtin_fn) |f| {
                            node = Node(Word).init(.{ .builtin = .{
                                .f = f,
                                .name = ident,
                            } });
                        } else {
                            node = Node(Word).init(.{ .call = .{
                                .name = ident,
                                .span = token.span,
                                .func = null,
                            } });
                        }
                        func.val.body.push(&node);
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
                    var node = Node(Word).init(.{ .int = i });
                    func.val.body.push(&node);
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
        var currFunc = self.data.functions.head;
        while (currFunc) |func| {
            var currWord = func.val.body.head;
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
        var currWord = func.body.head;
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
        var curr = self.data.functions.head;
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
        var node = Node(Func).init(.{
            .name = null,
            .span = span,
            .body = List(Word).init(),
            .is_finished = false,
        });
        comp.data.functions.push(&node);
        comp.data.last_item = .func;
        comp.run();
    }
    pub fn @"["(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |_| {} else {
            comp.err = .{ .kind = CompileErrorKind.UnexpectedToken, .span = span };
            return;
        }
        var node = Node(Func).init(.{
            .name = null,
            .span = span,
            .body = List(Word).init(),
            .is_finished = false,
        });
        comp.data.functions.push(&node);
        comp.data.last_item = .func;
        comp.run();
    }
    pub fn @"]"(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |func| {
            func.val.finish();
            var parent = comp.unfinishedFunc().?;
            var node = Node(Word).init(.{ .quote = .{ .name = null, .span = span, .func = &func.val } });
            parent.val.body.push(&node);
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

        pub fn init(val: T) @This() {
            return .{ .val = val, .next = null };
        }
    };
}

pub fn List(comptime T: type) type {
    return struct {
        head: ?*Node(T),

        pub fn init() @This() {
            return .{ .head = null };
        }

        pub fn push(self: *@This(), node: *Node(T)) void {
            node.next = self.head;
            self.head = node;
        }

        pub fn pop(self: *@This()) ?*Node(T) {
            if (self.head) |head| {
                self.head = head.next;
                return head;
            } else {
                return null;
            }
        }

        pub fn reverse(self: *@This()) void {
            if (self.head) |head| {
                var prev: ?*Node(T) = null;
                var current: ?*Node(T) = head;
                while (current) |curr| {
                    const next = curr.next;
                    curr.next = prev;
                    prev = curr;
                    current = next;
                }
                self.head = prev.?;
            }
        }

        pub fn len(self: *const @This()) usize {
            var curr = self.head;
            var n = 0;
            while (curr) |node| : (curr = node.next) n += 1;
            return n;
        }

        pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
            var curr = self.head;
            try writer.print("[", .{});
            var i: u8 = 0;
            while (curr) |node| {
                try writer.print("{" ++ s ++ "}", .{node.val});
                if (node.next) |_| {
                    try writer.print(", ", .{});
                }
                curr = node.next;
                i += 1;
                if (i > 50) {
                    try writer.print("...", .{});
                    break;
                }
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
    body: List(Word),
    is_finished: bool,

    pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        if (self.name) |name| {
            if (std.mem.eql(u8, s, "s")) {
                try writer.print("{s}", .{name});
                return;
            } else {
                try writer.print("fn {s}", .{name});
            }
        } else {
            try writer.print("fn at {}", .{self.span.start});
        }
        try writer.print(" {}", .{self.body});
    }

    pub fn finish(self: *Func) void {
        self.is_finished = true;
        self.body.reverse();
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
