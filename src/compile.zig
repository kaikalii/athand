const std = @import("std");
const lex = @import("lex.zig");
const runtime = @import("runtime.zig");
const Token = lex.Token;
const Sp = lex.Sp;
const Span = lex.Span;
const Int = runtime.Int;

pub const CompileErrorKind = enum {
    unexpected_token,
    invalid_number,
    unknown_function,
    unclosed_function,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            CompileErrorKind.unexpected_token => writer.print("unexpected token", .{}),
            CompileErrorKind.invalid_number => writer.print("invalid number", .{}),
            CompileErrorKind.unknown_function => writer.print("unknown function", .{}),
            CompileErrorKind.unclosed_function => writer.print("unclosed function", .{}),
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
        },
    };
    comp.run();
    return comp.err;
}

pub const Compiled = struct {
    functions: List(Func),

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
                    var has_name = if (func.val.name) |name| name.len > 0 else true;
                    if (has_name) {
                        var builtin_fn: ?runtime.BuiltinFn = null;
                        inline for (@typeInfo(runtime.RBuiltins).Struct.decls) |decl| {
                            if (std.mem.eql(u8, ident, decl.name)) {
                                builtin_fn = @field(runtime.RBuiltins, decl.name);
                                break;
                            }
                        }
                        var word: Word = undefined;
                        if (builtin_fn) |f| {
                            word = .{ .builtin = .{
                                .f = f,
                                .name = ident,
                            } };
                        } else {
                            word = .{ .call = .{
                                .name = ident,
                                .func = null,
                            } };
                        }
                        var node = Node(Sp(Word)).init(.{
                            .val = word,
                            .span = token.span,
                        });
                        func.val.body.push(&node);
                    } else {
                        func.val.name = ident;
                    }
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.unexpected_token, .span = token.span };
                }
            },
            Token.num => |num| {
                if (self.unfinishedFunc()) |func| {
                    const i = std.fmt.parseInt(Int, num, 10) catch {
                        self.err = .{ .kind = CompileErrorKind.invalid_number, .span = token.span };
                        return;
                    };
                    var node = Node(Sp(Word)).init(.{ .val = .{ .int = i }, .span = token.span });
                    func.val.body.push(&node);
                    self.run();
                } else {
                    self.err = .{ .kind = CompileErrorKind.unexpected_token, .span = token.span };
                }
            },
        }
    }

    fn finishItem(self: *Compiler) void {
        if (self.unfinishedFunc()) |func| {
            func.val.finish();
        }
    }

    fn finish(self: *Compiler) void {
        // Ensure all quotes are finished
        if (self.unfinishedFunc()) |func| {
            _ = func.val.name orelse {
                self.err = .{ .kind = CompileErrorKind.unclosed_function, .span = func.val.span };
            };
        }

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

        if (self.err) |_| {
            return;
        }

        // Call continuation
        self.on_success(self.data);
    }

    fn resolveIdentifiers(self: *Compiler, func: *Func) void {
        var currWord = func.body.head;
        while (currWord) |word| {
            switch (word.val.val) {
                .call => |*function| {
                    if (function.func) |child| {
                        self.resolveIdentifiers(child);
                    } else if (function.name) |name| {
                        function.func = self.data.findFunction(name) orelse {
                            self.err = .{
                                .kind = CompileErrorKind.unknown_function,
                                .span = (function.func orelse func).span,
                            };
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
            .name = "",
            .span = span,
            .body = List(Sp(Word)).init(),
            .is_finished = false,
        });
        comp.data.functions.push(&node);
        comp.run();
    }
    pub fn @"["(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |_| {} else {
            comp.err = .{ .kind = CompileErrorKind.unexpected_token, .span = span };
            return;
        }
        var node = Node(Func).init(.{
            .name = null,
            .span = span,
            .body = List(Sp(Word)).init(),
            .is_finished = false,
        });
        comp.data.functions.push(&node);
        comp.run();
    }
    pub fn @"]"(comp: *Compiler, span: Span) void {
        if (comp.unfinishedFunc()) |func| {
            func.val.finish();
            var parent = comp.unfinishedFunc().?;
            var node = Node(Sp(Word)).init(.{
                .val = .{ .quote = .{ .name = null, .func = &func.val } },
                .span = span,
            });
            parent.val.body.push(&node);
            comp.run();
        } else {
            comp.err = .{ .kind = CompileErrorKind.unexpected_token, .span = span };
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

/// A function definition
pub const Func = struct {
    /// The name of the function
    ///
    /// If this is null, the function is anonymous
    name: ?[]const u8,
    span: Span,
    body: List(Sp(Word)),
    is_finished: bool,

    pub fn format(self: @This(), comptime s: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        const just_name = std.mem.eql(u8, s, "s");
        if (self.name) |name| {
            if (just_name) {
                try writer.print("{s}", .{name});
                return;
            } else {
                try writer.print("fn {s}", .{name});
            }
        } else {
            try writer.print("fn at {}", .{self.span.start});
            if (just_name) {
                return;
            }
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
    int: Int,
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
