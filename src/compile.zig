const std = @import("std");
const lex = @import("lex.zig");
const Token = lex.Token;
const Sp = lex.Sp;
const eql = std.mem.eql;

const CompileError = error{
    StackOverflow,
    StackUnderflow,
};

pub const Compiler = struct {
    stack: [1024]CVal,
    stack_size: usize,
    last_span: ?lex.Span,
    newest_func: ?*Node(Func),

    pub fn init() Compiler {
        return Compiler{
            .stack = undefined,
            .stack_size = 0,
            .last_span = null,
            .newest_func = null,
        };
    }

    fn push(self: *Compiler, val: CVal) CompileError!*CVal {
        if (self.stack_size == self.stack.len) {
            return error.StackOverflow;
        }
        const ptr = &self.stack[self.stack_size];
        ptr.* = val;
        self.stack_size += 1;
        return ptr;
    }

    fn top(self: *Compiler) CompileError!*CVal {
        if (self.stack_size == 0) {
            return error.StackUnderflow;
        }
        return &self.stack[self.stack_size - 1];
    }

    fn pop(self: *Compiler) CompileError!CVal {
        if (self.stack_size == 0) {
            return error.StackUnderflow;
        }
        self.stack_size -= 1;
        return self.stack[self.stack_size];
    }

    pub fn compile(self: *Compiler, tokens: []const Sp(Token)) CompileError!void {
        tokens: for (tokens) |token| {
            self.last_span = token.span;
            switch (token.val) {
                Token.ident => |ident| {
                    // Builtins
                    inline for (@typeInfo(Builtins).Struct.decls) |builtin| {
                        if (eql(u8, builtin.name, ident)) {
                            self.finishItem();
                            try @field(Builtins, builtin.name)(self);
                            continue :tokens;
                        }
                    }
                    // Others
                    const val = try self.top();
                    switch (val.*) {
                        CVal.func => |*node| {
                            const func = &node.val;
                            if (func.name.len == 0) {
                                func.name = ident;
                            } else {
                                func.body = try self.addWord(func, Word{ .call = ident });
                            }
                        },
                        CVal.word => {
                            if (self.newest_func) |node| {
                                const func = &node.val;
                                func.body = try self.addWord(func, Word{ .call = ident });
                            }
                        },
                    }
                },
                Token.num => |num| {
                    if (self.newest_func) |node| {
                        const func = &node.val;
                        func.body = try self.addWord(func, Word{ .num = num });
                    }
                },
            }
        }
        self.finishItem();
        for (self.stack[0..self.stack_size]) |val| {
            std.debug.print("{}\n", .{val});
        }
    }

    fn addWord(self: *Compiler, func: *Func, word: Word) CompileError!*Node(Word) {
        const new_node = Node(Word).init(word).withNext(func.body);
        const new_val = try self.push(CVal{ .word = new_node });
        func.body = &new_val.word;
        return &new_val.word;
    }

    fn finishItem(self: *Compiler) void {
        const val = self.top() catch return;
        switch (val.*) {
            CVal.func, CVal.word => if (self.newest_func) |node|
                if (node.val.body) |*head|
                    reverse_list(Word, head),
        }
    }

    pub fn debugError(self: *Compiler, err: CompileError) void {
        std.debug.print("error", .{});
        if (self.last_span) |span|
            std.debug.print(" at {}:{}", .{ span.start.line, span.start.col });
        std.debug.print(": ", .{});
        switch (err) {
            error.StackOverflow => std.debug.print("compile stack overflow\n", .{}),
            error.StackUnderflow => std.debug.print("compile stack underflow\n", .{}),
        }
    }
};

const Builtins = struct {
    pub fn @"fn"(comp: *Compiler) CompileError!void {
        const new_node = Node(Func).init(Func{ .name = "", .body = null });
        const new_val = try comp.push(CVal{ .func = new_node });
        comp.newest_func = &new_val.func;
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

        pub fn init(val: T) Node(T) {
            return .{ .val = val, .next = null };
        }
        pub fn withNext(self: Node(T), next: ?*Node(T)) Node(T) {
            return .{ .val = self.val, .next = next };
        }

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
