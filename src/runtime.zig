const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");
const Node = compile.Node;
const List = compile.List;
const Func = compile.Func;
const Word = compile.Word;

pub const RuntimeError = error{
    NoMainFunction,
    StackUnderflow,
    UnknownFunction,
    UnresolvedFunction,
};

pub const Value = i64;

pub const trace_rt = @import("shared_ops").trace_rt;

pub const Runtime = struct {
    data: compile.Compiled,
    stack: List(Value),
    depth: usize,
    call_stack: List(Words),

    pub fn init(data: compile.Compiled) Runtime {
        return Runtime{
            .data = data,
            .stack = List(Value).init(),
            .depth = 0,
            .call_stack = List(Words).init(),
        };
    }

    pub fn start(self: *Runtime) RuntimeError!void {
        const main_fn = self.data.findFunction("main") orelse return error.NoMainFunction;
        try self.call(main_fn);
    }

    fn trace(self: *Runtime, comptime s: []const u8, args: anytype) void {
        if (!trace_rt)
            return;
        for (0..self.depth + 1) |_| {
            std.debug.print("  ", .{});
        }
        std.debug.print(s ++ "\n", args);
    }

    fn call(self: *Runtime, func: *const Func) RuntimeError!void {
        self.trace("call {s}", .{func});
        self.depth += 1;
        try self.execWords(func.body);
        self.depth -= 1;
        self.trace("return", .{});
    }

    fn execWords(self: *Runtime, words: Words) RuntimeError!void {
        var node = Node(Words).init(words);
        self.call_stack.push(&node);
        self.trace("push call stack {}", .{words});
        try self.execStack();
    }

    fn execStack(self: *Runtime) RuntimeError!void {
        self.trace("execStack stack: {}", .{self.stack});
        while (true) {
            if (self.call_stack.head) |call_node| {
                if (call_node.val.pop()) |word| {
                    try self.execWord(word.val);
                } else {
                    _ = self.call_stack.pop();
                    self.trace("pop call stack", .{});
                }
            } else {
                break;
            }
        }
    }

    fn execWord(self: *Runtime, word: Word) RuntimeError!void {
        self.trace("execWord stack:  {}", .{self.stack});
        switch (word) {
            .int => |int| {
                self.trace("push {}", .{int});
                var node = Node(Value).init(int);
                self.stack.push(&node);
                try self.execStack();
            },
            .builtin => |builtin| {
                self.trace("call {}", .{builtin});
                try builtin.f(self);
            },
            .call => |function| {
                return self.call(function.func orelse return error.UnresolvedFunction);
            },
            .quote => |function| {
                self.trace("quote {}", .{function});
            },
        }
    }

    fn pop(self: *Runtime) !Value {
        if (self.stack.pop()) |node| {
            return node.val;
        } else {
            return error.StackUnderflow;
        }
    }

    fn top(self: *Runtime) !*Value {
        if (self.stack.head) |node| {
            return &node.val;
        } else {
            return error.StackUnderflow;
        }
    }
};

pub const BuiltinFn = *const fn (*Runtime) RuntimeError!void;
pub const BuiltinFunction = struct {
    name: []const u8,
    f: BuiltinFn,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}", .{self.name});
    }
};
pub const CodeFunction = struct {
    name: ?[]const u8,
    span: lex.Span,
    func: ?*compile.Func,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        if (self.name) |name| {
            return writer.print("{s}", .{name});
        } else {
            return writer.print("fn at {}", .{self.span.start});
        }
    }
};
pub const Words = List(Word);

pub const RBuiltins = struct {
    pub fn dup(rt: *Runtime) RuntimeError!void {
        const val = try rt.pop();
        var a = Node(Value).init(val);
        var b = Node(Value).init(val);
        rt.stack.push(&a);
        rt.stack.push(&b);
        try rt.execStack();
    }
    pub fn drop(rt: *Runtime) RuntimeError!void {
        _ = try rt.pop();
    }
    pub fn @"+"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* += b;
    }
    pub fn @"-"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* -= b;
    }
    pub fn @"*"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* *= b;
    }
    pub fn @"/"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        rt.trace("div {} / {}", .{ a.*, b });
        a.* = @divTrunc(a.*, b);
    }
    pub fn @"=="(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* == b);
    }
    pub fn @"!="(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* != b);
    }
    pub fn @"<"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* < b);
    }
    pub fn @"<="(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* <= b);
    }
    pub fn @">"(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* > b);
    }
    pub fn @">="(rt: *Runtime) RuntimeError!void {
        const b = try rt.pop();
        var a = try rt.top();
        a.* = @intFromBool(a.* >= b);
    }
    pub fn print(rt: *Runtime) RuntimeError!void {
        const val = try rt.pop();
        std.debug.print("{d}", .{val});
    }
    pub fn println(rt: *Runtime) RuntimeError!void {
        const val = try rt.pop();
        std.debug.print("{d}\n", .{val});
    }
};
