const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");
const Sp = lex.Sp;
const Span = lex.Span;
const Node = compile.Node;
const List = compile.List;
const Func = compile.Func;
const Word = compile.Word;

pub const trace_rt = @import("shared_ops").trace_rt;

pub const RuntimeErrorKind = enum {
    no_main_function,
    stack_underflow,
    unresolved_function,
    mismatched_types,
};

pub const RuntimeErrorCause = union(RuntimeErrorKind) {
    no_main_function,
    stack_underflow,
    unresolved_function,
    mismatched_types: struct {
        expected: []const u8,
        actual: []const u8,
    },

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            .no_main_function => writer.print("no main function", .{}),
            .stack_underflow => writer.print("stack underflow", .{}),
            .unresolved_function => writer.print("unresolved function", .{}),
            .mismatched_types => |types| {
                return writer.print("mismatched types: expected {s}, got {s}", .{ types.expected, types.actual });
            },
        };
    }
};

pub const RuntimeError = struct {
    cause: RuntimeErrorCause,
    span: ?Span,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        try writer.print("{}", .{self.cause});
        if (self.span) |span| {
            try writer.print(" at {}", .{span.start});
        }
    }
};

pub const UnitError = error{err};

pub const Ty = enum {
    int,
    builtin_func,
    code_func,
};

pub const Int = i62;

pub const Value = union(Ty) {
    int: Int,
    builtin_func: *const BuiltinFunction,
    code_func: *const CodeFunction,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            .int => |int| try writer.print("{d}", .{int}),
            .builtin_func => |builtin| try writer.print("{}", .{builtin}),
            .code_func => |code| try writer.print("{}", .{code}),
        }
    }
};

pub const FunctionType = enum {
    builtin,
    code,
};

test "value size" {
    const size = @sizeOf(Value);
    const expected_size = std.math.ceilPowerOfTwo(usize, @sizeOf(Ty) + @sizeOf(Int));
    try std.testing.expectEqual(expected_size, size);
}

pub const Runtime = struct {
    data: compile.Compiled,
    stack: List(Value),
    depth: usize,
    call_stack: List(Words),
    err: ?RuntimeErrorCause,
    last_span: ?Span,

    pub fn init(data: compile.Compiled) Runtime {
        return Runtime{
            .data = data,
            .stack = List(Value).init(),
            .depth = 0,
            .call_stack = List(Words).init(),
            .err = null,
            .last_span = null,
        };
    }

    pub fn start(self: *Runtime) ?RuntimeError {
        const main_fn = self.data.findFunction("main") orelse {
            return .{
                .cause = .no_main_function,
                .span = null,
            };
        };
        self.call(main_fn) catch {};
        if (self.err) |err| {
            return .{
                .cause = err,
                .span = self.last_span,
            };
        }
        return null;
    }

    fn trace(self: *Runtime, comptime s: []const u8, args: anytype) void {
        if (!trace_rt)
            return;
        for (0..self.depth + 1) |_| {
            std.debug.print("  ", .{});
        }
        std.debug.print(s ++ "\n", args);
    }

    fn call(self: *Runtime, func: *const Func) UnitError!void {
        self.trace("call {s}", .{func});
        self.depth += 1;
        try self.execWords(func.body);
        self.depth -= 1;
        self.trace("return", .{});
    }

    fn execWords(self: *Runtime, words: Words) UnitError!void {
        var node = Node(Words).init(words);
        self.call_stack.push(&node);
        try self.execStack();
    }

    fn execStack(self: *Runtime) UnitError!void {
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

    fn execWord(self: *Runtime, word: Sp(Word)) UnitError!void {
        self.trace("stack: {}", .{self.stack});
        self.last_span = word.span;
        switch (word.val) {
            .int => |int| {
                self.trace("push {}", .{int});
                var node = Node(Value).init(.{ .int = int });
                self.stack.push(&node);
                try self.execStack();
            },
            .builtin => |builtin| {
                self.trace("call builtin {}", .{builtin});
                try builtin.f(self);
            },
            .call => |function| {
                return self.call(function.func orelse {
                    self.err = .unresolved_function;
                    return error.err;
                });
            },
            .quote => |*function| {
                self.trace("push {}", .{function});
                var node = Node(Value).init(.{ .code_func = function });
                self.stack.push(&node);
                try self.execStack();
            },
        }
    }

    fn popValue(self: *Runtime) UnitError!Value {
        if (self.stack.pop()) |node| {
            return node.val;
        } else {
            self.err = .stack_underflow;
            return error.err;
        }
    }

    fn pop(self: *Runtime, comptime T: type) UnitError!T {
        const val = try self.popValue();
        const type_name = switch (val) {
            .int => |int| if (T == Int) return int else "int",
            .builtin_func => |builtin| if (T == *const BuiltinFunction) return builtin else "function",
            .code_func => |code| if (T == *const CodeFunction) return code else "function",
        };
        self.err = .{ .mismatched_types = .{
            .expected = friendlyTypeName(T),
            .actual = type_name,
        } };
        return error.err;
    }

    fn topValue(self: *Runtime) UnitError!*Value {
        if (self.stack.head) |node| {
            return &node.val;
        } else {
            self.err = .stack_underflow;
            return error.err;
        }
    }

    fn topValue2(self: *Runtime) UnitError!struct { a: *Value, b: *Value } {
        if (self.stack.head) |b| {
            if (b.next) |a| {
                return .{ .a = &a.val, .b = &b.val };
            }
        }
        self.err = .stack_underflow;
        return error.err;
    }

    fn top(self: *Runtime, comptime T: type) UnitError!*T {
        const type_name = switch ((try self.topValue()).*) {
            .int => |*int| if (T == Int) return int else "int",
            .builtin_func => |*builtin| if (T == *const BuiltinFunction) return builtin else "function",
            .code_func => |*code| if (T == *const CodeFunction) return code else "function",
        };
        self.err = .{ .mismatched_types = .{
            .expected = friendlyTypeName(T),
            .actual = type_name,
        } };
        return error.err;
    }

    fn callTopValue(self: *Runtime) UnitError!void {
        switch ((try self.topValue()).*) {
            .int => {},
            else => try self.callValue(self.popValue() catch unreachable),
        }
    }

    fn callValue(self: *Runtime, val: Value) UnitError!void {
        switch (val) {
            .int => {
                var node = Node(Value).init(val);
                self.stack.push(&node);
                try self.execStack();
            },
            .builtin_func => |func| {
                self.trace("call builtin {}", .{func});
                try func.f(self);
            },
            .code_func => |func| {
                try self.call(func.func orelse {
                    self.err = .unresolved_function;
                    return error.err;
                });
            },
        }
    }
};

fn friendlyTypeName(comptime T: type) []const u8 {
    return switch (T) {
        Int => "int",
        *const BuiltinFunction => "function",
        *const CodeFunction => "function",
        else => @compileError("non-value type"),
    };
}

pub const BuiltinFn = *const fn (*Runtime) UnitError!void;
pub const BuiltinFunction = struct {
    name: []const u8,
    f: BuiltinFn,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}", .{self.name});
    }
};
pub const CodeFunction = struct {
    name: ?[]const u8,
    func: ?*compile.Func,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        if (self.name) |name| {
            return writer.print("{s}", .{name});
        } else if (self.func) |func| {
            return writer.print("fn at {}", .{func.span.start});
        } else {
            return writer.print("fn", .{});
        }
    }
};
pub const Words = List(Sp(Word));

pub const RBuiltins = struct {
    pub fn @"true"(rt: *Runtime) UnitError!void {
        var node = Node(Value).init(.{ .int = 1 });
        rt.stack.push(&node);
        try rt.execStack();
    }
    pub fn @"false"(rt: *Runtime) UnitError!void {
        var node = Node(Value).init(.{ .int = 0 });
        rt.stack.push(&node);
        try rt.execStack();
    }
    pub fn call(rt: *Runtime) UnitError!void {
        try rt.callTopValue();
    }
    pub fn @"if"(rt: *Runtime) UnitError!void {
        const if_false = try rt.popValue();
        const if_true = try rt.popValue();
        const cond = try rt.pop(Int);
        rt.trace("if {} {} {}", .{ cond, if_true, if_false });
        if (cond == 0) {
            try rt.callValue(if_false);
        } else {
            try rt.callValue(if_true);
        }
    }
    pub fn dup(rt: *Runtime) UnitError!void {
        const val = try rt.topValue();
        var duped = Node(Value).init(val.*);
        rt.stack.push(&duped);
        try rt.execStack();
    }
    pub fn swap(rt: *Runtime) UnitError!void {
        const pair = try rt.topValue2();
        std.mem.swap(Value, pair.a, pair.b);
    }
    pub fn over(rt: *Runtime) UnitError!void {
        const pair = try rt.topValue2();
        var duped = Node(Value).init(pair.a.*);
        rt.stack.push(&duped);
        try rt.execStack();
    }
    pub fn debug(rt: *Runtime) UnitError!void {
        std.debug.print("{}", .{rt.stack});
    }
    pub fn drop(rt: *Runtime) UnitError!void {
        _ = try rt.popValue();
    }
    pub fn @"+"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* += b;
    }
    pub fn @"-"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* -= b;
    }
    pub fn @"*"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* *= b;
    }
    pub fn @"/"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @divTrunc(a.*, b);
    }
    pub fn @"%"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @mod(a.*, b);
    }
    pub fn @"=="(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* == b);
    }
    pub fn @"!="(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* != b);
    }
    pub fn @"<"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* < b);
    }
    pub fn @"<="(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* <= b);
    }
    pub fn @">"(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* > b);
    }
    pub fn @">="(rt: *Runtime) UnitError!void {
        const b = try rt.pop(Int);
        var a = try rt.top(Int);
        a.* = @intFromBool(a.* >= b);
    }
    pub fn print(rt: *Runtime) UnitError!void {
        const val = try rt.popValue();
        std.debug.print("{}", .{val});
    }
    pub fn println(rt: *Runtime) UnitError!void {
        const val = try rt.popValue();
        std.debug.print("{}\n", .{val});
    }
};
