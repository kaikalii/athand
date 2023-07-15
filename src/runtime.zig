const std = @import("std");
const lex = @import("lex.zig");
const compile = @import("compile.zig");
const Node = compile.Node;
const Func = compile.Func;
const Word = compile.Word;

pub const RuntimeError = error{
    NoMainFunction,
    StackUnderflow,
    UnknownFunction,
    UnresolvedFunction,
};

pub const Value = i64;

pub const Runtime = struct {
    data: compile.Compiled,
    stack: ?*Node(Value),
    depth: usize,

    pub fn init(data: compile.Compiled) Runtime {
        return Runtime{ .data = data, .stack = null, .depth = 0 };
    }

    pub fn start(self: *Runtime) RuntimeError!void {
        const main_fn = self.data.findFunction("main") orelse return error.NoMainFunction;
        _ = try self.call(main_fn, null);
    }

    fn trace(self: *Runtime, comptime s: []const u8, args: anytype) void {
        for (0..self.depth + 1) |_| {
            std.debug.print("  ", .{});
        }
        std.debug.print(s, args);
    }

    fn call(self: *Runtime, func: *const Func, cont: Continue) RuntimeError!void {
        if (func.body) |body| {
            self.trace("call {}\n", .{func});
            self.depth += 1;
            try self.then(body);
            self.depth -= 1;
        }
        try self.then(cont);
    }

    fn then(self: *Runtime, cont: Continue) RuntimeError!void {
        if (self.stack) |stack| {
            self.trace("stack: {}\n", .{stack});
        } else {
            self.trace("stack: []\n", .{});
        }
        if (cont) |curr| {
            var next: Continue = curr.next;
            switch (curr.val) {
                .int => |int| {
                    self.trace("push {}\n", .{int});
                    var node = Node(Value){ .val = int, .next = self.stack };
                    self.stack = &node;
                },
                .builtin => |builtin| {
                    self.trace("call {}\n", .{builtin});
                    self.depth += 1;
                    next = try builtin.f(self, curr.next);
                    self.depth -= 1;
                },
                .call => |function| {
                    return self.call(function.func orelse return error.UnresolvedFunction, next);
                },
                .quote => |function| {
                    self.trace("quote {}\n", .{function});
                },
            }
            if (next) |nex| {
                try self.then(nex);
            }
        }
    }

    fn pop(self: *Runtime) !Value {
        if (self.stack) |node| {
            self.stack = node.next;
            return node.val;
        } else {
            return error.StackUnderflow;
        }
    }

    fn top(self: *Runtime) !*Value {
        if (self.stack) |node| {
            return &node.val;
        } else {
            return error.StackUnderflow;
        }
    }
};

pub const BuiltinFn = *const fn (*Runtime, Continue) RuntimeError!Continue;
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
pub const Continue = ?*const Node(Word);

pub const RBuiltins = struct {
    pub fn dup(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const val = try rt.pop();
        var a = Node(Value){ .val = val, .next = rt.stack };
        var b = Node(Value){ .val = val, .next = &a };
        rt.stack = &b;
        _ = try rt.then(cont);
        return null;
    }
    pub fn drop(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        _ = try rt.pop();
        return cont;
    }
    pub fn @"+"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* += a;
        return cont;
    }
    pub fn @"-"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* -= a;
        return cont;
    }
    pub fn @"*"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* *= a;
        return cont;
    }
    pub fn @"/"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @divExact(b.*, a);
        return cont;
    }
    pub fn @"=="(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* == a);
        return cont;
    }
    pub fn @"!="(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* != a);
        return cont;
    }
    pub fn @"<"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* < a);
        return cont;
    }
    pub fn @"<="(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* <= a);
        return cont;
    }
    pub fn @">"(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* > a);
        return cont;
    }
    pub fn @">="(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const a = try rt.pop();
        var b = try rt.top();
        b.* = @intFromBool(b.* >= a);
        return cont;
    }
    pub fn print(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const val = try rt.pop();
        std.debug.print("{d}", .{val});
        return cont;
    }
    pub fn println(rt: *Runtime, cont: Continue) RuntimeError!Continue {
        const val = try rt.pop();
        std.debug.print("{d}\n", .{val});
        return cont;
    }
};
