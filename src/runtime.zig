const std = @import("std");
const compile = @import("compile.zig");
const Node = compile.Node;
const Func = compile.Func;
const Word = compile.Word;

pub const RuntimeError = error{
    NoMainFunction,
    StackUnderflow,
    UnknownFunction,
};

pub const Value = i64;

pub const Runtime = struct {
    data: compile.Compiled,
    stack: ?*Node(Value),

    pub fn init(data: compile.Compiled) Runtime {
        return Runtime{ .data = data, .stack = null };
    }

    pub fn start(self: *Runtime) RuntimeError!void {
        const main_fn = self.data.findFunction("main") orelse return error.NoMainFunction;
        _ = try self.call(main_fn, null);
    }

    fn run(self: *Runtime) RuntimeError!void {
        _ = self;
    }

    fn call(self: *Runtime, func: *const Func, cont: Continue) RuntimeError!void {
        if (func.body) |body| {
            try self.then(body);
        }
        if (cont) |next| {
            try self.then(next);
        }
    }

    fn then(self: *Runtime, cont: Continue) RuntimeError!void {
        if (self.stack) |stack| {
            std.debug.print("  stack: {}\n", .{stack});
        } else {
            std.debug.print("  stack: []\n", .{});
        }
        if (cont) |curr| {
            var next: Continue = curr.next;
            switch (curr.val) {
                .int => |int| {
                    std.debug.print("  push {}\n", .{int});
                    var node = Node(Value){ .val = int, .next = self.stack };
                    self.stack = &node;
                },
                .builtin => |builtin| {
                    std.debug.print("  call {}\n", .{builtin});
                    next = try builtin.f(self, curr.next);
                },
                .call => |ident| {
                    std.debug.print("  call {s}\n", .{ident});
                    const func = self.data.findFunction(ident) orelse return error.UnknownFunction;
                    return self.call(func, next);
                },
            }
            if (next) |nex| {
                try self.then(nex);
            } else {
                std.debug.print("  return\n", .{});
                try self.run();
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

pub const BuiltinFn = struct {
    name: []const u8,
    f: *const fn (*Runtime, Continue) RuntimeError!Continue,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}", .{self.name});
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
