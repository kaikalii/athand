const std = @import("std");
const compile = @import("compile.zig");
const Node = compile.Node;
const Func = compile.Func;
const Word = compile.Word;

pub const RuntimeError = error{
    NoMainFunction,
};

pub const Value = i64;

pub const Runtime = struct {
    data: compile.Compiled,
    stack: ?*Node(Value),

    pub fn init(data: compile.Compiled) Runtime {
        return Runtime{ .data = data, .stack = null };
    }

    pub fn start(self: *Runtime) !void {
        const main_fn = self.data.findFunction("main") orelse return error.NoMainFunction;
        return self.call(main_fn);
    }

    fn run(self: *Runtime) !void {
        _ = self;
    }

    fn call(self: *Runtime, func: *const Func) !void {
        if (func.body) |body| {
            try self.callImpl(body);
        } else {
            try self.run();
        }
    }

    fn callImpl(self: *Runtime, words: *const Node(Word)) !void {
        switch (words.val) {
            .int => |int| {
                var node = Node(Value){ .val = int, .next = self.stack };
                self.stack = &node;
            },
            .call => |ident| {
                std.debug.print("TODO: call ({s})\n", .{ident});
                std.process.exit(1);
            },
        }
        if (words.next) |next| {
            try self.callImpl(next);
        } else {
            try self.run();
        }
    }
};

pub const RBuiltins = struct {};
