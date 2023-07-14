const std = @import("std");
const value = @import("value.zig");
const lex = @import("lex.zig");
const CVal = value.CVal;
const Ty = value.Ty;
const Node = value.Node;
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
    newest_struct: ?*Node(value.Struct),

    pub fn init() Compiler {
        return Compiler{
            .stack = undefined,
            .stack_size = 0,
            .last_span = null,
            .newest_struct = null,
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
            std.debug.print("token: {s}\n", .{token.val.toStr()});
            switch (token.val) {
                Token.alphanum => |ident| {
                    // Builtins
                    inline for (@typeInfo(Builtins).Struct.decls) |builtin| {
                        if (eql(u8, builtin.name, ident)) {
                            try @field(Builtins, builtin.name)(self);
                            continue :tokens;
                        }
                    }
                    // Others
                    const val = try self.top();
                    switch (val.*) {
                        CVal.struc => |*node| {
                            const struc = &node.val;
                            if (struc.name.len == 0) {
                                struc.name = ident;
                            } else {
                                const field = value.Field{
                                    .struc = &node.val,
                                    .name = ident,
                                    .ty = Ty.undefined,
                                };
                                const new_node = Node(value.Field).init(field).withNext(struc.field_root);
                                const new_val = try self.push(CVal{ .field = new_node });
                                struc.field_root = &new_val.*.field;
                            }
                        },
                        CVal.field => |*node| {
                            const field = &node.val;
                            if (field.ty == Ty.undefined) {
                                field.ty = Ty{ .named = ident };
                            } else {
                                const new_field = value.Field{
                                    .struc = field.struc,
                                    .name = ident,
                                    .ty = Ty.undefined,
                                };
                                const new_node = Node(value.Field).init(new_field).withNext(node);
                                const new_val = try self.push(CVal{ .field = new_node });
                                field.struc.field_root = &new_val.*.field;
                            }
                        },
                    }
                },
                Token.num => {},
                Token.other => {},
            }
        }
        std.debug.print("\nstack:\n", .{});
        for (self.stack[0..self.stack_size]) |val| {
            val.debug();
            std.debug.print("\n", .{});
        }
    }

    pub fn debugError(self: *Compiler, err: CompileError) void {
        std.debug.print("error", .{});
        if (self.last_span) |span|
            std.debug.print(" at {}:{}", .{ span.start.line, span.start.col });
        std.debug.print(": ", .{});
        switch (err) {
            error.StackOverflow => std.debug.print("stack overflow\n", .{}),
            error.StackUnderflow => std.debug.print("stack underflow\n", .{}),
        }
    }
};

const Builtins = struct {
    pub fn @"struct"(comp: *Compiler) CompileError!void {
        const new_node = Node(value.Struct).init(value.Struct.init()).withNext(comp.newest_struct);
        const new_val = try comp.push(CVal{ .struc = new_node });
        comp.newest_struct = &new_val.*.struc;
    }

    pub fn @"fn"(comp: *Compiler) CompileError!void {
        _ = comp;
    }
};
