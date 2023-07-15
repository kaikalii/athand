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
    newest_struct: ?*Node(Struct),

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
                        CVal.struc => |*node| {
                            const struc = &node.val;
                            if (struc.name.len == 0) {
                                struc.name = ident;
                            } else {
                                struc.field_root = try self.newField(ident, struc, struc.field_root);
                            }
                        },
                        CVal.field => |*node| {
                            const field = &node.val;
                            if (field.ty == Ty.undefined) {
                                field.ty = Ty.fromStr(ident);
                            } else {
                                field.struc.field_root = try self.newField(ident, field.struc, node);
                            }
                        },
                        else => {
                            @panic("TODO: ident on non-struct");
                        },
                    }
                },
                Token.num => |num| _ = try self.push(CVal{ .num = num }),
            }
        }
        self.finishItem();
        std.debug.print("stack:\n", .{});
        for (self.stack[0..self.stack_size]) |val| {
            std.debug.print("{}\n", .{val});
        }
    }

    fn newField(self: *Compiler, name: []const u8, struc: *Struct, next: ?*Node(Field)) CompileError!*Node(Field) {
        const new_field = Field{
            .struc = struc,
            .name = name,
            .ty = Ty.undefined,
        };
        const new_node = Node(Field).init(new_field).withNext(next);
        const new_val = try self.push(CVal{ .field = new_node });
        return &new_val.field;
    }

    fn finishItem(self: *Compiler) void {
        const val = self.top() catch return;
        switch (val.*) {
            CVal.struc => |*node| if (node.val.field_root) |*head| {
                reverse_list(Field, head);
            },
            CVal.field => |node| if (node.val.struc.field_root) |*head| {
                reverse_list(Field, head);
            },
            else => {},
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
    pub fn @"struct"(comp: *Compiler) CompileError!void {
        const new_node = Node(Struct).init(Struct.init()).withNext(comp.newest_struct);
        const new_val = try comp.push(CVal{ .struc = new_node });
        comp.newest_struct = &new_val.struc;
    }

    pub fn @"fn"(comp: *Compiler) CompileError!void {
        _ = comp;
    }
};

pub const CTy = enum {
    struc,
    field,
    num,
};

pub const CVal = union(CTy) {
    struc: Node(Struct),
    field: Node(Field),
    num: []const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        switch (self) {
            CVal.struc => |snode| {
                try writer.print("struct {s}", .{snode.val.name});
                var field = snode.val.field_root;
                while (field) |fnode| {
                    try writer.print("\n  {}", .{fnode.val});
                    field = fnode.next;
                }
            },
            CVal.field => |node| try writer.print("field {}", .{node.val}),
            CVal.num => |num| try writer.print("num {s}", .{num}),
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

pub const Struct = struct {
    name: []const u8,
    field_root: ?*Node(Field),

    pub fn init() Struct {
        return .{ .name = "", .field_root = null };
    }
};

pub const Field = struct {
    struc: *Struct,
    name: []const u8,
    ty: Ty,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s} {}", .{ self.name, self.ty });
    }
};

pub const TyTag = enum { primitive, named, undefined };

pub const Ty = union(TyTag) {
    primitive: Primitive,
    named: []const u8,
    undefined,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return switch (self) {
            Ty.named => |name| writer.print("{s}", .{name}),
            Ty.primitive => |prim| writer.print("{}", .{prim}),
            Ty.undefined => writer.print("undefined", .{}),
        };
    }

    pub fn fromStr(s: []const u8) Ty {
        if (Primitive.fromStr(s)) |prim| {
            return .{ .primitive = prim };
        } else {
            return .{ .named = s };
        }
    }
};

pub const Primitive = enum {
    u8,
    u16,
    u32,
    u64,
    usize,
    i8,
    i16,
    i32,
    i64,
    isize,
    f32,
    f64,
    bool,
    void,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{s}", .{@tagName(self)});
    }

    pub fn fromStr(s: []const u8) ?Primitive {
        inline for (@typeInfo(Primitive).Enum.fields) |field| {
            if (std.mem.eql(u8, s, field.name)) {
                return @field(Primitive, field.name);
            }
        }
        return null;
    }
};
