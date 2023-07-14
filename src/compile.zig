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
    stack: std.ArrayList(CVal),
    last_span: ?lex.Span,
    newest_struct: ?*Node(Struct),

    pub fn init(stack: []u8) Compiler {
        var alloc = std.heap.FixedBufferAllocator.init(stack);
        return Compiler{
            .stack = std.ArrayList(CVal).init(alloc.allocator()),
            .last_span = null,
            .newest_struct = null,
        };
    }

    fn push(self: *Compiler, val: CVal) CompileError!*CVal {
        var slot = self.stack.addOne() catch return error.StackOverflow;
        slot.* = val;
        return slot;
    }

    fn top(self: *Compiler) CompileError!*CVal {
        if (self.stack.items.len == 0) return error.StackUnderflow;
        return &self.stack.items[self.stack.items.len - 1];
    }

    fn pop(self: *Compiler) CompileError!CVal {
        return self.stack.pop() catch error.StackUnderflow;
    }

    pub fn compile(self: *Compiler, tokens: []const Sp(Token)) CompileError!void {
        tokens: for (tokens) |token| {
            self.last_span = token.span;
            switch (token.val) {
                Token.ident => |ident| {
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
        std.debug.print("stack:\n", .{});
        for (self.stack.items) |val| {
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
    };
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
