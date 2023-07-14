const std = @import("std");

pub const CTy = enum {
    struc,
    field,
};

pub const CVal = union(CTy) {
    struc: Node(Struct),
    field: Node(Field),

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
