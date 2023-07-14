const std = @import("std");

pub const CTy = enum {
    struc,
    field,
};

pub const CVal = union(CTy) {
    struc: Node(Struct),
    field: Node(Field),

    pub fn debug(self: *const CVal) void {
        switch (self.*) {
            CVal.struc => |snode| {
                std.debug.print("struct ", .{});
                std.debug.print("{s}", .{snode.val.name});
                var field = snode.val.field_root;
                while (field) |fnode| {
                    std.debug.print("\n  ", .{});
                    fnode.val.debug();
                    field = fnode.next;
                }
            },
            CVal.field => |node| {
                std.debug.print("field ", .{});
                node.val.debug();
            },
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

    pub fn debug(self: *const Field) void {
        std.debug.print("{s} ", .{self.name});
        self.ty.debug();
    }
};

pub const TyTag = enum { named, undefined };

pub const Ty = union(TyTag) {
    named: []const u8,
    undefined,

    pub fn debug(self: *const Ty) void {
        switch (self.*) {
            Ty.named => |name| std.debug.print("{s}", .{name}),
            Ty.undefined => std.debug.print("undefined", .{}),
        }
    }
};
