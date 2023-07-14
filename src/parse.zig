const std = @import("std");
const lex = @import("lex.zig");
const Token = lex.Token;
const Sp = lex.Sp;

const ItemTy = enum {
    struc,
};

pub const Item = union(ItemTy) {
    struc: Struct,

    pub fn debug(self: Item) void {
        switch (self) {
            Item.struc => |struc| std.debug.print("struct {any}", .{@TypeOf(struc.name)}),
        }
    }
};

pub const Struct = struct {
    name: []const u8,
    fields: []Field,
};

pub const Field = struct {
    name: []const u8,
    ty: []const u8,
};

pub fn parse(tokens: []const Sp(Token), then: *const fn ([]Item) void) void {
    var parser = Parser{
        .tokens = tokens,
        .curr_token = 0,
        .items = undefined,
        .items_len = 0,
        .then = then,
    };
    parser.item() catch |err| parser.debugError(err);
}

const ParseError = error{
    UnexpectedToken,
    ExpectedToken,
};

const Parser = struct {
    tokens: []const Sp(Token),
    curr_token: usize,
    items: [1 << 10]Item,
    items_len: usize,
    then: *const fn ([]Item) void,

    fn currToken(self: *const Parser) ?Sp(Token) {
        if (self.curr_token >= self.tokens.len) {
            return null;
        }
        return self.tokens[self.curr_token];
    }

    fn takeExact(self: *Parser, ty: lex.TokenTy) bool {
        const token = self.currToken() orelse return false;
        const matches = token.val == ty;
        if (matches) {
            self.curr_token += 1;
        }
        return matches;
    }

    fn takeIdent(self: *Parser) ?[]const u8 {
        const token = self.currToken() orelse return null;
        switch (token.val) {
            Token.ident => |ident| {
                self.curr_token += 1;
                return ident;
            },
            else => return null,
        }
    }

    fn item(self: *Parser) ParseError!void {
        const parsed = try self.takeStruct();
        if (parsed) {
            try self.item();
        } else if (self.curr_token < self.tokens.len) {
            return error.UnexpectedToken;
        } else {
            self.then(self.items[0..self.items_len]);
        }
    }

    fn takeStruct(self: *Parser) ParseError!bool {
        if (!self.takeExact(Token.struc)) {
            return false;
        }
        const name = self.takeIdent() orelse return error.ExpectedToken;

        var fields: [8]Field = undefined;
        var fields_len: usize = 0;
        while (self.takeIdent()) |field_name| {
            const field_ty = self.takeIdent() orelse return error.ExpectedToken;
            fields[fields_len] = Field{ .name = field_name, .ty = field_ty };
            fields_len += 1;
        }

        self.items[self.items_len] = Item{ .struc = .{ .name = name } };
        self.items_len += 1;
        return true;
    }

    fn debugError(self: *Parser, err: ParseError) void {
        switch (err) {
            ParseError.UnexpectedToken => std.debug.print("unexpected token {s}", .{self.currToken().?.val.toStr()}),
            ParseError.ExpectedToken => std.debug.print("expected token", .{}),
        }
    }
};
