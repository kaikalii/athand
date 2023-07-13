const std = @import("std");

pub fn lex(input: []const u8, output: []Token) usize {
    var lexer = Lexer{
        .input = input,
        .curr = Loc{
            .line = 1,
            .col = 1,
            .pos = 0,
        },
        .tokens = output,
        .token_count = 0,
    };
    lexer.go() catch |err| {
        lexer.debugError(err);
        std.os.exit(1);
    };
    return lexer.token_count;
}

pub const TokenTy = enum { ident, int, open_paren, close_paren, plus, minus, star, slash, eof };

pub const Token = union(TokenTy) {
    ident: []const u8,
    int: []const u8,
    open_paren: void,
    close_paren: void,
    plus: void,
    minus: void,
    star: void,
    slash: void,
    eof: void,

    pub fn toStr(self: Token) []const u8 {
        return switch (self) {
            TokenTy.ident => |ident| ident,
            TokenTy.int => |int| int,
            TokenTy.open_paren => "(",
            TokenTy.close_paren => ")",
            TokenTy.plus => "+",
            TokenTy.minus => "-",
            TokenTy.star => "*",
            TokenTy.slash => "/",
            TokenTy.eof => "EOF",
        };
    }
};

const Loc = struct {
    line: u32,
    col: u32,
    pos: u32,
};

const LexError = error{InvalidChar};

const Lexer = struct {
    input: []const u8,
    curr: Loc,
    tokens: []Token,
    token_count: u32,

    fn currChar(self: *Lexer) ?u8 {
        if (self.curr.pos >= self.input.len) {
            return null;
        }
        return self.input[self.curr.pos];
    }

    fn nextIf(self: *Lexer, comptime f: fn (u8) bool) ?u8 {
        const c = self.currChar() orelse return null;
        if (!f(c)) {
            return null;
        }
        switch (c) {
            '\n' => {
                self.curr.line += 1;
                self.curr.col = 1;
            },
            '\r' => {},
            '\t' => {
                self.curr.col += 4;
            },
            else => {
                self.curr.col += 1;
            },
        }
        self.curr.pos += 1;
        return c;
    }

    fn next(self: *Lexer) ?u8 {
        return self.nextIf(always);
    }
    fn always(_: u8) bool {
        return true;
    }

    fn go(self: *Lexer) LexError!void {
        while (true) {
            const start = self.curr;
            const c = self.next() orelse break;
            switch (c) {
                '(' => self.addToken(Token{ .open_paren = undefined }),
                ')' => self.addToken(Token{ .close_paren = undefined }),
                '+' => self.addToken(Token{ .plus = undefined }),
                '-' => self.addToken(Token{ .minus = undefined }),
                '*' => self.addToken(Token{ .star = undefined }),
                '/' => self.addToken(Token{ .slash = undefined }),
                ' ' => {},
                else => {
                    if (isIdentStart(c)) {
                        while (true) {
                            _ = self.nextIf(isIdentBody) orelse break;
                        }
                        const name = self.input[start.pos..self.curr.pos];
                        self.addToken(Token{ .ident = name });
                    } else if (isDigit(c)) {
                        while (true) {
                            _ = self.nextIf(isDigit) orelse break;
                        }
                        const name = self.input[start.pos..self.curr.pos];
                        self.addToken(Token{ .int = name });
                    } else {
                        self.curr = start;
                        return LexError.InvalidChar;
                    }
                },
            }
        }
    }

    fn addToken(self: *Lexer, token: Token) void {
        self.tokens[self.token_count] = token;
        self.token_count += 1;
    }

    fn debugError(self: *Lexer, err: LexError) void {
        std.debug.print("error at {}:{}  ", .{ self.curr.line, self.curr.col });
        switch (err) {
            LexError.InvalidChar => if (self.currChar()) |c| {
                std.debug.print("invalid character '{c}'\n", .{c});
            },
        }
    }
};

fn isIdentStart(c: u8) bool {
    return 'A' <= c and c <= 'Z' or 'a' <= c and c <= 'z' or c == '_';
}

fn isDigit(c: u8) bool {
    return '0' <= c and c <= '9';
}

fn isIdentBody(c: u8) bool {
    return isIdentStart(c) or isDigit(c);
}
