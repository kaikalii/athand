const std = @import("std");

pub fn lex(input: []const u8, output: []Sp(Token)) usize {
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

pub const TokenTy = enum {
    ident,
    int,
    open_paren,
    close_paren,
    open_curly,
    close_curly,
    open_bracket,
    close_bracket,
    plus,
    minus,
    star,
    slash,
    colon,
    semicolon,
    period,
    comma,
    struc,
};

pub const Token = union(TokenTy) {
    ident: []const u8,
    int: []const u8,
    open_paren,
    close_paren,
    open_curly,
    close_curly,
    open_bracket,
    close_bracket,
    plus,
    minus,
    star,
    slash,
    colon,
    semicolon,
    period,
    comma,
    struc,

    pub fn toStr(self: Token) []const u8 {
        return switch (self) {
            Token.ident => |ident| ident,
            Token.int => |int| int,
            Token.open_paren => "(",
            Token.close_paren => ")",
            Token.open_curly => "{",
            Token.close_curly => "}",
            Token.open_bracket => "[",
            Token.close_bracket => "]",
            Token.plus => "+",
            Token.minus => "-",
            Token.star => "*",
            Token.slash => "/",
            Token.colon => ":",
            Token.semicolon => ";",
            Token.period => ".",
            Token.comma => ",",
            Token.struc => "struct",
        };
    }
};

const Loc = struct {
    line: u32,
    col: u32,
    pos: u32,
};

const Span = struct {
    start: Loc,
    end: Loc,
};

pub fn Sp(comptime T: type) type {
    return struct {
        val: T,
        span: Span,
    };
}

const LexError = error{InvalidChar};

const Lexer = struct {
    input: []const u8,
    curr: Loc,
    tokens: []Sp(Token),
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
                '(' => self.addToken(start, Token.open_paren),
                ')' => self.addToken(start, Token.close_paren),
                '{' => self.addToken(start, Token.open_curly),
                '}' => self.addToken(start, Token.close_curly),
                '[' => self.addToken(start, Token.open_bracket),
                ']' => self.addToken(start, Token.close_bracket),
                '+' => self.addToken(start, Token.plus),
                '-' => self.addToken(start, Token.minus),
                '*' => self.addToken(start, Token.star),
                '/' => self.addToken(start, Token.slash),
                ':' => self.addToken(start, Token.colon),
                ';' => self.addToken(start, Token.semicolon),
                '.' => self.addToken(start, Token.period),
                ',' => self.addToken(start, Token.comma),
                ' ', '\t', '\r', '\n' => {},
                else => {
                    if (isIdentStart(c)) {
                        // Identifiers and keywords
                        while (true) {
                            _ = self.nextIf(isIdentBody) orelse break;
                        }
                        const name = self.input[start.pos..self.curr.pos];
                        if (std.mem.eql(u8, name, "struct")) {
                            self.addToken(start, Token.struc);
                        } else {
                            self.addToken(start, Token{ .ident = name });
                        }
                    } else if (isDigit(c)) {
                        // Integers
                        while (true) {
                            _ = self.nextIf(isDigit) orelse break;
                        }
                        const name = self.input[start.pos..self.curr.pos];
                        self.addToken(start, Token{ .int = name });
                    } else {
                        // Invalid character
                        self.curr = start;
                        return LexError.InvalidChar;
                    }
                },
            }
        }
    }

    fn addToken(self: *Lexer, start: Loc, token: Token) void {
        self.tokens[self.token_count] = .{
            .val = token,
            .span = .{
                .start = start,
                .end = self.curr,
            },
        };
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
