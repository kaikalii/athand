const std = @import("std");
const unicode = std.unicode;

pub fn lex(input: []const u8, output: []Sp(Token)) usize {
    var lexer = Lexer{
        .input = input,
        .inputIter = unicode.Utf8View.initUnchecked(input).iterator(),
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
    num,
};

pub const Token = union(TokenTy) {
    ident: []const u8,
    num: []const u8,

    pub fn toStr(self: *const Token) []const u8 {
        return switch (self.*) {
            Token.ident => |ident| ident,
            Token.num => |int| int,
        };
    }
};

pub const Loc = struct {
    line: usize,
    col: usize,
    pos: usize,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) std.os.WriteError!void {
        return writer.print("{}:{}", .{ self.line, self.col });
    }
};

pub const Span = struct {
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
    inputIter: unicode.Utf8Iterator,
    curr: Loc,
    tokens: []Sp(Token),
    token_count: usize,

    fn currChar(self: *Lexer) ?u8 {
        const peeked = self.inputIter.peek(1);
        if (peeked.len == 0) {
            return null;
        }
        return peeked[0];
    }

    fn nextIf(self: *Lexer, comptime f: fn ([]const u8) bool) ?[]const u8 {
        const cp = self.inputIter.peek(1);
        if (cp.len == 0) return null;
        if (!f(cp)) {
            return null;
        }
        switch (cp[0]) {
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
        _ = self.inputIter.nextCodepoint().?;
        self.curr.pos += cp.len;
        return cp;
    }

    fn next(self: *Lexer) ?[]const u8 {
        return self.nextIf(always);
    }
    fn always(_: []const u8) bool {
        return true;
    }

    fn go(self: *Lexer) LexError!void {
        while (true) {
            const start = self.curr;
            const cp = self.next() orelse break;
            switch (cp[0]) {
                ' ', '\t', '\r', '\n' => {},
                else => {
                    if (isIdentStart(cp)) {
                        // Identifiers
                        while (true) {
                            _ = self.nextIf(isIdentBody) orelse break;
                        }
                        const name = self.input[start.pos..self.curr.pos];
                        self.addToken(start, Token{ .ident = name });
                    } else if (isDigit(cp) or cp[0] == '-') {
                        // Integers
                        var got_digit = false;
                        while (true) {
                            _ = self.nextIf(isDigit) orelse break;
                            got_digit = true;
                        }
                        if (got_digit) {
                            const name = self.input[start.pos..self.curr.pos];
                            self.addToken(start, Token{ .num = name });
                        } else {
                            self.addToken(start, Token{ .ident = cp });
                        }
                    } else if (cp[0] > ' ') {
                        // Other characters
                        self.addToken(start, Token{ .ident = cp });
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

fn isIdentStart(cp: []const u8) bool {
    if (cp.len > 1) return true;
    const c = cp[0];
    return 'A' <= c and c <= 'Z' or 'a' <= c and c <= 'z' or c == '_';
}

fn isDigit(cp: []const u8) bool {
    return '0' <= cp[0] and cp[0] <= '9';
}

fn isIdentBody(c: []const u8) bool {
    return isIdentStart(c) or isDigit(c);
}
