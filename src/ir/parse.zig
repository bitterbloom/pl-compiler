const std = @import("std");
const ir = @import("./types.zig");

pub const TokenKind = union(enum) {
    // Keywords
    tmp: void,
    let: void,
    @"var": void,
    def: void,
    @"extern": void,
    @"export": void,
    module: void,
    // Instructions
    jump: void,
    branch: void,
    @"if": void,
    @"else": void,
    // Operators
    @"+ub": void,
    @"-ub": void,
    @"*ub": void,
    @"u>": void,
    // Symbols
    @"=": void,
    @",": void,
    @"[": void,
    @"]": void,
    @"{": void,
    @"}": void,
    @"(": void,
    @")": void,
    @"...": void,
    // Identifiers
    gbl_id: [*:0]const u8,
    lcl_id: [*:0]const u8,
    lbl_id: [*:0]const u8,
    mod_id: [*:0]const u8,
    word: [*:0]const u8,
    // Constants
    int: u64,
    float: f64,
    str: [*:0]const u8,
    // Other
    eof: void,
    invalid: [*:0]const u8,
};

pub const Location = struct {
    line: u32,
    char: u32,
};

pub const Token = struct {
    kind: TokenKind,
    location: Location,
};

pub const Tokenizer = struct {
    source: []const u8,
    word_start: usize,
    location: Location,
    alloc: std.mem.Allocator,

    pub fn init(source: []const u8, alloc: std.mem.Allocator) Tokenizer {
        return .{
            .source = source,
            .word_start = 0,
            .location = .{.line = 1, .char = 0},
            .alloc = alloc,
        };
    }

    const State = enum {
        is_empty,
        is_global,
        is_local,
        is_label,
        is_module,
        is_string,
        is_number,
        is_decimal,
        is_comment,
        is_word,
    };

    pub fn tokenize(self: *Tokenizer) !Token {

        if (self.word_start >= self.source.len) {
            return self.withLocation(.{.eof = void{}}, self.word_start);
        }

        var token: Token = undefined;
        var state = State.is_empty;
        var index = self.word_start;

        make_token: {
            var char = self.source[index];

            // `index` (and `location`) are incremented after each loop iteration.
            // All branches that breaks the loop also sets the `word_start` accordingly.

            while (true) {
                next_char: {
                    switch (state) {
                        .is_empty => switch (char) {
                            '\n' => {
                                index += 1;
                                self.location.line += 1;
                                self.location.char = 0;
                                break :next_char;
                            },
                            '$' => {
                                state = State.is_global;
                                self.word_start = index + 1;
                            },
                            '%' => {
                                state = State.is_local;
                                self.word_start = index + 1;
                            },
                            '@' => {
                                state = State.is_label;
                                self.word_start = index + 1;
                            },
                            '#' => {
                                state = State.is_module;
                                self.word_start = index + 1;
                            },
                            '"' => {
                                state = State.is_string;
                                self.word_start = index + 1;
                            },
                            ';' => {
                                state = State.is_comment;
                            },
                            else => {
                                if (std.ascii.isDigit(char)) {
                                    state = State.is_number;
                                    self.word_start = index; // don't skip the digit
                                }
                                else if (std.ascii.isWhitespace(char)) {
                                    // do nothing
                                }
                                else if (isDelimiter(char)) {
                                    token = self.withLocation(fromDelimiter(char), index);
                                    self.word_start = index + 1;
                                    break :make_token;
                                }
                                else {
                                    state = State.is_word;
                                    self.word_start = index;
                                }
                            },
                        },
                        .is_global, .is_local,
                        .is_label, .is_module => switch (char) {
                            '\n' => {
                                const word = try self.allocWord(index);
                                token = self.withLocation(switch (state) {
                                    .is_global => .{.gbl_id = word},
                                    .is_local =>  .{.lcl_id = word},
                                    .is_label =>  .{.lbl_id = word},
                                    .is_module => .{.mod_id = word},
                                    else => unreachable,
                                }, index);
                                self.location.line += 1;
                                self.location.char = 0;
                                self.word_start = index + 1;
                                break :make_token;
                            },
                            else => {
                                if (std.ascii.isWhitespace(char)) {
                                    const word = try self.allocWord(index);
                                    token = self.withLocation(switch (state) {
                                        .is_global => .{.gbl_id = word},
                                        .is_local =>  .{.lcl_id = word},
                                        .is_label =>  .{.lbl_id = word},
                                        .is_module => .{.mod_id = word},
                                        else => unreachable,
                                    }, index);
                                    self.location.char += 1;
                                    self.word_start = index + 1;
                                    break :make_token;
                                }
                                else if (isDelimiter(char)) {
                                    const word = try self.allocWord(index);
                                    token = self.withLocation(switch (state) {
                                        .is_global => .{.gbl_id = word},
                                        .is_local =>  .{.lcl_id = word},
                                        .is_label =>  .{.lbl_id = word},
                                        .is_module => .{.mod_id = word},
                                        else => unreachable,
                                    }, index);
                                    self.word_start = index; // don't consume char
                                    break :make_token;
                                }
                                else {
                                    // do nothing
                                }
                            },
                        },
                        .is_string => switch (char) {
                            // TODO: Add support for escape sequences
                            '\n' => {
                                @panic("todo");
                            },
                            '"' => {
                                const word = try self.allocWord(index);
                                token = self.withLocation(.{.str = word}, index);
                                self.location.char += 1;
                                self.word_start = index + 1;
                                break :make_token;
                            },
                            else => {
                                // do nothing
                            },
                        },
                        .is_number => switch (char) {
                            '.' => {
                                state = State.is_decimal;
                            },
                            else => {
                                if (std.ascii.isDigit(char)) {
                                    // do nothing
                                }
                                else if (std.ascii.isWhitespace(char)) {
                                    const number = try std.fmt.parseUnsigned(u64, self.source[self.word_start..index], 10);
                                    token = self.withLocation(.{.int = number}, index);
                                    self.location.char += 1;
                                    self.word_start = index + 1;
                                    break :make_token;
                                }
                                else if (isDelimiter(char)) {
                                    const number = try std.fmt.parseUnsigned(u64, self.source[self.word_start..index], 10);
                                    token = self.withLocation(.{.int = number}, index);
                                    self.word_start = index; // don't consume char
                                    break :make_token;
                                }
                                else {
                                    // TODO: Add support for specifying type of number literal
                                    @panic("todo");
                                }
                            },
                        },
                        .is_decimal => switch (char) {
                            else => {
                                if (std.ascii.isDigit(char)) {
                                    // do nothing
                                }
                                else if (std.ascii.isWhitespace(char)) {
                                    const number = try std.fmt.parseFloat(f64, self.source[self.word_start..index]);
                                    token = self.withLocation(.{.float = number}, index);
                                    self.location.char += 1;
                                    self.word_start = index + 1;
                                    break :make_token;
                                }
                                else if (isDelimiter(char)) {
                                    const number = try std.fmt.parseFloat(f64, self.source[self.word_start..index]);
                                    token = self.withLocation(.{.float = number}, index);
                                    self.word_start = index; // don't consume char
                                    break :make_token;
                                }
                                else {
                                    // TODO: Add support for specifying type of number literal
                                    @panic("todo");
                                }
                            },
                        },
                        .is_comment => switch (char) {
                            '\n' => {
                                state = State.is_empty;
                                self.location.line += 1;
                                self.location.char = 0;
                                self.word_start = index + 1;
                            },
                            else => {
                                // do nothing
                            },
                        },
                        .is_word => switch (char) {
                            '\n' => {
                                const keyword_token_opt = self.fromKeyword(index);
                                if (keyword_token_opt) |keyword_token| {
                                    token = keyword_token;
                                }
                                else {
                                    const word = try self.allocWord(index);
                                    token = self.withLocation(.{.word = word}, index);
                                }
                                self.location.line += 1;
                                self.location.char = 0;
                                self.word_start = index + 1;
                                break :make_token;
                            },
                            else => {
                                if (std.ascii.isWhitespace(char)) {
                                    const keyword_token_opt = self.fromKeyword(index);
                                    if (keyword_token_opt) |keyword_token| {
                                        token = keyword_token;
                                    }
                                    else {
                                        const word = try self.allocWord(index);
                                        token = self.withLocation(.{.word = word}, index);
                                    }
                                    self.location.char += 1;
                                    self.word_start = index + 1;
                                    break :make_token;
                                }
                                else if (isDelimiter(char)) {
                                    const keyword_token_opt = self.fromKeyword(index);
                                    if (keyword_token_opt) |keyword_token| {
                                        token = keyword_token;
                                    }
                                    else {
                                        const word = try self.allocWord(index);
                                        token = self.withLocation(.{.word = word}, index);
                                    }
                                    self.word_start = index; // don't consume char
                                    break :make_token;
                                }
                                else {
                                    // do nothing
                                }
                            },
                        },
                    }

                    index += 1;
                    self.location.char += 1;
                }

                if (index >= self.source.len) {
                    self.word_start = index;
                    break;
                }
                char = self.source[index];
                continue;
            }

            // When we reach the end of the source
            switch (state) {
                .is_empty => {
                    token = self.withLocation(.{.eof = void{}}, index);
                    break :make_token;
                },
                .is_global => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.gbl_id = word}, index);
                    break :make_token;
                },
                .is_local => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.lcl_id = word}, index);
                    break :make_token;
                },
                .is_label => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.lbl_id = word}, index);
                    break :make_token;
                },
                .is_module => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.mod_id = word}, index);
                    break :make_token;
                },
                .is_word => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.word = word}, index);
                    break :make_token;
                },
                .is_string => {
                    const word = try self.allocWord(index);
                    token = self.withLocation(.{.invalid = word}, index);
                    break :make_token;
                },
                .is_number => {
                    token = self.withLocation(.{.int = @panic("todo")}, index);
                    break :make_token;
                },
                .is_decimal => {
                    token = self.withLocation(.{.float = @panic("todo")}, index);
                    break :make_token;
                },
                .is_comment => {
                    token = self.withLocation(.{.eof = void{}}, index);
                    break :make_token;
                },
            }

            std.debug.panic("Unreachable");
        }
        
        return token;
    }

    fn withLocation(self: *const Tokenizer, token_kind: TokenKind, word_end: usize) Token {
        _ = word_end;
        // if (self.location.char < word_end - self.word_start)
        //     std.debug.panic("self.location.char: {d}, word_end: {d}, self.word_start: {d}", .{self.location.char, word_end, self.word_start});
        return .{
            .kind = token_kind,
            .location = .{
                .line = self.location.line,
                .char = self.location.char, //@intCast(self.location.char - (word_end - self.word_start))
            },
        };
    }

    fn isDelimiter(c: u8) bool {
        return switch (c) {
            '[', ']',
            '{', '}',
            '(', ')',
            ',' => true,
            else => false,
        };
    }

    fn fromDelimiter(c: u8) TokenKind {
        return switch (c) {
            '[' => TokenKind.@"[",
            ']' => TokenKind.@"]",
            '{' => TokenKind.@"{",
            '}' => TokenKind.@"}",
            '(' => TokenKind.@"(",
            ')' => TokenKind.@")",
            ',' => TokenKind.@",",
            else => std.debug.panic("Not a delimiter: '{}'", .{c}),
        };
    }

    fn allocWord(self: *const Tokenizer, word_end: usize) ![*:0]u8 {
        return try self.alloc.dupeZ(u8, self.source[self.word_start..word_end]);
    }

    fn fromKeyword(self: *const Tokenizer, word_end: usize) ?Token {
        return if (std.mem.eql(u8, self.source[self.word_start..word_end],    "tmp")) self.withLocation(.{     .tmp  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "let")) self.withLocation(.{     .let  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "var")) self.withLocation(.{   .@"var" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "def")) self.withLocation(.{     .def  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "extern")) self.withLocation(.{.@"extern" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "export")) self.withLocation(.{.@"export" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "module")) self.withLocation(.{  .module  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],   "jump")) self.withLocation(.{    .jump  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "branch")) self.withLocation(.{  .branch  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],     "if")) self.withLocation(.{    .@"if" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],   "else")) self.withLocation(.{  .@"else" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "+ub")) self.withLocation(.{   .@"+ub" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "-ub")) self.withLocation(.{   .@"-ub" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "*ub")) self.withLocation(.{   .@"*ub" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],     "u>")) self.withLocation(.{    .@"u>" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],      "=")) self.withLocation(.{     .@"=" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "...")) self.withLocation(.{   .@"..." = void{}}, word_end)
        else null;
    }
};

