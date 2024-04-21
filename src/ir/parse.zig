const std = @import("std");
const ir = @import("./types.zig");

const TokenKind = union(enum) {
    // Keywords
    tmp: void,
    let: void,
    @"var": void,
    def: void,
    @"extern": void,
    @"export": void,
    module: void,
    // Instructions
    @"return": void,
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

const Location = struct {
    line: u32,
    char: u32,

    const first_line = 1;
    const first_char = 1;
};

const Token = struct {
    kind: TokenKind,
    location: Location,
};

const Tokenizer = struct {
    source: []const u8,
    word_start: u32 = 0,
    location: Location = .{
        .line = Location.first_line,
        .char = Location.first_char
    },

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

    fn tokenize(self: *Tokenizer, alloc: std.mem.Allocator) !Token {
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
                                self.location.char = Location.first_char;
                                self.word_start = index + 1;
                                break :next_char;
                            },
                            // TODO: Should the word_start also include the sigil?
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
                                    // We already know the token won't be longer than this
                                    // character so we can just end it before looking at
                                    // the next comment.
                                    token = Token{
                                        .kind = fromDelimiter(char),
                                        .location = Location{
                                            .line = self.location.line,
                                            .char = self.location.char,
                                        },
                                    };
                                    self.location.char += 1;
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
                                const word = try self.allocWord(index, alloc);
                                token = self.withLocation(switch (state) {
                                    .is_global => .{.gbl_id = word},
                                    .is_local =>  .{.lcl_id = word},
                                    .is_label =>  .{.lbl_id = word},
                                    .is_module => .{.mod_id = word},
                                    else => unreachable,
                                }, index);
                                self.location.line += 1;
                                self.location.char = Location.first_char;
                                self.word_start = index + 1;
                                break :make_token;
                            },
                            else => {
                                if (std.ascii.isWhitespace(char)) {
                                    const word = try self.allocWord(index, alloc);
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
                                    const word = try self.allocWord(index, alloc);
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
                                const word = try self.allocWord(index, alloc);
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
                                self.location.char = Location.first_char;
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
                                    const word = try self.allocWord(index, alloc);
                                    token = self.withLocation(.{.word = word}, index);
                                }
                                self.location.line += 1;
                                self.location.char = Location.first_char;
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
                                        const word = try self.allocWord(index, alloc);
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
                                        const word = try self.allocWord(index, alloc);
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
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.gbl_id = word}, index);
                    break :make_token;
                },
                .is_local => {
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.lcl_id = word}, index);
                    break :make_token;
                },
                .is_label => {
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.lbl_id = word}, index);
                    break :make_token;
                },
                .is_module => {
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.mod_id = word}, index);
                    break :make_token;
                },
                .is_word => {
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.word = word}, index);
                    break :make_token;
                },
                .is_string => {
                    const word = try self.allocWord(index, alloc);
                    token = self.withLocation(.{.invalid = word}, index);
                    break :make_token;
                },
                .is_number => {
                    const number = try std.fmt.parseUnsigned(u64, self.source[self.word_start..index], 10);
                    token = self.withLocation(.{.int = number}, index);
                    break :make_token;
                },
                .is_decimal => {
                    const number = try std.fmt.parseFloat(f64, self.source[self.word_start..index]);
                    token = self.withLocation(.{.float = number}, index);
                    break :make_token;
                },
                .is_comment => {
                    token = self.withLocation(.{.eof = void{}}, index);
                    break :make_token;
                },
            }

            @compileError("Unreachable");
        }
        
        return token;
    }

    fn withLocation(self: *const Tokenizer, token_kind: TokenKind, word_end: u32) Token {
        // std.debug.print("line: {d},\t char: {d},\tstart: {d},\tend: {d},\tkind: {}\n", .{self.location.line, self.location.char, self.word_start, word_end, token_kind});
        return Token{
            .kind = token_kind,
            .location = .{
                .line = self.location.line,
                .char = self.location.char - (word_end - self.word_start),
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

    fn allocWord(self: *const Tokenizer, word_end: u32, alloc: std.mem.Allocator) ![*:0]u8 {
        return try alloc.dupeZ(u8, self.source[self.word_start..word_end]);
    }

    fn fromKeyword(self: *const Tokenizer, word_end: u32) ?Token {
        return if (std.mem.eql(u8, self.source[self.word_start..word_end],    "tmp")) self.withLocation(.{     .tmp  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "let")) self.withLocation(.{     .let  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "var")) self.withLocation(.{   .@"var" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end],    "def")) self.withLocation(.{     .def  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "extern")) self.withLocation(.{.@"extern" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "export")) self.withLocation(.{.@"export" = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "module")) self.withLocation(.{  .module  = void{}}, word_end)
        else   if (std.mem.eql(u8, self.source[self.word_start..word_end], "return")) self.withLocation(.{.@"return" = void{}}, word_end)
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

pub const ParseError = struct {
    ///This string is not owned, and most likely a compile time string.
    message: []const u8,
    actual: Token,

    pub fn fmt(self: ParseError, alloc: std.mem.Allocator) ![]const u8 {
        return std.fmt.allocPrint(
            alloc,
            "{s}, but instead found: '{s}' at line {d}, char {d}",
            .{
                self.message,
                @tagName(self.actual.kind),
                self.actual.location.line,
                self.actual.location.char
            },
        );
    }
};

const Parser = struct {
    tokenizer: Tokenizer,
    program: ir.Program,
    errors: std.ArrayListUnmanaged(ParseError) = .{},
    alloc: std.mem.Allocator,

    inline fn tokenize(self: *Parser) !Token {
        return self.tokenizer.tokenize(self.alloc);
    }

    inline fn addError(self: *Parser, comptime message: []const u8, actual: Token) !void {
        //std.debug.print("Added error: {s}\n", .{message});
        try self.errors.append(self.alloc, .{.message = message, .actual = actual});
    }
};

pub fn parseProgram(source: []const u8, alloc: std.mem.Allocator) !struct {[]const ParseError, ir.Program} {
    var parser = Parser{
        .tokenizer = Tokenizer{
            .source = source,
        },
        .program = ir.Program{
            .tys = .{.list = std.ArrayList(ir.Ty).init(alloc) },
            .mods = .{.list = std.ArrayList(ir.Mod).init(alloc) },
            .insts = .{.list = std.ArrayList(ir.Inst).init(alloc) },
        },
        .errors = std.ArrayListUnmanaged(ParseError){},
        .alloc = alloc,
    };

    while (true) {
        const token = try parser.tokenize();
        switch (token.kind) {
            .module => {
                try parseModuleDecl(&parser);
                continue;
            },
            .eof => break,
            else => {
                try parser.addError("Expected 'module'", token);
                break;
            },
        }
    }

    return .{
        try parser.errors.toOwnedSlice(alloc),
        parser.program
    };
}

fn parseModuleDecl(parser: *Parser) !void {
    const token = try parser.tokenize();
    const mod = switch (token.kind) {
        .@"{" => blck: {
            const ptr = try parser.program.mods.list.addOne();
            ptr.* = ir.Mod{
                .ident = "",
                .funcs = .{.list = std.ArrayList(ir.Func).init(parser.alloc) },
                .globals = .{.list = std.ArrayList(ir.Global).init(parser.alloc) },
            };
            break :blck ptr;
        },
        .mod_id => |ident| blck: {
            const ptr = try parser.program.mods.list.addOne();
            ptr.* = ir.Mod{
                .ident = std.mem.sliceTo(ident, 0),
                .funcs = .{.list = std.ArrayList(ir.Func).init(parser.alloc) },
                .globals = .{.list = std.ArrayList(ir.Global).init(parser.alloc) },
            };

            const token_ = try parser.tokenize();
            break :blck switch (token_.kind) {
                .@"{" => ptr,
                else => return parser.addError("Expected '{'", token_),
            };
        },
        else => return parser.addError("Expected '{' or module identifier", token),
    };

    return parseModule(parser, mod);
}

fn parseModule(parser: *Parser, mod: *ir.Mod) !void {
    while (true) {
        const token = try parser.tokenize();
        switch (token.kind) {
            .@"}" => return,
            .def => try parseDefDecl(parser, mod, false, false),
            .@"extern" => try parseDefDecl(parser, mod, false, true),
            .@"export" => {
                const token_ = try parser.tokenize();
                switch (token_.kind) {
                    .def => try parseDefDecl(parser, mod, true, false),
                    else => return parser.addError("Expected 'def'", token_),
                }
            },
            else => return parser.addError("Expected 'def', 'extern', or 'export'", token),
        }
    }
}

fn parseDefDecl(parser: *Parser, mod: *ir.Mod, comptime exported: bool, comptime external: bool) !void {
    comptime if (exported and external) @compileError("Cannot be both 'export' and 'extern'");

    const ident_token = try parser.tokenize();
    const ident = switch (ident_token.kind) {
        .gbl_id => |ident| ident,
        else => return parser.addError("Expected global identifier", ident_token),
    };

    if (external) {
        @panic("todo");
    }

    const paren_token = try parser.tokenize();
    if (paren_token.kind != TokenKind.@"(") {
        return parser.addError("Expected '('", paren_token);
    }

    var indices = std.ArrayList(ir.InstList.Index).init(parser.alloc);

    while (true) {
        const token = try parser.tokenize();
        switch (token.kind) {
            .@")" => break,
            else => return parser.addError("Expected ')'", token),
        }
    }

    const ret_ty = try parseType(parser) orelse return;

    const brace_token = try parser.tokenize();
    if (brace_token.kind != TokenKind.@"{") {
        return parser.addError("Expected '{'", brace_token);
    }

    while (true) {
        const token = try parser.tokenize();
        switch (token.kind) {
            .@"}" => break,
            .@"return" => {
                const inst = try parser.program.insts.add(ir.Inst{
                    .ret = .{
                        .expr = try parseArg(parser, mod, &indices) orelse return,
                    },
                });
                try indices.append(inst);
            },
            else => return parser.addError("Expected 'return'", token),
        }
    }

    try mod.funcs.list.append(ir.Func{
        .internal = .{
            .ident = std.mem.sliceTo(ident, 0),
            .ret_ty = ret_ty,
            .exported = exported,
            .indices = indices,
        },
    });
}

fn parseType(parser: *Parser) !?ir.TyList.Index {
    const token = try parser.tokenize();
    switch (token.kind) {
        .word => |ident| {
            const slice = std.mem.sliceTo(ident, 0);
            const ty = if (std.mem.eql(u8, slice, "UInt32")) ir.TyList.Index.uint32_ty
            else @panic("Unknown type identifier");

            parser.alloc.free(slice);
            return ty;
        },
        else => {
            try parser.addError("Expected type identifier", token);
            return null;
        },
    }
}

fn parseArg(parser: *Parser, mod: *ir.Mod, indices: *std.ArrayList(ir.InstList.Index)) !?ir.Inst.Expr.Arg {
    _ = mod;
    _ = indices;

    const token = try parser.tokenize();
    switch (token.kind) {
        .int => |int| {
            return ir.Inst.Expr.Arg{
                .uint32 = @truncate(int),
            };
        },
        else => {
            try parser.addError("Expected integer literal", token);
            return null;
        }
    }
}

test "hello world" {
    const testing = std.testing;

    var buffer: [100_000]u8 = undefined;
    var alloc = std.heap.FixedBufferAllocator.init(&buffer);
    //const alloc = testing.allocator;

    const string =
      \\module {
      \\    def $fmt []UInt8 = "Hello, World!\0"
      \\    extern $puts(^[]UInt8, ) UInt32
      \\    export def $main() UInt32 {
      \\        tmp %r UInt32
      \\        %r = $puts($fmt, )
      \\        return %r
      \\    }
      \\}
      \\
    ;

    const expected = [_]Token{
        .{.kind = .{.module    = void{}},        .location = .{.line =  1, .char =  1}},
        .{.kind = .{.@"{"      = void{}},        .location = .{.line =  1, .char =  8}},
        .{.kind = .{.def       = void{}},        .location = .{.line =  2, .char =  5}},
        .{.kind = .{.gbl_id    = "fmt" },        .location = .{.line =  2, .char = 10}},
        .{.kind = .{.@"["      = void{}},        .location = .{.line =  2, .char = 14}},
        .{.kind = .{.@"]"      = void{}},        .location = .{.line =  2, .char = 15}},
        .{.kind = .{.word      = "UInt8" },      .location = .{.line =  2, .char = 16}},
        .{.kind = .{.@"="      = void{}},        .location = .{.line =  2, .char = 22}},
        .{.kind = .{.str = "Hello, World!\\0" }, .location = .{.line =  2, .char = 25}},
        .{.kind = .{.@"extern" = void{}},        .location = .{.line =  3, .char =  5}},
        .{.kind = .{.gbl_id    = "puts" },       .location = .{.line =  3, .char = 13}},
        .{.kind = .{.@"("      = void{}},        .location = .{.line =  3, .char = 17}},
        .{.kind = .{.word      = "^" },          .location = .{.line =  3, .char = 18}},
        .{.kind = .{.@"["      = void{}},        .location = .{.line =  3, .char = 19}},
        .{.kind = .{.@"]"      = void{}},        .location = .{.line =  3, .char = 20}},
        .{.kind = .{.word      = "UInt8" },      .location = .{.line =  3, .char = 21}},
        .{.kind = .{.@","      = void{}},        .location = .{.line =  3, .char = 26}},
        .{.kind = .{.@")"      = void{}},        .location = .{.line =  3, .char = 28}},
        .{.kind = .{.word      = "UInt32" },     .location = .{.line =  3, .char = 30}},
        .{.kind = .{.@"export" = void{}},        .location = .{.line =  4, .char =  5}},
        .{.kind = .{.def       = void{}},        .location = .{.line =  4, .char = 12}},
        .{.kind = .{.gbl_id    = "main" },       .location = .{.line =  4, .char = 17}},
        .{.kind = .{.@"("      = void{}},        .location = .{.line =  4, .char = 21}},
        .{.kind = .{.@")"      = void{}},        .location = .{.line =  4, .char = 22}},
        .{.kind = .{.word      = "UInt32" },     .location = .{.line =  4, .char = 24}},
        .{.kind = .{.@"{"      = void{}},        .location = .{.line =  4, .char = 31}},
        .{.kind = .{.tmp       = void{}},        .location = .{.line =  5, .char =  9}},
        .{.kind = .{.lcl_id    = "r" },          .location = .{.line =  5, .char = 14}},
        .{.kind = .{.word      = "UInt32" },     .location = .{.line =  5, .char = 16}},
        .{.kind = .{.lcl_id    = "r" },          .location = .{.line =  6, .char = 10}},
        .{.kind = .{.@"="      = void{}},        .location = .{.line =  6, .char = 12}},
        .{.kind = .{.gbl_id    = "puts" },       .location = .{.line =  6, .char = 15}},
        .{.kind = .{.@"("      = void{}},        .location = .{.line =  6, .char = 19}},
        .{.kind = .{.gbl_id    = "fmt" },        .location = .{.line =  6, .char = 21}},
        .{.kind = .{.@","      = void{}},        .location = .{.line =  6, .char = 24}},
        .{.kind = .{.@")"      = void{}},        .location = .{.line =  6, .char = 26}},
        .{.kind = .{.@"return" = void{}},        .location = .{.line =  7, .char =  9}},
        .{.kind = .{.lcl_id    = "r" },          .location = .{.line =  7, .char = 17}},
        .{.kind = .{.@"}"      = void{}},        .location = .{.line =  8, .char =  5}},
        .{.kind = .{.@"}"      = void{}},        .location = .{.line =  9, .char =  1}},
        .{.kind = .{.eof       = void{}},        .location = .{.line = 10, .char =  1}},
    };

    var tokens = try std.ArrayList(Token).initCapacity(alloc.allocator(), 100);

    var tokenizer = Tokenizer{.source = string};
    try tokens.append(try tokenizer.tokenize(alloc.allocator()));

    while (tokens.getLast().kind != TokenKind.eof) {
        try tokens.append(try tokenizer.tokenize(alloc.allocator()));
    }

    const actual: []Token = try tokens.toOwnedSlice();
    
    // Partly from std.testing.expectEqualDeep:
    for (expected, actual[0..expected.len], 0..) |e, a, i| {
        const Tag = std.meta.Tag(@TypeOf(e.kind));
        std.testing.expectEqual(@as(Tag, e.kind), @as(Tag, a.kind)) catch {
            std.debug.panic("Incorrect tag at i: {d}, expected: {s}, actual: {s}\n", .{i, @tagName(e.kind), @tagName(a.kind)});
        };
    }
    for (expected, actual[0..expected.len], 0..) |e, a, i| {
        switch (e.kind) {
            inline else => |e_val, e_tag| {
                if (@TypeOf(e_val) == [*:0]const u8) {
                    const e_str = @as([*:0]const u8, e_val);
                    const a_str = @as([*:0]const u8, @field(a.kind, @tagName(e_tag)));
                    if (std.mem.orderZ(u8, e_str, a_str) != std.math.Order.eq) {
                        std.debug.panic("Incorrect string at i: {d}, expected: {s}, actual: {s} kind: {s}\n", .{i, e_str, a_str, @tagName(a.kind)});
                    }
                }
            }
        }
    }
    for (expected, actual[0..expected.len], 0..) |e, a, i| {
        std.testing.expectEqual(e.location, a.location) catch {
            std.debug.panic("Incorrect location at i: {d}, expected: {}, actual: {} kind: {s}\n", .{i, e.location, a.location, @tagName(a.kind)});
        };
    }

    if (expected.len != actual.len) {
        std.debug.print("Expected length: {d}, found: {d}\n", .{expected.len, actual.len});
        try testing.expect(false);
    }

    alloc.reset();
}

