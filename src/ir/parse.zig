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
    word_start: u32,
    location: Location,

    pub fn init(source: []const u8) Tokenizer {
        return .{
            .source = source,
            .word_start = 0,
            .location = .{.line = 1, .char = 0},
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

    pub fn tokenize(self: *Tokenizer, alloc: std.mem.Allocator) !Token {

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
                                self.location.char = 0;
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
                                    const word = try self.allocWord(index, alloc);
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
        return .{
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

pub fn parseProgram(source: []const u8, alloc: std.mem.Allocator) !ir.Program {
    var tokenizer = Tokenizer.init(source);

    var program = ir.Program{
        .tys = .{.list = std.ArrayList(ir.Ty).init(alloc) },
        .mods = .{.list = std.ArrayList(ir.Mod).init(alloc) },
        .insts = .{.list = std.ArrayList(ir.Inst).init(alloc) },
    };

    const token = try tokenizer.tokenize(alloc);
    switch (token.kind) {
        .module => {
            try parseModuleDecl(&tokenizer, &program, alloc);
        },
        .eof => {},
        // TODO: Add compiler errors
        else => {
            panic("Expected 'module'", token);
        },
    }

    return program;
}

fn parseModuleDecl(tokenizer: *Tokenizer, program: *ir.Program, alloc: std.mem.Allocator) !void {
    const token = try tokenizer.tokenize(alloc);
    const mod = switch (token.kind) {
        .@"{" => blck: {
            const ptr = try program.mods.list.addOne();
            ptr.* = ir.Mod{
                .ident = "",
                .funcs = .{.list = std.ArrayList(ir.Func).init(alloc) },
                .globals = .{.list = std.ArrayList(ir.Global).init(alloc) },
            };
            break :blck ptr;
        },
        .mod_id => |ident| blck: {
            const ptr = try program.mods.list.addOne();
            ptr.* = ir.Mod{
                .ident = std.mem.sliceTo(ident, 0),
                .funcs = .{.list = std.ArrayList(ir.Func).init(alloc) },
                .globals = .{.list = std.ArrayList(ir.Global).init(alloc) },
            };

            const token_ = try tokenizer.tokenize(alloc);
            break :blck switch (token_.kind) {
                .@"{" => ptr,
                else => {
                    panic("Expected '{'", token_);
                },
            };
        },
        else => {
            panic("Expected '{' or module identifier", token);
        },
    };

    try parseModule(tokenizer, &program.tys, mod, &program.insts, alloc);
}

fn parseModule(tokenizer: *Tokenizer, tys: *ir.TyList, mod: *ir.Mod, insts: *ir.InstList, alloc: std.mem.Allocator) !void {
    while (true) {
        const token = try tokenizer.tokenize(alloc);
        switch (token.kind) {
            .@"}" => return,
            .def => try parseDefDecl(tokenizer, tys, mod, insts, false, false, alloc),
            .@"extern" => try parseDefDecl(tokenizer, tys, mod, insts, false, true, alloc),
            .@"export" => {
                const token_ = try tokenizer.tokenize(alloc);
                switch (token_.kind) {
                    .def => try parseDefDecl(tokenizer, tys, mod, insts, true, false, alloc),
                    else => {
                        panic("Expected 'def'", token_);
                    },
                }
            },
            else => {
                panic("Expected 'def', 'extern', or 'export'", token);
            },
        }
    }
}

fn parseDefDecl(tokenizer: *Tokenizer, tys: *ir.TyList, mod: *ir.Mod, insts: *ir.InstList, comptime exported: bool, comptime external: bool, alloc: std.mem.Allocator) !void {
    comptime if (exported and external) @compileError("Cannot be both 'export' and 'extern'");

    const ident_token = try tokenizer.tokenize(alloc);
    const ident = switch (ident_token.kind) {
        .gbl_id => |ident| ident,
        else => {
            panic("Expected global identifier", ident_token);
        },
    };

    if (external) {
        @panic("todo");
    }

    if ((try tokenizer.tokenize(alloc)).kind != TokenKind.@"(") {
        @panic("Expected '('");
    }

    var indices = std.ArrayList(ir.InstList.Index).init(alloc);

    while (true) {
        const token = try tokenizer.tokenize(alloc);
        switch (token.kind) {
            .@")" => break,
            else => {
                panic("Expected ')'", token);
            },
        }
    }

    // TODO: Parse return type
    const ret_ty = try parseType(tokenizer, tys, alloc);

    if ((try tokenizer.tokenize(alloc)).kind != TokenKind.@"{") {
        @panic("Expected '{'");
    }

    while (true) {
        const token = try tokenizer.tokenize(alloc);
        switch (token.kind) {
            .@"}" => break,
            .@"return" => {
                const inst = try insts.add(ir.Inst{
                    .ret = .{
                        .expr = try parseArg(tokenizer, mod, insts, &indices, alloc),
                    },
                });
                try indices.append(inst);
            },
            else => {
                panic("Expected 'return' or '}'", token);
            },
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

fn parseType(tokenizer: *Tokenizer, tys: *ir.TyList, alloc: std.mem.Allocator) !ir.TyList.Index {
    _ = tys;

    const token = try tokenizer.tokenize(alloc);
    switch (token.kind) {
        .word => |ident| {
            const slice = std.mem.sliceTo(ident, 0);
            const ty = if (std.mem.eql(u8, slice, "UInt32")) ir.TyList.Index.uint32_ty
            else @panic("Unknown type identifier");

            alloc.free(slice);
            return ty;
        },
        else => {
            @panic("Expected type identifier");
        },
    }
}

fn parseArg(tokenizer: *Tokenizer, mod: *ir.Mod, insts: *ir.InstList, indices: *std.ArrayList(ir.InstList.Index), alloc: std.mem.Allocator) !ir.Inst.Expr.Arg {
    _ = mod;
    _ = insts;
    _ = indices;

    const token = try tokenizer.tokenize(alloc);
    switch (token.kind) {
        .int => |int| {
            return ir.Inst.Expr.Arg{
                .uint32 = @truncate(int),
            };
        },
        else => {
            @panic("Expected integer literal");
        }
    }
}

fn panic(comptime message: []const u8, actual: Token) noreturn {
    std.debug.panic("{s}, but instead found: '{s}' at line {d}, char {d}", .{message, @tagName(actual.kind), actual.location.line, actual.location.char});
}

