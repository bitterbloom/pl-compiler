const std = @import("std");

// TODO: Maybe make all the array lists unmanaged and store the allocator in the program

pub const Program = struct {
    tys: TyList,
    mods: ModList,
    insts: InstList,

    pub fn deinit(self: *Program, alloc: std.mem.Allocator) void {
        self.tys.deinit(alloc);
        self.mods.deinit(alloc);
        self.insts.deinit(alloc);
    }
};

pub const Ty = union(enum(u8)) {
    ptr_ty: TyList.Index,
    arr_ty: TyList.Index,
};

pub const TyList = struct {
    list: std.ArrayList(Ty),

    pub const Index = enum(u32) {
        void_ty,
        uint8_ty,
        uint32_ty,
        vararg_ty,
        _,

        const builtin_count: u32 = @typeInfo(Index).Enum.fields.len;
    };

    pub fn deinit(self: *TyList, alloc: std.mem.Allocator) void {
        _ = alloc;
        for (self.list.items) |_| {
            // switch (ty) {
            // }
        }
        self.list.deinit();
    }

    pub fn get(self: *const TyList, index: Index) *Ty {
        return &self.list.items[@intFromEnum(index) - Index.builtin_count];
    }

    pub fn indexOf(self: *const TyList, ptr: *const Ty) Index {
        var index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        index /= @sizeOf(Ty);
        std.debug.assert(index < self.list.items.len);
        return @enumFromInt(index + Index.builtin_count);
    }
};

pub const Mod = struct {
    ident: []const u8,
    globals: GlobalList,
    funcs: FuncList,
};

pub const ModList = struct {
    list: std.ArrayList(Mod),

    pub fn deinit(self: *ModList, alloc: std.mem.Allocator) void {
        for (self.list.items) |*mod| {
            alloc.free(mod.ident);
            mod.globals.deinit(alloc);
            mod.funcs.deinit(alloc);
        }
        self.list.deinit();
    }
};

pub const Global = struct {
    ident: []const u8,
    ty: TyList.Index,
    expr: Constant,

    pub fn deinit(self: *Global, alloc: std.mem.Allocator) void {
        alloc.free(self.ident);
        self.expr.deinit(alloc);
    }
};

pub const GlobalList = struct {
    list: std.ArrayList(Global),

    pub const Index = u32;

    pub fn deinit(self: *GlobalList, alloc: std.mem.Allocator) void {
        for (self.list.items) |*global| {
            global.deinit(alloc);
        }
        self.list.deinit();
    }

    pub fn get(self: *const GlobalList, index: Index) *Global {
        return &self.list.items[index];
    }

    pub fn indexOf(self: *const GlobalList, ptr: *const Global) Index {
        var index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        index /= @sizeOf(Global);
        std.debug.assert(index < self.list.items.len);
        return @intCast(index);
    }
};

pub const Func = union(enum(u1)) {
    internal: Internal = 0,
    external: External = 1,

    pub const Internal = struct {
        ident: [:0]const u8,
        ret_ty: TyList.Index,
        exported: bool,
        indices: std.ArrayList(InstList.Index),

        pub fn deinit(self: *Internal, alloc: std.mem.Allocator) void {
            alloc.free(self.ident);
            self.indices.deinit();
        }
    };

    pub const External = struct {
        ident: []const u8,
        ret_ty: TyList.Index,
        params: []const TyList.Index,

        pub fn deinit(self: *External, alloc: std.mem.Allocator) void {
            alloc.free(self.ident);
            alloc.free(self.params);
        }
    };

    pub fn deinit(self: *Func, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .internal => |internal| internal.deinit(alloc),
            .external => |external| external.deinit(alloc),
        }
    }
};

pub const FuncList = struct {
    list: std.ArrayList(Func),

    pub const Index = u32;

    pub fn deinit(self: *FuncList, alloc: std.mem.Allocator) void {
        for (self.list.items) |*func| {
            switch (func.*) {
                .internal => |*internal| internal.deinit(alloc),
                .external => |*external| external.deinit(alloc),
            }
        }
        self.list.deinit();
    }

    pub fn get(self: *const FuncList, index: Index) *Func {
        return &self.list.items[index];
    }

    pub fn indexOf(self: *const FuncList, ptr: *const Func) Index {
        var index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        index /= @sizeOf(Func);
        std.debug.assert(index < self.list.items.len);
        return @intCast(index);
    }
};

pub const InstList = struct {
    list: std.ArrayList(Inst),

    pub const Index = u32;

    pub fn add(self: *InstList, inst: Inst) !Index {
        ((try self.list.addOne()).*) = inst;
        return @intCast(self.list.items.len - 1);
    }

    pub fn addNoAlloc(self: *InstList, inst: Inst) Index {
        self.list.addOneAssumeCapacity().* = inst;
        return @intCast(self.list.items.len - 1);
    }

    pub fn addUninit(self: *InstList) !Index {
        try self.list.addOne();
        return @intCast(self.list.items.len - 1);
    }

    pub fn addUninitNoAlloc(self: *InstList) Index {
        _ = self.list.addOneAssumeCapacity();
        return @intCast(self.list.items.len - 1);
    }

    pub fn init(self: *InstList, index: Index, inst: Inst) void {
        self.get(index).* = inst;
    }

    pub fn deinit(self: *InstList, alloc: std.mem.Allocator) void {
        for (self.list.items) |*inst| {
            inst.deinit(alloc);
        }
        self.list.deinit();
    }

    pub fn get(self: *const InstList, index: Index) *Inst {
        return &self.list.items[index];
    }

    pub fn indexOf(self: *const InstList, ptr: *const Inst) Index {
        var index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        index /= @sizeOf(Inst);
        std.debug.assert(index < self.list.items.len);
        return @intCast(index);
    }
};

pub const Inst = union(enum(u8)) {
    param: Local,
    local: Local,
    set: Set,
    jump: Jump,
    branch: Branch,
    ret: Ret,
    block: Block,

    pub const Local = struct {
        ident: []const u8,
        ty: TyList.Index,
        data: Data,

        pub const Data = packed struct {
            on_stack: bool,
            is_const: bool,

            pub const d_tmp: Data = .{.on_stack = false, .is_const = true};
            // I don't know what to call a non-stack variable that is not const, if it should exist
            pub const d_let: Data = .{.on_stack = true,  .is_const = true};
            pub const d_var: Data = .{.on_stack = true,  .is_const = false};
        };

        pub fn deinit(self: *Local, alloc: std.mem.Allocator) void {
            alloc.free(self.ident);
        }
    };

    pub const Set = struct {
        target: InstList.Index,
        expr: Expr,

        pub fn deinit(self: *Set, alloc: std.mem.Allocator) void {
            self.expr.deinit(alloc);
        }
    };

    pub const Jump = struct {
        block: InstList.Index,
    };

    pub const Branch = struct {
        cond: Expr.Arg,
        block_t: InstList.Index,
        block_f: InstList.Index,
    };

    pub const Ret = struct {
        expr: Expr.Arg,
    };

    pub const Expr = union(enum(u8)) {
        local: InstList.Index,
        uint32: u32,
        bi_op: BiOp,
        call0: Call0,
        call1: Call1,
        call2: Call2,
        calln: CallN,

        pub const BiOp = struct {
            op: Op,
            lhs: Arg,
            rhs: Arg,

            pub const Op = enum(u4) {
                add = 0,
                sub = 1,
                mul = 2,
                ugt = 3,
            };
        };

        pub const Call0 = struct {
            index: FuncList.Index,
        };

        pub const Call1 = struct {
            index: FuncList.Index,
            arg: Arg,
        };

        pub const Call2 = struct {
            index: FuncList.Index,
            arg0: Arg,
            arg1: Arg,
        };

        pub const CallN = struct {
            index: FuncList.Index,
            arg_len: u32,
            args: [*]const Arg,

            pub fn deinit(self: *CallN, alloc: std.mem.Allocator) void {
                alloc.free(self.args[0..self.arg_len]);
            }
        };

        pub const Arg = union(enum) {
            local: InstList.Index,
            global: GlobalList.Index,
            uint32: u32,
            vararg: void,
        };

        pub fn deinit(self: *Expr, alloc: std.mem.Allocator) void {
            switch (self.*) {
                .calln => |*calln| calln.deinit(alloc),
                else => {},
            }
        }
    };

    pub const Block = struct {
        ident: []const u8,
        indices: std.ArrayList(InstList.Index),

        pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
            alloc.free(self.ident);
            self.indices.deinit();
        }
    };

    pub fn deinit(self: *Inst, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .local => |*local| local.deinit(alloc),
            .set   => |*set| set.deinit(alloc),
            .block => |*block| block.deinit(alloc),
            else => {},
        }
    }
};

pub const Constant = union(enum) {
    uint32: u32,
    str: []const u8,

    pub fn deinit(self: *Constant, alloc: std.mem.Allocator) void {
        switch (self.*) {
            .str => |str| alloc.free(str),
            else => {},
        }
    }
};

