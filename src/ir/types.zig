const std = @import("std");

pub const Program = struct {
    tys: TyList,
    mods: ModList,
};

pub const TyList = struct {
    list: std.ArrayList(Ty),

    pub const Index = enum(u32) {
        void_ty,
        uint8_ty,
        uint32_ty,
        _,

        const builtin_count: u32 = @typeInfo(Index).Enum.fields.len;
    };

    pub fn get(self: *const TyList, index: Index) *Ty {
        return &self.list.items[@intFromEnum(index) - Index.builtin_count];
    }

    pub fn indexOf(self: *const TyList, ptr: *const Ty) Index {
        var index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        index /= @sizeOf(*void);
        std.debug.assert(index < self.list.items.len);
        return @enumFromInt(index + Index.builtin_count);
    }
};

pub const Ty = union(enum(u8)) {
    ptr_ty: TyList.Index,
    arr_ty: TyList.Index,
};

pub const ModList = struct {
    list: std.ArrayList(Mod),
};

pub const Mod = struct {
    ident: []const u8,
    globals: GlobalList,
    funcs: FuncList,
};

pub const Global = struct {
    ident: []const u8,
    ty: TyList.Index,
    expr: Constant,
};

pub const GlobalList = struct {
    list: std.ArrayList(Global),

    pub const Index = u32;

    pub fn get(self: *const GlobalList, index: Index) *Global {
        return &self.list.items[index];
    }

    pub fn indexOf(self: *const GlobalList, ptr: *const Global) Index {
        const index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        std.debug.assert(index < self.list.items.len);
        return @intCast(index);
    }
};

pub const Func = union(enum(u1)) {
    internal: Internal = 0,
    external: External = 1,

    pub const Internal = struct {
        ident: []const u8,
        ret_ty: TyList.Index,
        params: []const TypedIdent,
        exported: bool,
        insts: InstList,
    };

    pub const External = struct {
        ident: []const u8,
        ret_ty: TyList.Index,
        params: []const TyList.Index,
    };
};

pub const FuncList = struct {
    list: std.ArrayList(Func),

    pub const Index = u32;

    pub fn get(self: *const FuncList, index: Index) *Func {
        return &self.list.items[index];
    }

    pub fn indexOf(self: *const FuncList, ptr: *const Func) Index {
        const index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        std.debug.assert(index < self.list.items.len);
        return @intCast(index);
    }
};

pub const InstList = struct {
    list: std.ArrayList(Inst),

    pub const Index = enum(u32) {
        _,
    };

    pub fn get(self: *const InstList, index: Index) *Inst {
        return &self.list.items[@intFromEnum(index)];
    }

    pub fn indexOf(self: *const InstList, ptr: *const Inst) Index {
        const index = @intFromPtr(ptr) - @intFromPtr(self.list.items.ptr);
        std.debug.assert(index < self.list.items.len);
        return @enumFromInt(index);
    }
};

pub const Inst = union(enum(u8)) {
    local: Local,
    set: Set,
    ret: Ret,

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
    };

    pub const Set = struct {
        target: Target,
        expr: Expr,

        pub const Target = union(enum(u8)) {
            local: InstList.Index,
        };
    };

    pub const Ret = struct {
        expr: Expr,
    };

    pub const Expr = union(enum(u8)) {
        local: InstList.Index,
        constant: Constant,
        call0: Call0,
        call1: Call1,

        pub const Call0 = struct {
            index: FuncList.Index,
        };

        pub const Call1 = struct {
            index: FuncList.Index,
            arg: Arg,
        };

        pub const Arg = union(enum) {
            local: InstList.Index,
            global: GlobalList.Index,
        };
    };
};

pub const Constant = union(enum) {
    uint32: u32,
    str: []const u8,
};

pub const TypedIdent = struct {
    ident: []const u8,
    ty: TyList.Index,
};

