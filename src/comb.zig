const std = @import("std");
const wabi = @import("wabi.zig");
const Word = wabi.types.Word;
const Tag = wabi.types.Tag;
const UInt = wabi.types.UInt;
const Mem = wabi.mem.Mem;
const RefTo = wabi.ref.RefTo;
const Ref = wabi.ref.Ref;
const Sym = wabi.sym.Sym;
const VectorList = wabi.list.VectorList;
const Offset = Mem.Offset;
const Val = wabi.val.Val;
const Cont = wabi.cont.Cont;
const Meta = wabi.meta.Meta;
const word_align = @alignOf(Word);
const testing = std.testing;

pub const Builtin = packed struct(Word) {
    tag: Tag,
    uid: Uid,

    pub const Uid = UInt;

    const Self = @This();

    pub const Ptr = *align(@alignOf(Word)) Self;
    pub const smallness = .always;

    pub fn from(
        mem: *Mem,
        uid: Uid,
        comptime t: Tag,
    ) !*Self {
        const self = try mem.one(Self);
        self.tag = t;
        self.uid = uid;
        return self;
    }

    pub fn operative(
        mem: *Mem,
        uid: Uid,
    ) !*Self {
        return from(mem, uid, .BOpr);
    }

    pub fn applicative(
        mem: *Mem,
        uid: Uid,
    ) !*Self {
        return from(mem, uid, .BApp);
    }

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !*Self {
        return try ctx.memMove(self);
    }

    pub fn copy(
        _: Ptr,
        _: anytype,
    ) !Word {
        return @sizeOf(Self);
    }
};

test "create builtin operative" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const b = try Builtin.operative(&mem, 123);
    try testing.expectEqual(b.tag, .BOpr);
    try testing.expectEqual(b.uid, 123);
}

test "create builtin applicative" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const b = try Builtin.applicative(&mem, 123);
    try testing.expectEqual(b.tag, .BApp);
    try testing.expectEqual(b.uid, 123);
}

test "move builtin" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    var cur = try Builtin.operative(&orig, 131);
    const nxt = try cur.move(&ctx);
    try std.testing.expect(nxt != cur);
    try std.testing.expect(nxt.tag == cur.tag);
    try std.testing.expect(nxt.uid == cur.uid);
}

pub const Derived = packed struct {
    //todo: optimize pattern to be transformed in a function
    tag: Tag,
    env_offset: Offset,
    caller_env_name: Sym,
    pattern: Ref,
    body: RefTo(VectorList, .{ .nullable = false }),

    const Self = @This();
    pub const Ptr = *align(word_align) Self;
    pub const smallness = .never;

    pub fn create(
        mem: *Mem,
        args: anytype,
    ) !Ptr {
        const self = try mem.one(Self);
        self.tag = args.tag;
        self.env_offset = mem.offsetOf(args.env);
        self.caller_env_name = args.env_name.*;
        self.pattern.set(mem, args.pattern);
        self.body.set(mem, args.body);
        return self;
    }

    pub fn applicative(mem: *Mem, args: anytype) !Ptr {
        const res = create(mem, .{
            .tag = .DApp,
            .env = args.env,
            .env_name = args.env_name,
            .pattern = args.pattern,
            .body = args.body,
        });
        return res;
    }
    pub fn operative(mem: *Mem, args: anytype) !Ptr {
        const res = create(mem, .{
            .tag = .DOpr,
            .env = args.env,
            .env_name = args.env_name,
            .pattern = args.pattern,
            .body = args.body,
        });
        return res;
    }

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        return try ctx.memMove(self);
    }

    pub fn copy(
        self: Ptr,
        ctx: anytype,
    ) error{
        MemoryOverflow,
        OutOfMemory,
    }!Word {
        // copy original env
        const orig_env = ctx.orig.fromOffset(Val.Ptr, self.env_offset);
        const dest_env = try ctx.move(orig_env);
        self.env_offset = ctx.dest.offsetOf(dest_env);

        // copy things
        _ = try self.caller_env_name.copy(ctx);
        _ = try self.pattern.copy(ctx);
        _ = try self.body.copy(ctx);

        return @sizeOf(Self);
    }
};

test "create derived operative" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const env = try wabi.env.Env.root(&mem, 1);
    const name_bin = try wabi.bin.Block.from(&mem, "_");
    const name_sym = try wabi.sym.Sym.from(&mem, name_bin);
    const emp = try wabi.val.Val.from(&mem, wabi.list.VectorList, .{});
    const opr = try Derived.operative(&mem, .{
        .env = env,
        .env_name = name_sym,
        .pattern = emp,
        .body = &emp.list,
    });
    try testing.expectEqual(opr.tag, .DOpr);
}

test "create derived applicative" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const env = try wabi.env.Env.root(&mem, 1);
    const name_bin = try wabi.bin.Block.from(&mem, "_");
    const name_sym = try wabi.sym.Sym.from(&mem, name_bin);
    const emp = try wabi.val.Val.from(&mem, wabi.list.VectorList, .{});
    const opr = try Derived.applicative(&mem, .{
        .env = env,
        .env_name = name_sym,
        .pattern = emp,
        .body = &emp.list,
    });
    try testing.expectEqual(opr.tag, .DApp);
}

test "move derived" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const env = try wabi.env.Env.root(&orig, 1);
    const name_bin = try wabi.bin.Block.from(&orig, "_");
    const name_sym = try wabi.sym.Sym.from(&orig, name_bin);
    const emp = try wabi.val.Val.from(&orig, wabi.list.VectorList, .{});
    var cur = try Derived.applicative(&orig, .{
        .env = env,
        .env_name = name_sym,
        .pattern = emp,
        .body = &emp.list,
    });
    const nxt = try cur.move(&ctx);
    try testing.expect(cur != nxt);
    try testing.expect(cur.tag == nxt.tag);
    try testing.expect(cur.env_offset == nxt.env_offset);
    try testing.expect(cur.caller_env_name.bin_offset == nxt.caller_env_name.bin_offset);
    try testing.expect(cur.pattern.a_ref.tag == nxt.pattern.a_ref.tag);
    try testing.expect(cur.pattern.a_ref.offset == nxt.pattern.a_ref.offset);
    try testing.expect(cur.body.a_ref.tag == nxt.body.a_ref.tag);
    try testing.expect(cur.body.a_ref.offset == nxt.body.a_ref.offset);
}

pub const Control = packed struct {
    tag: Tag,
    _: Offset,
    cont: RefTo(Cont, .{ .nullable = true }),
    cont_mark: RefTo(Val, .{ .nullable = true }),
    rev_meta: RefTo(Meta, .{ .nullable = true }),

    const Self = @This();

    // todo: all these Ptr(s) must be a HOT like Ptr(Self)
    pub const Ptr = *align(word_align) Self;
    pub const smallness = .never;

    pub fn new(
        mem: *Mem,
        opts: struct {
            cont: ?Cont.Ptr,
            cont_mark: ?Val.Ptr,
            rev_meta: ?Meta.Ptr,
        },
    ) !Ptr {
        const self = try mem.one(Self);
        self.tag = .Kont;
        self.cont.set(mem, opts.cont);
        self.cont_mark.set(mem, opts.cont_mark);
        self.rev_meta.set(mem, opts.rev_meta);
        return self;
    }

    pub fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        return try ctx.memMove(self);
    }

    pub fn copy(
        self: Ptr,
        ctx: anytype,
    ) !Word {
        try self.cont.copy(ctx);
        try self.cont_mark.copy(ctx);
        try self.rev_meta.copy(ctx);
        return @sizeOf(Self);
    }
};

test "copy control" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur = try Control.new(&orig, .{
        .cont = null,
        .cont_mark = null,
        .rev_meta = null,
    });

    const nxt = try ctx.one(cur);
    try std.testing.expect(nxt.cont.get(&orig) == null);
    try std.testing.expect(nxt.cont_mark.get(&orig) == null);
    try std.testing.expect(nxt.rev_meta.get(&orig) == null);
}

test "copy complex control" {
    const Vm = wabi.vm.Vm;

    const vmConfig = .{
        .allocator = std.testing.allocator,
        .mem_size = 10 * 1_024,
    };

    var vm = try Vm.init(vmConfig);
    defer vm.deinit();

    var out_buf: [1024]u8 = undefined;
    _ = try vm.evalString("(do (def k (prompt :p (control :p ret ret) (+ 2 3))) k)", out_buf[0..1024]);
    const orig = &vm.mem;
    var dest = try Mem.init(std.testing.allocator, .{
        .mem_size = @intCast(orig.space.len),
    });
    defer dest.deinit();

    const cur = Val.cast(vm.ctrl);
    var ctx = try wabi.copy.Copy.init(orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const nxt = try ctx.one(cur);
    //todo: do real tests
    try std.testing.expect(nxt != cur);
    try std.testing.expect(nxt.tag() == .Kont);
}
