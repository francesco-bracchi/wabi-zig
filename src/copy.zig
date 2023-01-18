const std = @import("std");
const wabi = @import("wabi.zig");
const Val = wabi.val.Val;
const Mem = wabi.mem.Mem;
const Word = wabi.types.Word;
const Fix = wabi.num.Fix;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const Block = wabi.bin.Block;
const VectorList = wabi.list.VectorList;
const Size = Mem.Size;
const Offset = Mem.Offset;
const HashMap = std.AutoHashMap(Offset, Offset);
const Builtin = wabi.comb.Builtin;
const Env = wabi.env.Env;
const Derived = wabi.comb.Derived;
const Control = wabi.comb.Control;
const cont = wabi.cont;
const Cont = cont.Cont;

const Allocator = std.mem.Allocator;
// for the allocator use an arena allocator: all the memory can be freed once the
// copy is over

pub const Copy = struct {
    orig: *Mem,
    dest: *Mem,
    refs: HashMap,
    scan: Word,

    const Self = @This();

    pub fn init(
        orig: *Mem,
        dest: *Mem,
        allocator: Allocator,
    ) !Self {
        return .{
            .orig = orig,
            .dest = dest,
            .refs = HashMap.init(allocator),
            .scan = dest.heap,
        };
    }

    pub fn deinit(self: *Self) void {
        self.refs.deinit();
    }

    pub const Error = error{
        OutOfMemory,
        MemoryOverflow,
    };

    pub fn move(
        self: *Self,
        val: anytype,
    ) Error!@TypeOf(val) {
        //todo: doesn't have to be Val type, can be tagged?
        switch (Val.cast(val).tag()) {
            inline else => |tag| {
                const T = wabi.type_of.typeOfTag(tag);
                const v: T.Ptr = @ptrCast(val);
                if (T.smallness == .always or (T.smallness == .sometimes and v.isSmall())) {
                    const res = try v.move(self);
                    return @as(@TypeOf(val), @ptrCast(res));
                } else {
                    const orig_offset = self.orig.offsetOf(v);

                    if (self.refs.get(orig_offset)) |dest_offset| {
                        const res = self.dest.fromOffset(T.Ptr, dest_offset);
                        return @as(@TypeOf(val), @ptrCast(res));
                    } else {
                        // const dest_offset = @as(Offset, @intCast(self.dest.heap));
                        const res = try v.move(self);
                        const dest_offset = self.dest.offsetOf(res);
                        try self.refs.put(orig_offset, dest_offset);
                        return @as(@TypeOf(val), @ptrCast(res));
                    }
                }
            },
        }
    }

    pub fn memMove(self: *Self, val: anytype) !@TypeOf(val) {
        const T = @typeInfo(@TypeOf(val)).Pointer.child;
        const next = try self.dest.one(T);
        next.* = val.*;
        return next;
    }

    pub fn copy(
        self: *Self,
        val: anytype,
    ) !void {
        switch (Val.cast(val).tag()) {
            inline else => |tag| {
                const T = wabi.type_of.typeOfTag(tag);
                const res = try T.copy(@as(T.Ptr, @ptrCast(val)), self);
                self.scan += res;
            },
        }
    }

    pub inline fn one(self: *Self, val: anytype) !@TypeOf(val) {
        const res = try self.many(.{val});
        return res[0];
    }

    // many is used to avoid duplicates in subtrees common to all the copied stuff
    pub fn many(self: *Self, vals: anytype) !@TypeOf(vals) {
        var res: @TypeOf(vals) = undefined;
        self.reset();
        inline for (vals, 0..) |v, j| res[j] = try self.add(v);
        try self.run();
        return res;
    }

    pub fn reset(self: *Self) void {
        self.scan = self.dest.heap;
    }

    pub fn add(self: *Self, val: anytype) !@TypeOf(val) {
        switch (@typeInfo(@TypeOf(val))) {
            .Optional => {
                return if (val) |v| @ptrCast(try self.move(v)) else null;
            },
            else => {
                return @ptrCast(try self.move(val));
            },
        }
    }

    pub fn run(self: *Self) !void {
        while (self.scan < self.dest.heap) {
            const candidate = self.dest.fromOffset(*Word, @as(Offset, @intCast(self.scan)));
            try self.copy(candidate);
        }
    }
};

test "copy hits the refs table" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();
    const cur = try Val.from(&orig, Block, "this is a binary");
    const nxt = try ctx.move(cur);
    const nxt_ = try ctx.move(cur);
    try std.testing.expect(nxt == nxt_);
}

const Vm = wabi.vm.Vm;

const vmConfig = .{
    .allocator = std.testing.allocator,
    .mem_size = 10 * 1_024,
};

test "copy env0" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();

    var out_buf: [10]u8 = undefined;
    _ = try vm.evalString("0", out_buf[0..10]);
    var orig = &vm.mem;
    var dest = try Mem.init(std.testing.allocator, .{
        .mem_size = @intCast(orig.space.len),
    });
    defer dest.deinit();

    const cur = Val.cast(vm.env);
    var ctx = try Copy.init(orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const nxt = try ctx.one(cur);
    //todo: do real tests
    try std.testing.expect(nxt != cur);
}
