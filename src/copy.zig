const std = @import("std");
const Mem = @import("mem.zig").Mem;
const types = @import("types.zig");
const Word = types.Word;
const Offset = Mem.Offset;
const HashMap = std.AutoHashMap(Offset, Offset);

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
        const smallness = @typeInfo(@TypeOf(val)).Pointer.child.smallness;
        if (smallness == .always or (smallness == .sometimes and val.isSmall())) {
            return try val.move(self);
        }
        const orig_offset = self.orig.offsetOf(val);

        if (self.refs.get(orig_offset)) |dest_offset| {
            return self.dest.fromOffset(@TypeOf(val), dest_offset);
        }

        const res = try val.move(self);
        const dest_offset = self.dest.offsetOf(res);
        try self.refs.put(orig_offset, dest_offset);
        return res;
    }

    pub fn copy(
        self: *Self,
        val: anytype,
    ) !void {
        const delta = try val.copy(self);
        self.scan += delta;
    }

    pub inline fn memMove(self: *Self, val: anytype) !@TypeOf(val) {
        const T = @typeInfo(@TypeOf(val)).Pointer.child;
        const next = try self.dest.one(T);
        next.* = val.*;
        return next;
    }

    pub inline fn one(self: *Self, val: anytype) !@TypeOf(val) {
        const vals = [1]@TypeOf(val){val};
        const res = try self.many(vals);
        return res[0];
    }

    // many is used to avoid duplicates in subtrees common to all the copied stuff
    pub fn many(self: *Self, vals: anytype) !@TypeOf(vals) {
        const E = @typeInfo(@TypeOf(vals)).Array.child;
        var res: @TypeOf(vals) = undefined;
        self.reset();
        inline for (vals, 0..) |v, j| res[j] = try self.add(v);
        try self.run(E);
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

    pub fn run(self: *Self, comptime E: type) !void {
        const EPtr = *align(@sizeOf(Word)) (@typeInfo(E).Pointer.child);
        while (self.scan < self.dest.heap) {
            const candidate = self.dest.fromOffset(EPtr, @as(Offset, @intCast(self.scan)));
            try self.copy(candidate);
        }
    }
};

const Elem = struct {
    a: Word,
    b: Word,

    pub const smallness = .never;

    const Self = @This();
    pub const Ptr = *align(@sizeOf(Word)) Self;

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        const res = try ctx.dest.one(Self);
        res.a = self.a;
        res.b = self.b;
        return res;
    }

    pub inline fn copy(
        self: Ptr,
        _: anytype,
    ) !Word {
        return @sizeOf(Self) + Mem.binSize(self.len);
    }
};

test "copy hits the refs table" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();
    const cur = try orig.one(Elem);
    cur.a = 100;
    cur.b = 200;
    const nxt = try ctx.move(cur);
    const nxt_ = try ctx.move(cur);
    try std.testing.expect(nxt == nxt_);
}
