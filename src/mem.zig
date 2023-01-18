const std = @import("std");
const wabi = @import("wabi.zig");

const Word = wabi.types.Word;
const Allocator = std.mem.Allocator;
const UniversalHash = wabi.hash.UniversalHash;
const shift_for_word = @ctz(@as(u64, @intCast(@sizeOf(Word))));
const word_size = @sizeOf(Word);
const word_align = @alignOf(Word);
const word_div_mask = word_size - 1;

const default_reserve = 1024 * 4;

pub const Mem = struct {
    space: []align(word_align) u8,
    len: Size,
    reserve: usize = default_reserve,
    heap: Word,
    allocator: Allocator,
    intern_table: HashMap,

    const Self = @This();

    pub const Size = wabi.types.UInt;
    pub const Offset = Size;

    // const HashMap = std.StringHashMap(
    //     Offset,
    // );

    pub const H = UniversalHash(.{
        .level = 2,
        .Int = u64,
    });

    const HCtx = struct {
        pub const hash = struct {
            fn hash(
                _: anytype,
                key: []u8,
            ) u64 {
                var h: H = .{};
                h.step(key);
                return h.hash();
            }
        }.hash;
        pub const eql = struct {
            fn eql(
                _: anytype,
                as: []u8,
                bs: []u8,
            ) bool {
                return std.mem.eql(u8, as, bs);
                // if (as.len != bs.len) return false;
                // for (as, bs) |a, b| if (a != b) return false;
                // return true;
            }
        }.eql;
    };

    pub const HashMap = std.HashMap([]u8, Offset, HCtx, 80);

    pub const Config = struct {
        mem_size: Size,
        reserve: Size = 0,
    };

    pub fn init(
        allocator: Allocator,
        config: Config,
    ) error{
        MemoryOverflow,
        OutOfMemory,
    }!Self {
        var self: Self = undefined;
        self.space = try allocator.allocWithOptions(u8, config.mem_size, word_align, null);
        self.reserve = config.reserve;
        self.len = config.mem_size - config.reserve;
        self.heap = 0;
        self.allocator = allocator;
        self.intern_table = HashMap.init(allocator);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.intern_table.deinit();
        self.allocator.free(self.space);
        self.heap = 0;
    }

    pub fn update(
        self: *Self,
        config: Config,
    ) error{
        MemoryOverflo,
        OutOfMemory,
    }!void {
        const allocator = self.allocator;
        self.space = try allocator.realloc(self.space, config.mem_size);
        self.reserve = config.reserve;
        self.len = config.mem_size - config.reserve;
    }

    pub fn reset(self: *Self) void {
        self.heap = 0;
        self.intern_table.deinit();
        self.intern_table = HashMap.init(self.allocator);
    }

    pub const E = error{
        MemoryOverflow,
        OutOfMemory,
    };

    fn allocBytes(
        self: *Self,
        size: Word,
    ) E![*]align(word_size) u8 {
        if (self.heap + size >= self.len) return error.MemoryOverflow;
        const res: [*]align(word_align) u8 = @alignCast(self.space.ptr + self.heap);
        self.heap += size;
        return res;
    }

    pub fn one(
        self: *Self,
        comptime T: type,
    ) E!*align(word_size) T {
        if (@sizeOf(T) % @sizeOf(Word) != 0) @compileError("unaligned entity");
        const res = try self.allocBytes(@sizeOf(T));
        return @as(*align(word_size) T, @ptrCast(res));
    }
    pub fn many(
        self: *Self,
        comptime T: type,
        num: anytype,
    ) E![]align(word_size) T {
        if (@sizeOf(T) % @sizeOf(Word) != 0) @compileError("unaligned entity");
        const res = try self.allocBytes(@as(Word, @intCast(@sizeOf(T) * num)));
        return @as([*]align(word_size) T, @ptrCast(@alignCast(res)))[0..num];
    }

    pub fn offsetOf(
        self: Self,
        val: anytype,
    ) Offset {
        return @as(Offset, @intCast(@intFromPtr(@as(*u8, @ptrCast(val))) - @intFromPtr(self.space.ptr)));
    }

    pub fn fromOffset(
        self: Self,
        comptime T: type,
        offset: Offset,
    ) T {
        return @as(T, @ptrCast(@alignCast(self.space.ptr + offset)));
    }

    pub fn alloc(
        self: *Self,
        num: anytype,
    ) ![]u8 {
        const size = binSize(@intCast(num));
        const res = try self.allocBytes(size);
        return res[0..num];
    }

    pub fn binSize(
        num: Word,
    ) Word {
        return if (num & word_div_mask == 0) num else ((num >> shift_for_word) + 1) << shift_for_word;
    }

    pub fn intern(
        self: *Self,
        comptime T: type,
        bin: []u8,
    ) E!T.Ptr {
        if (self.intern_table.get(bin)) |offset| {
            return self.fromOffset(T.Ptr, offset);
        } else {
            const block = try T.from(self, bin);
            try self.intern_table.put(block.data(), self.offsetOf(block));
            // std.debug.print("interned '{s}'@#{x}\n", .{ bin, HCtx.hash(null, bin) });
            return block;
        }
    }

    pub fn useReserve(self: *Self) void {
        self.len = @intCast(self.space.len);
    }

    pub fn dbg(
        self: *Self,
    ) void {
        //todo: 2 must be inferred from Word size
        const shft = 2;
        for (0..(self.heap >> shft)) |j| {
            const offset = @as(Offset, @intCast(j)) << shft;
            const bs = self.fromOffset(*[4]u8, offset);
            const val = self.fromOffset(wabi.val.Val.Ptr, offset);
            std.debug.print("{: >5} {b:0>27} {b:0>5} {}\n", .{
                offset,
                val.tagged._,
                @intFromEnum(val.tagged.tag_),
                val.tag(),
            });
            _ = .{ bs, val };
        }
    }
};

///// TESTING

const testing = std.testing;

test "init" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    try testing.expect(mem.space.len == 1_024);
}

test "fail" {
    try testing.expectError(error.OutOfMemory, Mem.init(testing.failing_allocator, .{ .mem_size = 1_024 }));
}

const TT = packed struct {
    len: Mem.Size,
    tag: wabi.types.Tag,
    wrd: wabi.types.Word,

    const Ptr = *align(word_align) TT;
};

test "one" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    var tt = try mem.one(TT);
    tt.tag = .Fix;
    try testing.expect(tt.tag == .Fix);
    try testing.expect(mem.heap == m0 + 2 * word_size);
}

test "many" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    var tt = try mem.many(u128, 7);
    tt[0] = 11;
    try testing.expect(tt[0] == 11);
}

test "offset" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    _ = try mem.one(TT);
    var tt = try mem.one(TT);
    const os = mem.offsetOf(tt);
    try testing.expectEqual(os, @as(Mem.Size, @intCast(m0 + 2 * word_size)));
    try testing.expectEqual(mem.heap, m0 + 4 * word_size);
}

test "from offset" {
    const tt0 = .{ .len = 10, .tag = .Fix, .wrd = 19 };

    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    _ = try mem.one(TT);
    var tt = try mem.one(TT);
    const os = mem.offsetOf(tt);
    tt.* = tt0;
    const tt1 = mem.fromOffset(TT.Ptr, os);
    try testing.expect(tt1.len == tt0.len);
    try testing.expect(tt1.tag == tt0.tag);
    try testing.expect(tt1.wrd == tt0.wrd);
}

test "allocate empty u8" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    var x = try mem.alloc(0);
    try testing.expect(mem.heap == m0);
    try testing.expect(x.len == 0);
}

test "allocate u8 within a word" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    var x = try mem.alloc(1);
    try testing.expect(mem.heap == m0 + word_size);
    try testing.expect(x.len == 1);
}

test "allocate u8 with more than a word" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    var x = try mem.alloc(11);
    try testing.expect(x.len == 11);
}

test "allocate u8 exact words" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    var x = try mem.alloc(16);
    try testing.expect(x.len == 16);
}

const II = packed struct {
    len: Word,

    const Self = @This();
    pub const Ptr = *align(word_align) Self;

    pub fn from(mem: *Mem, raw: []u8) !Ptr {
        const res = try mem.one(Self);
        var selfData = try mem.alloc(raw.len);
        for (selfData, 0..) |*b, j| b.* = raw[j];
        res.len = raw.len;
        return res;
    }

    pub fn data(
        self: Ptr,
    ) []u8 {
        const sizeOfMe = @sizeOf(Self);
        const unlimited = @as([*]u8, @ptrFromInt(@intFromPtr(self) + sizeOfMe));
        return unlimited[0..self.len];
    }
};

test "intern" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    defer mem.deinit();
    var x = try mem.intern(II, @constCast("t"));
    var y = try mem.intern(II, @constCast("t"));
    try testing.expect(x == y);
}

test "update" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 25 * 1_024 });
    try mem.update(.{ .mem_size = 50 * 1024 });
    mem.deinit();
}
