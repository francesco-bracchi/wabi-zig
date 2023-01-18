const std = @import("std");
const wabi = @import("wabi.zig");
const Tag = wabi.types.Tag;
const Word = wabi.types.Word;
const UInt = wabi.types.UInt;
const Mem = wabi.mem.Mem;
const word_align = @alignOf(Word);

const testing = std.testing;

pub const Block = packed struct(Word) {
    tag: Tag,
    len: UInt,

    const Self = @This();
    pub const Ptr = *align(word_align) Self;
    pub const smallness = .sometimes;

    pub fn from(mem: *Mem, raw: anytype) !Ptr {
        const self = try mem.one(Block);
        // todo: parameterize this u32?
        var selfData = try mem.alloc(raw.len);
        self.tag = .BBlk;
        self.len = @as(UInt, @intCast(raw.len));
        for (selfData, 0..) |*b, j| b.* = raw[j];
        return self;
    }

    pub fn isSmall(self: Ptr) bool {
        return self.len == 0;
    }

    pub fn data(
        self: Ptr,
    ) []u8 {
        const sizeOfMe = @sizeOf(Self);
        const unlimited = @as([*]u8, @ptrFromInt(@intFromPtr(self) + sizeOfMe));
        return unlimited[0..self.len];
    }

    pub fn concat(
        left: Ptr,
        mem: *Mem,
        right: Ptr,
    ) !Ptr {
        const self = try mem.one(Block);
        const self_len = left.len + right.len;
        var self_data = try mem.alloc(self_len);
        var left_data = left.data();
        const left_len = left_data.len;
        var right_data = right.data();
        for (left_data, 0..) |byte, j| self_data[j] = byte;
        for (right_data, 0..) |byte, j| self_data[j + left_len] = byte;

        self.tag = .BBlk;
        self.len = self_len;
        return self;
    }

    pub fn isEqual(left: Ptr, right: Ptr, _: *Mem) bool {
        if (left.len != right.len) return false;
        const left_data = left.data();
        const right_data = right.data();
        return std.mem.eql(u8, left_data, right_data);
    }

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        const res = try Self.from(ctx.dest, self.data());
        return res;
    }

    pub inline fn copy(
        self: Ptr,
        _: anytype,
    ) !Word {
        return @sizeOf(Self) + Mem.binSize(self.len);
    }
};

test "new binary empty block" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const b = try Block.from(&mem, "");
    try testing.expect(b.len == 0);
    try testing.expect(b.tag == .BBlk);
}

test "new binary nonempty block" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const b = try Block.from(&mem, "Hello World!");
    try testing.expect(b.len == 12);
    try testing.expect(b.tag == .BBlk);
}

test "data" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    _ = try mem.alloc(10);
    const b = try Block.from(&mem, "Hello World!");
    const d = b.data();
    try testing.expectEqualSlices(u8, d, "Hello World!");
}

test "copy binary" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();
    const cur = try Block.from(&orig, "this is a binary");
    const nxt = try ctx.one(cur);
    try std.testing.expect(nxt.tag == .BBlk);
    try std.testing.expect(std.mem.eql(u8, nxt.data(), cur.data()));
}
