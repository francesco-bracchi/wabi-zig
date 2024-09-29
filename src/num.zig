const std = @import("std");
const types = @import("types.zig");
const Tag = types.Tag;
const Word = types.Word;
const SInt = types.SInt;
const Mem = @import("mem.zig").Mem;
const Copy = @import("copy.zig").Copy;

const word_align = @alignOf(Word);

const testing = std.testing;

pub const Fix = packed struct(Word) {
    tag: Tag,
    val: SInt,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub const smallness = .always;

    pub fn from(
        mem: *Mem,
        val: anytype,
    ) !Ptr {
        const self = try mem.one(Self);
        self.* = .{
            .tag = .Fix,
            .val = @as(SInt, val),
        };
        return self;
    }

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        return try ctx.memMove(self);
    }

    pub fn copy(
        _: Ptr,
        _: anytype,
    ) !Word {
        return @sizeOf(Self);
    }
};

test "new fix" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const val = std.crypto.random.int(SInt);
    const num = try Fix.from(&mem, val);
    try testing.expect(num.tag == .Fix);
    try testing.expect(num.val == val);
}

test "copy fix" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();

    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();
    const cur = try Fix.from(&orig, 129);
    const nxt = try ctx.one(cur);
    try std.testing.expect(nxt != cur);
    try std.testing.expect(nxt.val == cur.val);
}
