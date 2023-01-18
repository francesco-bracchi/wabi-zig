const std = @import("std");
const wabi = @import("wabi.zig");
const types = wabi.types;
const testing = std.testing;

const Mem = wabi.mem.Mem;
const Offset = Mem.Offset;
const Block = wabi.bin.Block;

const Word = wabi.types.Word;
const Tag = wabi.types.Tag;
const word_align = @alignOf(Word);

pub const Kwd = packed struct(Word) {
    tag: Tag,
    bin_offset: Offset,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;
    pub const smallness = .always;

    pub fn from(
        mem: *Mem,
        val: anytype,
    ) !Ptr {
        const self = try mem.one(Self);
        const offset = mem.offsetOf(val);
        self.* = .{
            .tag = .Kwd,
            .bin_offset = offset,
        };
        return self;
    }

    pub fn binOf(self: Ptr, mem: *Mem) Block.Ptr {
        return mem.fromOffset(*Block, self.bin_offset);
    }

    pub fn fromString(mem: *Mem, s: []u8) !Ptr {
        const string = try mem.intern(Block, @constCast(s));
        return Self.from(mem, string);
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
    ) !Word {
        const orig_bin = self.binOf(ctx.orig);
        const dest_bin = try ctx.dest.intern(Block, orig_bin.data());
        self.bin_offset = ctx.dest.offsetOf(dest_bin);
        return @sizeOf(Self);
    }
};
// TODO: decide if _ is a symbol or a keyword

test "create a keyword" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;

    const string = try Block.from(&mem, "foo");
    const keyword = try Kwd.from(&mem, string);
    try testing.expect(keyword.tag == .Kwd);
    try testing.expect(keyword.bin_offset == m0);
}

test "copy keyword" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const str = try Block.from(&orig, "kwd");
    const cur = try Kwd.from(&orig, str);
    const nxt = try ctx.one(cur);

    try std.testing.expect(nxt.tag == .Kwd);
    try std.testing.expect(std.mem.eql(
        u8,
        cur.binOf(&orig).data(),
        nxt.binOf(&dest).data(),
    ));
}
