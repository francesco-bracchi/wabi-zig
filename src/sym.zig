const std = @import("std");
const testing = std.testing;
const wabi = @import("wabi.zig");
const types = wabi.types;

const Mem = wabi.mem.Mem;
const Offset = wabi.mem.Mem.Offset;
const Block = wabi.bin.Block;

const Word = wabi.types.Word;
const Tag = wabi.types.Tag;
const word_align = @alignOf(Word);

pub const Sym = packed struct(Word) {
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
            .tag = .Sym,
            .bin_offset = offset,
        };
        return self;
    }

    pub fn binOf(
        self: Ptr,
        mem: *Mem,
    ) Block.Ptr {
        return mem.fromOffset(Block.Ptr, self.bin_offset);
    }

    pub fn fromString(
        mem: *Mem,
        s: anytype,
    ) !Ptr {
        const string = try mem.intern(Block, @constCast(s));
        // const string = try Block.from(mem, s);
        return Self.from(mem, string);
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
        const orig_bin = self.binOf(ctx.orig);
        const dest_bin = try ctx.dest.intern(Block, orig_bin.data());
        self.bin_offset = ctx.dest.offsetOf(dest_bin);
        return @sizeOf(Self);
    }
};

test "create a symbol" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    const string = try Block.from(&mem, "foo");
    const symbol = try Sym.from(&mem, string);
    try testing.expect(symbol.tag == .Sym);
    try testing.expect(symbol.bin_offset == m0);
}

test "copy symbol" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const str = try Block.from(&orig, "sym");
    const cur = try Sym.from(&orig, str);
    const nxt = try ctx.one(cur);

    try std.testing.expect(nxt.tag == .Sym);
    try std.testing.expect(std.mem.eql(
        u8,
        cur.binOf(&orig).data(),
        nxt.binOf(&dest).data(),
    ));
}
