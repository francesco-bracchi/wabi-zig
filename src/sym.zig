const std = @import("std");
const testing = std.testing;
const types = @import("types.zig");
const Mem = @import("mem.zig").Mem;
const Offset = Mem.Offset;
const Block = @import("bin.zig").Block;
const Copy = @import("copy.zig").Copy;
const Word = types.Word;
const Tag = types.Tag;
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

const Val = packed union {
    tagged: packed struct(Word) {
        tag: Tag,
        _: types.UInt,
    },
    sym: Sym,
    bin: Block,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;
    pub const smallness = .sometimes;

    pub fn isSmall(self: Ptr) bool {
        switch (self.tagged.tag) {
            .Sym => {
                return true;
            },
            .BBlk => {
                return self.bin.isSmall();
            },
            else => unreachable,
        }
    }

    pub fn move(self: Ptr, ctx: anytype) !Ptr {
        switch (self.tagged.tag) {
            .Sym => {
                return @ptrCast(try self.sym.move(ctx));
            },
            .BBlk => {
                return @ptrCast(try self.bin.move(ctx));
            },
            else => unreachable,
        }
    }

    pub fn copy(self: Ptr, ctx: anytype) !Word {
        switch (self.tagged.tag) {
            .Sym => {
                return try self.sym.copy(ctx);
            },
            .BBlk => {
                return try self.bin.copy(ctx);
            },
            else => unreachable,
        }
    }
};

test "copy symbol" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const str = try Block.from(&orig, "sym");
    const cur = try Sym.from(&orig, str);
    const val: Val.Ptr = @ptrCast(cur);

    const nxt = try ctx.one(val);

    try std.testing.expect(nxt.sym.tag == .Sym);
    try std.testing.expect(std.mem.eql(
        u8,
        val.sym.binOf(&orig).data(),
        nxt.sym.binOf(&dest).data(),
    ));
}
