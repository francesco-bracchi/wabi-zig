const std = @import("std");
const wabi = @import("wabi.zig");
const Tag = wabi.types.Tag;
const Word = wabi.types.Word;
const Fix = wabi.num.Fix;
// TODO: a better name, Buffer or whatever, chunk?
const Block = wabi.bin.Block;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const VectorList = wabi.list.VectorList;
const Env = wabi.env.Env;
const Builtin = wabi.comb.Builtin;
const Derived = wabi.comb.Derived;
const Control = wabi.comb.Control;

const Mem = wabi.mem.Mem;
const SInt = wabi.types.SInt;
const testing = std.testing;

pub const Val = packed union {
    tagged: packed struct(Word) {
        tag_: Tag,
        _: wabi.types.UInt,
    },
    word: Word,
    fix: Fix,
    block: Block,
    sym: Sym,
    kwd: Kwd,
    list: VectorList,
    env: Env,
    builtin: Builtin,
    derived: Derived,
    control: Control,

    const Self = @This();

    const word_align = @alignOf(Word);

    pub const Ptr = *align(word_align) Self;

    pub fn from(
        mem: *Mem,
        comptime T: type,
        data: anytype,
    ) !Ptr {
        const res = try T.from(mem, data);
        return @as(Ptr, @ptrCast(res));
    }

    pub const smallness = .sometimes;

    pub fn isSmall(self: Ptr) bool {
        switch (self.tag()) {
            inline else => |t| {
                const T = wabi.type_of.typeOfTag(t);
                return switch (T.smallness) {
                    .always => true,
                    .sometimes => T.isSmall(@ptrCast(self)),
                    .never => false,
                    else => @compileError("smallness not defined"),
                };
            },
        }
    }

    pub fn isOperative(self: Ptr) bool {
        return switch (self.tag()) {
            .BOpr, .DOpr => true,
            else => false,
        };
    }

    pub fn isApplicative(self: Ptr) bool {
        return switch (self.tag()) {
            .LVec, .BApp, .DApp, .Kont => true,
            else => false,
        };
    }
    pub fn cast(val: anytype) Ptr {
        return @as(Ptr, @ptrCast(val));
    }

    pub fn tag(self: Ptr) Tag {
        return @as(Tag, @enumFromInt(@intFromEnum(self.tagged.tag_)));
    }

    pub fn isEqual(left: Ptr, right: Ptr, mem: *Mem) bool {
        switch (left.tag()) {
            .Fix, .Sym, .Kwd, .BOpr, .BApp => {
                return left.word == right.word;
            },
            .BBlk => {
                return switch (right.tag()) {
                    .BBlk => left.block.isEqual(&right.block, mem),
                    else => false,
                };
            },
            .LVec => {
                return switch (right.tag()) {
                    .LVec => left.list.isEqual(&right.list, mem),
                    else => false,
                };
            },
            .Kont, .Env, .DOpr, .DApp => {
                return left == right;
            },
            else => {
                unreachable;
            },
        }
    }

    pub fn toString(
        val: Ptr,
        mem: *Mem,
        out: anytype,
    ) !void {
        var bin_wrt = std.io.fixedBufferStream(out);
        var wrt = wabi.write.valueWriter(bin_wrt.writer(), mem);
        try wrt.writeVal(val);
    }

    pub fn fromString(
        mem: *Mem,
        in: anytype,
    ) !Ptr {
        // _ = .{mem};
        // std.debug.print("intype: {}\n", .{@TypeOf(in)});
        // return error.OutOfMemory;
        var str = std.io.fixedBufferStream(@constCast(in));
        var reader = str.reader();
        var read = wabi.read.valueReader(reader, mem);
        return try read.readVal();
    }
};

test "new fix" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const num = std.crypto.random.int(SInt);
    var val = try Val.from(&mem, Fix, num);
    try testing.expectEqual(val.tag(), .Fix);
    try testing.expectEqual(val.fix.val, num);
}
