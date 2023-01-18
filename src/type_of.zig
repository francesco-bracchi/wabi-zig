const std = @import("std");
const wabi = @import("wabi.zig");
const Tag = wabi.types.Tag;
const Word = wabi.types.Word;

pub inline fn typeOfTag(comptime a_tag: Tag) type {
    return switch (a_tag) {
        .Fix => wabi.num.Fix,
        .BBlk => wabi.bin.Block,
        .Sym => wabi.sym.Sym,
        .Kwd => wabi.kwd.Kwd,
        .LVec => wabi.list.VectorList,
        .BOpr, .BApp => wabi.comb.Builtin,
        .DOpr, .DApp => wabi.comb.Derived,
        .Kont => wabi.comb.Control,
        .Env => wabi.env.Env,
        .KEval => wabi.cont.Eval,
        .KApply => wabi.cont.Apply,
        .KCall => wabi.cont.Call,
        .KArgs => wabi.cont.Args,
        .KDef => wabi.cont.Def,
        .KSel => wabi.cont.Select,
        .KProg => wabi.cont.Prog,
        .KMeta => wabi.meta.Meta,
        else => UndefinedType,
    };
}

pub const UndefinedType = packed struct {
    tag: Tag,
    ign: wabi.types.UInt,

    const Self = @This();

    const word_align = @alignOf(Word);

    pub const smallness = .never;

    pub const Ptr = *align(word_align) Self;

    pub fn move(
        self: Ptr,
        _: anytype,
    ) !*Self {
        std.debug.print("my tag: {}\n", .{self.tag});
        unreachable;
    }
    pub fn copy(
        _: Ptr,
        _: anytype,
    ) !Word {
        std.debug.print("copy unknown:\n", .{});
        // ctx.dest.dbg();
        unreachable;
    }
};
