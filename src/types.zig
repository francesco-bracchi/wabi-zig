const std = @import("std");
const testing = std.testing;

pub const wordSize = 64;
pub const tagSize = 5;
pub const valSize = wordSize - tagSize;

pub const Word = @Type(.{
    .Int = .{
        .signedness = .unsigned,
        .bits = wordSize,
    },
});

pub const UInt = @Type(.{
    .Int = .{
        .signedness = .unsigned,
        .bits = valSize,
    },
});

pub const SInt = @Type(.{
    .Int = .{
        .signedness = .signed,
        .bits = valSize,
    },
});

pub const max_word = ~@as(Word, 0);
pub const max_uint = ~@as(UInt, 0);
pub const max_sint = @as(SInt, ~@as(UInt, 0) >> 1);
pub const min_sint = ~max_sint;

const tagInt = @Type(.{
    .Int = .{
        .signedness = .unsigned,
        .bits = tagSize,
    },
});

// todo: consider a compsoed scheme like erlang does (11 for immediates ...)
pub const Tag = enum(tagInt) {
    Fix = 0,
    Flo = 1,
    Big = 2,
    Rat = 3,
    BBlk = 4,
    BSub = 5,
    BCnc = 6,
    Sym = 7,
    Kwd = 8,
    LVec = 9,
    BOpr = 10,
    BApp = 11,
    DOpr = 12,
    DApp = 13,
    Env = 14,
    Kont = 15,
    KEval = 16,
    KApply = 17,
    KCall = 18,
    KSel = 19,
    KArgs = 20,
    KDef = 21,
    KProg = 22,
    KMeta = 23,

    X18 = 24,
    X19 = 25,
    X1a = 26,
    X1b = 27,
    X1c = 28,

    Nul = 29,
    Ref = 30,
    Fwd = 31,
};

pub const Bytes = @Type(.{
    .Array = .{
        .len = wordSize >> 3,
        .child = u8,
        .sentinel = null,
    },
});

test "word maxes & mins" {
    try testing.expectError(error.Overflow, std.math.add(Word, max_word, 1));
    try testing.expectError(error.Overflow, std.math.add(UInt, max_uint, 1));
    try testing.expectError(error.Overflow, std.math.add(SInt, max_sint, 1));
    try testing.expectError(error.Overflow, std.math.sub(SInt, min_sint, 1));
}

// test "tagOf function" {
//     const T = packed struct {
//         tag: Tag,
//         val: UInt,
//         v1: Word,
//     };

//     var t = T{
//         .tag = .Fix,
//         .val = 123,
//         .v1 = 0,
//     };

//     try testing.expectEqual(tagOf(&t), .Fix);
//     try testing.expect(tagOf(&t) == .Fix);
// }
