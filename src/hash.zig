const std = @import("std");
const wabi = @import("wabi.zig");
const Word = wabi.types.Word;
const testing = std.testing;

const Config = struct {
    level: Word = 0,
    Int: type = Word,
};

const euler: Word = @as(Word, @truncate(2718281828459045235));
const pi: Word = @as(Word, @truncate(3141592653589793238));

pub fn UniversalHash(
    comptime config: Config,
) type {
    //todo: add test cases
    return struct {
        a: Int = @as(Int, @intCast(pi)),
        h: Int = 0,

        const Int = config.Int;

        const b: Int = @as(Int, @intCast(euler));
        const Self = @This();
        // level is always > 0
        const level = 1 + config.level;

        pub fn init(self: *Self) void {
            self.a = pi;
            self.h = 0;
        }

        pub fn step(
            self: *Self,
            bytes: anytype,
        ) void {
            for (bytes) |k| {
                self.h = self.a *% self.h *% level +% k;
                self.a *%= b;
            }
        }

        pub fn hash(
            self: *Self,
        ) Int {
            return self.h;
        }
    };
}

test "hash empty" {
    const H = UniversalHash(.{});
    var h: H = .{};

    try testing.expect(h.hash() == 0);
}

test "hash empty string" {
    const H = UniversalHash(.{});
    var h: H = .{};
    h.step("");
    try testing.expect(h.hash() == 0);
}

test "hash a string" {
    const H = UniversalHash(.{});
    var h: H = .{};
    h.step("foo");
    try testing.expect(h.hash() != 0);
}

test "hash a string, different level" {
    const H0 = UniversalHash(.{ .level = 0 });
    const H1 = UniversalHash(.{ .level = 1 });
    var h0: H0 = .{};
    var h1: H1 = .{};
    h0.step("foo");
    h1.step("foo");
    try testing.expect(h0.hash() != h1.hash());
}
