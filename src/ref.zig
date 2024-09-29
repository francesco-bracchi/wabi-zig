const std = @import("std");
const wabi = @import("wabi.zig");
const Word = wabi.types.Word;
const Tag = wabi.types.Tag;
const Mem = wabi.mem.Mem;
const Offset = Mem.Offset;

const testing = std.testing;
const word_align = @alignOf(Word);

pub const Smallness = enum { always, never, sometimes };

//todo: a Ref parametric on type, and a default Ref(Val)
pub fn RefTo(comptime T: type, comptime args: anytype) type {
    const smallness: Smallness = T.smallness;
    return switch (smallness) {
        .always => SmallRefTo(T, args),
        .never => HugeRefTo(T, args),
        .sometimes => MixedRefTo(T, args),
    };
}

pub fn SmallRefTo(comptime T: type, comptime args: anytype) type {
    const ChildPtr = *align(word_align) T;

    if (args.nullable) {
        return packed struct(Word) {
            tag: Tag,
            offset: Offset,

            const Self = @This();
            pub const Ptr = *align(word_align) Self;

            pub fn get(
                self: *Self,
                _: *Mem,
            ) ?*ChildPtr {
                if (self.tag == .Nul) return null;
                return @as(ChildPtr, @ptrCast(self));
            }

            pub fn set(
                self: *Self,
                _: *Mem,
                val: ?ChildPtr,
            ) void {
                if (val) |v| {
                    const w_ptr: *Word = @as(*Word, @ptrCast(v));
                    self.word = w_ptr.*;
                } else {
                    self.tag = .Nul;
                }
            }
            //todo: evaluate if it's the case of creating special cases for symbols and keywords
            pub fn copy(
                self: Ptr,
                ctx: anytype,
            ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
                switch (self.tag) {
                    .Nul => {},
                    else => {
                        const myself: ChildPtr = @ptrCast(self);
                        try ctx.copy(myself);
                    },
                }
            }
        };
    }
    return packed struct(Word) {
        word: Word,

        const Self = @This();
        pub const Ptr = *align(word_align) Self;

        pub fn get(
            self: *Self,
            _: *Mem,
        ) ChildPtr {
            return @as(ChildPtr, @ptrCast(self));
        }

        pub fn set(
            self: *Self,
            _: *Mem,
            val: ChildPtr,
        ) void {
            const w_ptr: *Word = @as(*Word, @ptrCast(val));
            self.word = w_ptr.*;
        }

        pub fn copy(
            self: Ptr,
            ctx: anytype,
        ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
            const myself: ChildPtr = @ptrCast(self);
            try ctx.copy(myself);
        }
    };
}

pub fn HugeRefTo(comptime T: type, comptime args: anytype) type {
    const ChildPtr = *align(word_align) T;

    if (args.nullable) {
        return packed struct(Word) {
            tag: Tag,
            offset: Offset,

            const Self = @This();
            pub const Ptr = *align(word_align) Self;

            pub fn get(
                self: *Self,
                mem: *Mem,
            ) ?ChildPtr {
                if (self.tag == .Nul) return null;
                const res = mem.fromOffset(ChildPtr, self.offset);
                return res;
            }

            pub fn set(
                self: *Self,
                mem: *Mem,
                val: ?ChildPtr,
            ) void {
                if (val) |v| {
                    const val_offset = mem.offsetOf(v);
                    self.offset = val_offset;
                    self.tag = .Ref;
                    return;
                }
                self.tag = .Nul;
            }
            inline fn isSmall(
                self: Ptr,
            ) bool {
                return self.tag == .Nul;
            }

            pub fn copy(
                self: Ptr,
                ctx: anytype,
            ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
                switch (self.tag) {
                    .Nul => {},
                    inline else => {
                        const orig = ctx.orig.fromOffset(ChildPtr, self.offset);
                        const dest = try ctx.move(orig);
                        self.set(ctx.dest, dest);
                    },
                }
            }
        };
    }
    return packed struct(Word) {
        _: Tag,
        offset: Offset,

        const Self = @This();
        pub const Ptr = *align(word_align) Self;

        pub fn get(
            self: *Self,
            mem: *Mem,
        ) ChildPtr {
            return mem.fromOffset(ChildPtr, self.offset);
        }

        pub fn set(
            self: *Self,
            mem: *Mem,
            val: ChildPtr,
        ) void {
            self.offset = mem.offsetOf(val);
        }
        inline fn isSmall(
            _: Ptr,
        ) bool {
            return false;
        }

        pub fn copy(
            self: Ptr,
            ctx: anytype,
        ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
            const orig = ctx.orig.fromOffset(ChildPtr, self.offset);
            const dest = try ctx.move(orig);
            self.offset = ctx.dest.offsetOf(dest);
        }
    };
}

pub fn MixedRefTo(comptime T: type, comptime args: anytype) type {
    const ChildPtr = *align(word_align) T;

    if (args.nullable) {
        return packed union {
            word: Word,
            a_ref: packed struct(Word) {
                tag: Tag,
                offset: Offset,
            },

            const Self = @This();
            pub const Ptr = *align(word_align) Self;

            pub fn get(
                self: *Self,
                mem: *Mem,
            ) ?ChildPtr {
                switch (self.a_ref.tag) {
                    .Nul => {
                        return null;
                    },
                    .Ref => {
                        return mem.fromOffset(ChildPtr, self.a_ref.offset);
                    },
                    else => {
                        return @as(ChildPtr, @ptrCast(self));
                    },
                }
            }

            pub fn set(
                self: *Self,
                mem: *Mem,
                val: ?ChildPtr,
            ) void {
                if (val) |v| {
                    if (v.isSmall()) {
                        const w_ptr: *Word = @ptrCast(v);
                        self.word = w_ptr.*;
                        return;
                    }

                    const val_offset = mem.offsetOf(v);
                    self.a_ref.tag = .Ref;
                    self.a_ref.offset = val_offset;
                    return;
                }
                self.a_ref.tag = .Nul;
            }

            pub fn copy(
                self: Ptr,
                ctx: anytype,
            ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
                const tag = @as(Tag, @enumFromInt(@intFromEnum(self.a_ref.tag)));
                switch (tag) {
                    .Nul => {},
                    .Ref => {
                        const orig = ctx.orig.fromOffset(ChildPtr, self.a_ref.offset);
                        const dest = try ctx.move(orig);
                        self.a_ref.offset = ctx.dest.offsetOf(dest);
                    },
                    else => {
                        const orig: ChildPtr = @ptrCast(self);
                        try ctx.copy(orig);
                    },
                }
            }
        };
    }

    return packed union {
        word: Word,
        a_ref: packed struct(Word) {
            tag: Tag,
            offset: Offset,
        },

        const Self = @This();

        pub fn get(
            self: *Self,
            mem: *Mem,
        ) ChildPtr {
            const tag = @as(Tag, @enumFromInt(@intFromEnum(self.a_ref.tag)));
            return switch (tag) {
                .Ref => mem.fromOffset(ChildPtr, self.a_ref.offset),
                else => @as(ChildPtr, @ptrCast(self)),
            };
        }

        pub fn set(
            self: *Self,
            mem: *Mem,
            val: ChildPtr,
        ) void {
            if (val.isSmall()) {
                const w_ptr: *Word = @as(*Word, @ptrCast(val));
                self.word = w_ptr.*;
                return;
            }

            const val_offset = mem.offsetOf(val);
            self.a_ref.tag = .Ref;
            self.a_ref.offset = val_offset;
        }

        pub fn copy(
            self: *Self,
            ctx: anytype,
        ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!void {
            const tag = @as(Tag, @enumFromInt(@intFromEnum(self.a_ref.tag)));
            switch (tag) {
                .Ref => {
                    const orig = ctx.orig.fromOffset(ChildPtr, self.a_ref.offset);
                    const dest = try ctx.move(orig);
                    self.a_ref.offset = ctx.dest.offsetOf(dest);
                },
                else => {
                    const orig: ChildPtr = @ptrCast(self);
                    _ = try ctx.copy(orig);
                },
            }
        }
    };
}

// TODO: tests

pub const Small = packed struct(Word) {
    val: Word,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub const smallness = .always;

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
pub const Big = packed struct {
    a: Word,
    b: Word,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub const smallness = .never;

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

test "ref to a small value (i.e. word fitting) is inlined" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const n0 = try mem.one(Small);
    n0.val = 13;
    var ref = try mem.one(RefTo(Small, .{ .nullable = false }));
    ref.set(&mem, n0);
    try testing.expect(n0 != ref.get(&mem));
    try testing.expectEqual(n0.val, ref.get(&mem).val);
}

test "ref to big entity is an actual reference" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const n0 = try mem.one(Big);
    n0.a = 123;
    n0.b = 567;
    var ref = try mem.one(RefTo(Big, .{ .nullable = false }));
    ref.set(&mem, n0);
    try testing.expectEqual(n0, ref.get(&mem));
}
