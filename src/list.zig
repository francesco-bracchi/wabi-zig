const std = @import("std");
const types = @import("types.zig");
const Word = types.Word;
const Tag = types.Tag;
const UInt = types.UInt;
const Mem = @import("mem.zig").Mem;
const RefTo = @import("ref.zig").RefTo;
const Copy = @import("copy.zig").Copy;

const word_align = @alignOf(Word);

const testing = std.testing;

// todo: make this parametric on the type `Val`, so it can be tested indipendently
pub fn VectorListOf(comptime T: type) type {
    const ChildPtr = *align(word_align) T;

    return packed struct(Word) {
        tag: Tag,
        len: UInt,

        const Self = @This();

        pub const Ptr = *align(word_align) Self;

        const empty = Self{
            .tag = .LVec,
            .len = 0,
        };

        const Ref = RefTo(T, .{ .nullable = false });

        pub fn create(
            mem: *Mem,
            len: anytype,
        ) !Ptr {
            const self = try mem.one(Self);
            _ = try mem.many(Ref, len);
            self.* = .{
                .tag = .LVec,
                .len = @as(UInt, @intCast(len)),
            };
            return self;
        }

        pub fn from(
            mem: *Mem,
            lst: anytype,
        ) !Ptr {
            const self = try create(mem, lst.len);
            const selfRefs = self.refs();
            for (lst, 0..) |val, j|
                selfRefs[j].set(mem, val);

            return self;
        }

        pub const smallness = .sometimes;

        pub fn isSmall(self: Ptr) bool {
            return self.len == 0;
        }

        pub fn refs(self: Ptr) []align(word_align) Ref {
            const refs_ = @as([*]Ref, @ptrFromInt(@intFromPtr(self) + @sizeOf(Self)));
            return refs_[0..self.len];
        }

        pub fn get(self: Ptr, mem: *Mem, j: usize) ChildPtr {
            return self.refs()[j].get(mem);
        }

        pub fn set(
            self: Ptr,
            mem: *Mem,
            j: anytype,
            val: ChildPtr,
        ) !void {
            return self.refs()[j].set(mem, val);
        }

        pub fn left(self: Ptr, mem: *Mem) ?ChildPtr {
            if (self.len == 0) return null;
            const leftVal = self.refs()[0].get(mem);
            return leftVal;
        }

        pub fn right(self: Ptr, mem: *Mem) ?ChildPtr {
            if (self.len == 0) return null;
            const rightVal = self.refs()[self.len - 1].get(mem);
            return rightVal;
        }

        pub fn pushLeft(
            lst: Ptr,
            mem: *Mem,
            val: ChildPtr,
        ) !Ptr {
            const self = try mem.one(Self);
            const selfRefs = try mem.many(Ref, lst.len + 1);
            const listRefs = lst.refs();
            selfRefs[0].set(mem, val);
            // todo: be sure it's optimized in a memcpy
            for (selfRefs[1..], 0..) |*ref, j| ref.* = listRefs[j];
            self.* = .{ .tag = .LVec, .len = lst.len + 1 };
            return self;
        }

        pub fn pushRight(
            lst: Ptr,
            mem: *Mem,
            val: ChildPtr,
        ) !Ptr {
            const self = try mem.one(Self);
            const selfRefs = try mem.many(Ref, lst.len + 1);
            const listRefs = lst.refs();
            selfRefs[lst.len].set(mem, val);
            // todo: be sure it's optimized in a memcpy
            for (selfRefs[0..lst.len], 0..) |*ref, j| ref.* = listRefs[j];
            self.* = .{ .tag = .LVec, .len = lst.len + 1 };
            return self;
        }

        pub fn popLeft(
            lst: Ptr,
            mem: *Mem,
        ) !?Ptr {
            //todo: is it ok returning null or an error is better?
            if (lst.len == 0) return null;
            const self = try mem.one(Self);
            const selfLen = lst.len - 1;
            const selfRefs = try mem.many(Ref, selfLen);
            const listRefs = lst.refs();
            for (selfRefs, 0..) |*ref, j| ref.* = listRefs[j + 1];
            self.* = .{ .tag = .LVec, .len = selfLen };
            return self;
        }
        pub fn popLeftU(
            lst: Ptr,
            mem: *Mem,
        ) !Ptr {
            const self = try mem.one(Self);
            const selfLen = lst.len - 1;
            const selfRefs = try mem.many(Ref, selfLen);
            const listRefs = lst.refs();
            for (selfRefs, 1..) |*ref, j| ref.* = listRefs[j];
            self.* = .{ .tag = .LVec, .len = selfLen };
            return self;
        }
        pub fn popRight(
            lst: Ptr,
            mem: *Mem,
        ) !?Ptr {
            if (lst.len == 0) return null;
            const self = try mem.one(Self);
            const selfLen = lst.len - 1;
            const selfRefs = try mem.many(Ref, selfLen);
            const listRefs = lst.refs();
            for (selfRefs, 0..) |*ref, j| ref.* = listRefs[j];
            self.* = .{ .tag = .LVec, .len = selfLen };
            return self;
        }

        pub fn destructLeftU(
            self: Ptr,
            mem: *Mem,
        ) !struct { head: ChildPtr, tail: Ptr } {
            const head = self.refs()[0].get(mem);
            const tail = try mem.one(Self);
            const tailLen = self.len - 1;
            const tailRefs = try mem.many(Ref, tailLen);
            const selfRefs = self.refs();
            for (tailRefs, 0..) |*ref, j| ref.* = selfRefs[j + 1];
            tail.* = .{ .tag = .LVec, .len = tailLen };
            return .{ .head = head, .tail = tail };
        }

        pub fn destructLeft(
            self: Ptr,
            mem: *Mem,
        ) !?struct {
            left: ChildPtr,
            right: Ptr,
        } {
            if (self.len == 0) return null;
            const left_ = self.refs()[0].get(mem);
            const right_ = try mem.one(Self);
            const right_len = self.len - 1;
            const right_refs = try mem.many(Ref, right_len);
            const self_refs = self.refs();
            for (right_refs, 0..) |*ref, j| ref.* = self_refs[j + 1];
            right_.tag = .LVec;
            right_.len = right_len;
            return .{
                .left = left_,
                .right = right_,
            };
        }

        pub fn destructRight(
            self: Ptr,
            mem: *Mem,
        ) !?struct {
            left: Ptr,
            right: ChildPtr,
        } {
            if (self.len == 0) return null;
            const left_ = try mem.one(Self);
            const left_len = self.len - 1;
            const left_refs = try mem.many(Ref, left_len);
            const self_refs = self.refs();
            for (left_refs, 0..) |*ref, j| ref.* = self_refs[j];
            left_.tag = .LVec;
            left_.len = left_len;
            const right_ = self.refs()[self.len - 1].get(mem);
            return .{
                .left = left_,
                .right = right_,
            };
        }

        pub fn concatA(
            left_l: Ptr,
            mem: *Mem,
            right_a: []ChildPtr,
        ) !Ptr {
            const right_len = right_a.len;
            if (right_len == 0) return left_l;
            const left_len = left_l.len;
            const self_len = left_len + right_len;
            const left_refs = left_l.refs();
            const self = try Self.create(mem, self_len);
            const self_refs = self.refs();
            for (self_refs[0..left_len], 0..) |*ref, j| ref.* = left_refs[j];
            for (self_refs[left_len..self_len], 0..) |*ref, j| ref.set(mem, right_a[j]);
            return self;
        }

        pub fn concat(
            left_l: Ptr,
            mem: *Mem,
            right_l: Ptr,
        ) !Ptr {
            const right_len = right_l.len;
            const left_len = left_l.len;
            if (right_len == 0) return left_l;
            if (left_len == 0) return right_l;

            const self_len = left_len + right_len;
            const left_refs = left_l.refs();
            const right_refs = right_l.refs();
            const self = try Self.create(mem, self_len);
            const self_refs = self.refs();
            for (self_refs[0..left_len], 0..) |*ref, j| ref.* = left_refs[j];
            for (self_refs[left_len..self_len], 0..) |*ref, j| ref.* = right_refs[j];
            return self;
        }

        pub fn sub(
            self: Ptr,
            mem: *Mem,
            start: anytype,
            end: anytype,
        ) !Ptr {
            if (end < start or start < 0 or end > self.len) return error.ArgumentError;
            const new_len = end - start;
            const new = try Self.create(mem, new_len);
            const self_refs = self.refs();
            const new_refs = new.refs();
            for (new_refs, 0..) |*ref, j| ref.* = self_refs[j + start];
            return new;
        }

        pub inline fn isEqual(
            left_list: Ptr,
            right_list: Ptr,
            mem: *Mem,
        ) bool {
            if (left_list.len != right_list.len) return false;
            const left_refs = left_list.refs();
            const right_refs = right_list.refs();

            for (left_refs, 0..) |*r, j| {
                const left_term = r.get(mem);
                const right_term = right_refs[j].get(mem);
                //todo: isEqual should not allocate i.e. no errors
                const eq = left_term.isEqual(right_term, mem);
                if (!eq) return false;
            }
            return true;
        }

        pub fn move(
            self: Ptr,
            ctx: anytype,
        ) !Ptr {
            const next = try Self.create(ctx.dest, self.len);
            const next_refs = next.refs();
            const self_refs = self.refs();
            for (self_refs, next_refs) |s, *n| n.* = s;
            return next;
        }

        const e = error{
            OutOfMemory,
            MemoryOverflow,
        };

        pub fn copy(
            self: Ptr,
            ctx: anytype,
        ) e!Word {
            for (self.refs()) |*ref| try ref.copy(ctx);
            return @sizeOf(Self) + @sizeOf(Ref) * self.len;
        }
    };
}

// pub const VirtualList = packed struct {
//     tag: Tag,
//     from: FromLen,
//     to: ToLen,
//     vlist: Ref(VectorList),

//     const FromLen = @Type(.{
//         .Int = .{
//             .signedness = .unsigned,
//             .bits = @bitSizeOf(Offset) >> 1,
//         },
//     });
//     const ToLen = @Type(.{
//         .Int = .{
//             .signedness = .unsigned,
//             .bits = @bitSizeOf(Word) - @bitSizeOf(Tag) - @bitSizeOf(FromLen),
//         },
//     });

//     const Self = @This();

//     pub fn new(
//         mem: *Mem,
//         lst: *VectorList,
//         from: anytype,
//         to: anytype,
//     ) !Ptr {
//         const self = try mem.one(Self);
//         self.tag = .VVl;
//         self.from = from;
//         self.to = to;
//         try self.vlist.set(mem, lst);
//         return self;
//     }
// };

// pub const List = packed union {
//     vector: VectorList,
//     reflist: VirtualList,
// }

const Elem = packed struct {
    tag: types.Tag,
    val: types.UInt,

    pub const smallness = .always;

    const Self = @This();
    pub const Ptr = *align(@sizeOf(Word)) Self;

    pub fn from(
        mem: *Mem,
        v: anytype,
    ) !Ptr {
        const self = try mem.one(Self);
        self.* = .{
            .tag = .Fix,
            .val = v,
        };
        return self;
    }

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        const res = try ctx.dest.one(Self);
        res.tag = self.tag;
        res.val = self.val;
        return res;
    }

    pub inline fn copy(
        _: Ptr,
        _: anytype,
    ) !Word {
        return @sizeOf(Self);
    }
};

const Val = packed union {
    tagged: packed struct(Word) {
        tag: Tag,
        _: types.UInt,
    },
    elem: Elem,
    list: VectorListOf(Val),

    pub const smallness = .sometimes;

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub fn isSmall(self: Ptr) bool {
        switch (self.tagged.tag) {
            .LVec => {
                return self.list.isSmall();
            },
            else => {
                switch (Elem.smallness) {
                    .never => {
                        return false;
                    },
                    .always => {
                        return true;
                    },
                    else => {
                        return self.elem.isSmall();
                    },
                }
            },
        }
    }
};

const VectorList = VectorListOf(Elem);

test "create empty list" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const m0 = mem.heap;
    const lst = try VectorList.from(&mem, .{});
    try testing.expectEqual(lst.tag, .LVec);
    try testing.expectEqual(lst.len, 0);
    try testing.expectEqual(mem.heap, m0 + 1 * @sizeOf(Word));
}

test "create non empty list" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const elem = try Elem.from(&mem, 37);
    const elems = [_]Elem.Ptr{elem};
    const m0 = mem.heap;
    const lst = try VectorList.from(&mem, elems);
    try testing.expectEqual(lst.tag, .LVec);
    try testing.expectEqual(lst.len, 1);
    try testing.expectEqual(mem.heap, m0 + 2 * @sizeOf(Word));
    const elem1 = lst.get(&mem, 0);
    try testing.expectEqual(elem1.val, 37);
}

test "list left and right of an empty list is null" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const lst = try VectorList.from(&mem, .{});
    const l = lst.left(&mem);
    const r = lst.right(&mem);
    try testing.expectEqual(l, null);
    try testing.expectEqual(r, null);
}

test "list left and right of a list" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const elems = [_]Elem.Ptr{ n0, n1 };
    const lst = try VectorList.from(&mem, elems);
    const l = lst.left(&mem);
    const r = lst.right(&mem);
    try testing.expectEqual(l.?.val, n0.val);
    try testing.expectEqual(r.?.val, n1.val);
}

test "list push left" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const elem = try Elem.from(&mem, 10);
    const elems = [_]Elem.Ptr{elem};
    const lst = try VectorList.from(&mem, elems);
    const elem1 = try Elem.from(&mem, 20);
    const lst1 = try lst.pushLeft(&mem, elem1);
    try testing.expectEqual(lst1.tag, .LVec);
    try testing.expectEqual(lst1.len, 2);
    const v0 = lst1.get(&mem, 0);
    try testing.expectEqual(v0.val, 20);
}

test "list push right" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const elem = try Elem.from(&mem, 10);
    const elems = [_]Elem.Ptr{elem};
    const lst = try VectorList.from(&mem, elems);
    const elem1 = try Elem.from(&mem, 20);
    const lst1 = try lst.pushRight(&mem, elem1);
    try testing.expectEqual(lst1.tag, .LVec);
    try testing.expectEqual(lst1.len, 2);
    const v0 = lst1.get(&mem, 1);
    try testing.expectEqual(v0.val, 20);
}

test "list pop of empty is null" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const lst = try VectorList.from(&mem, .{});
    const l1 = try lst.popLeft(&mem);
    try testing.expectEqual(l1, null);
    const r1 = try lst.popRight(&mem);
    try testing.expectEqual(r1, null);
}

test "list pop  on list" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const elems = [_]Elem.Ptr{ n0, n1 };
    const lst = try VectorList.from(&mem, elems);
    const l = try lst.popLeft(&mem);
    const r = try lst.popRight(&mem);
    const xl = l.?.get(&mem, 0);
    const xr = r.?.get(&mem, 0);
    try testing.expectEqual(xl.val, n1.val);
    try testing.expectEqual(xr.val, n0.val);
}

test "list set" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const elem = try Elem.from(&mem, 10);
    const elems = [_]Elem.Ptr{ elem, elem, elem, elem };
    const lst = try VectorList.from(&mem, elems);
    const elem1 = try Elem.from(&mem, 20);
    try lst.set(&mem, 1, elem1);
    const v0 = lst.get(&mem, 0);
    const v1 = lst.get(&mem, 1);
    const v2 = lst.get(&mem, 2);
    try testing.expectEqual(elem.val, v0.val);
    try testing.expectEqual(elem1.val, v1.val);
    try testing.expectEqual(elem.val, v2.val);
}

test "list destructLeft empty" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const lst = try VectorList.from(&mem, .{});
    const dst = try lst.destructLeft(&mem);
    try testing.expectEqual(dst, null);
}

test "list destructLeft nonempty" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const elems = [_]Elem.Ptr{ n0, n1 };
    const lst = try VectorList.from(&mem, elems);
    const dst = try lst.destructLeft(&mem);
    try testing.expectEqual(dst.?.left.val, n0.val);
    try testing.expectEqual(dst.?.right.tag, .LVec);
    try testing.expectEqual(dst.?.right.len, 1);
}

test "concat array" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const ns0 = [_]Elem.Ptr{ n0, n1 };
    var ns1 = [_]Elem.Ptr{ n1, n0, n0 };

    const lst0 = try VectorList.from(&mem, ns0);
    const lst = try lst0.concatA(&mem, ns1[0..3]);
    try testing.expectEqual(lst.tag, .LVec);
    try testing.expectEqual(lst.len, 5);

    try testing.expectEqual(lst.get(&mem, 0).val, n0.val);
    try testing.expectEqual(lst.get(&mem, 1).val, n1.val);
    try testing.expectEqual(lst.get(&mem, 2).val, n1.val);
    try testing.expectEqual(lst.get(&mem, 3).val, n0.val);
    try testing.expectEqual(lst.get(&mem, 4).val, n0.val);
}

test "concat" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const ns0 = [_]Elem.Ptr{ n0, n1 };
    const ns1 = [_]Elem.Ptr{ n1, n0, n0 };

    const lst0 = try VectorList.from(&mem, ns0);
    const lst1 = try VectorList.from(&mem, ns1);
    const lst = try lst0.concat(&mem, lst1);
    try testing.expectEqual(lst.tag, .LVec);
    try testing.expectEqual(lst.len, 5);

    try testing.expectEqual(lst.get(&mem, 0).val, n0.val);
    try testing.expectEqual(lst.get(&mem, 1).val, n1.val);
    try testing.expectEqual(lst.get(&mem, 2).val, n1.val);
    try testing.expectEqual(lst.get(&mem, 3).val, n0.val);
    try testing.expectEqual(lst.get(&mem, 4).val, n0.val);
}

test "sub" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const n0 = try Elem.from(&mem, 21);
    const n1 = try Elem.from(&mem, 37);
    const ns0 = [_]Elem.Ptr{ n0, n1, n1, n0, n0 };

    const lst0 = try VectorList.from(&mem, ns0);
    const lst = try lst0.sub(&mem, 1, 3);
    try testing.expectEqual(lst.tag, .LVec);
    try testing.expectEqual(lst.len, 2);

    try testing.expectEqual(lst.get(&mem, 0).val, n1.val);
    try testing.expectEqual(lst.get(&mem, 1).val, n1.val);
}

test "copy list" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const n0 = try Elem.from(&orig, 21);
    const n1 = try Elem.from(&orig, 37);
    const ns0 = [_]Elem.Ptr{ n0, n1 };
    const cur = try VectorList.from(&orig, ns0);
    const nxt = try ctx.one(cur);

    try testing.expectEqual(nxt.get(&dest, 0).val, n0.val);
    try testing.expectEqual(nxt.get(&dest, 1).val, n1.val);
}
