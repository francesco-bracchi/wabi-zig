const std = @import("std");
const wabi = @import("wabi.zig");
const Cont = wabi.cont.Cont;
const RefTo = wabi.ref.RefTo;
const Val = wabi.val.Val;
const Mem = wabi.mem.Mem;
const Word = wabi.types.Word;
const Tag = wabi.types.Tag;
const word_align = @alignOf(Word);

//todo: it would be fantastic to have a hash_table for the latest marks, per tag
// at vm level as well (maintenance?)
pub const Meta = packed struct {
    tag: Tag,
    next_offset: Mem.Offset,
    cont: RefTo(Cont, .{ .nullable = true }),
    mark: RefTo(Val, .{ .nullable = true }),

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub const smallness = .never;

    fn setNext(
        self: Ptr,
        mem: *Mem,
        next: ?Ptr,
    ) void {
        if (next) |nxt| {
            self.next_offset = mem.offsetOf(nxt);
        } else {
            self.next_offset = mem.offsetOf(self);
        }
    }
    pub fn push(
        cur: ?Ptr,
        mem: *Mem,
        opts: anytype,
    ) !Ptr {
        const self = try mem.one(Self);
        self.tag = .KMeta;
        self.setNext(mem, cur);
        self.cont.set(mem, opts.cont);
        self.mark.set(mem, opts.mark);
        return self;
    }

    pub fn pop(self: Ptr, mem: *Mem) ?Ptr {
        return if (mem.offsetOf(self) == self.next_offset)
            null
        else
            mem.fromOffset(Ptr, self.next_offset);
    }

    pub fn revConj(
        rev0: ?Ptr,
        mem: *Mem,
        stack0: ?Ptr,
    ) !?Ptr {
        var rev = rev0;
        var stack = stack0;

        while (rev) |r| {
            const next = try mem.one(Self);
            next.cont = r.cont;
            next.mark = r.mark;
            next.setNext(mem, stack);
            stack = next;
            rev = r.pop(mem);
        }
        return stack;
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
        _ = .{ self, ctx };
        try self.cont.copy(ctx);
        try self.mark.copy(ctx);
        const next_orig = ctx.orig.fromOffset(Ptr, self.next_offset);
        const next_dest = try ctx.move(next_orig);
        self.next_offset = ctx.dest.offsetOf(next_dest);
        return @sizeOf(Self);
    }
};

test "copy meta" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur = try Meta.push(null, &orig, .{
        .cont = null,
        .mark = null,
    });
    var nxt = try ctx.one(cur);

    try std.testing.expect(nxt.cont.get(&dest) == null);
}
