const std = @import("std");
const wabi = @import("wabi.zig");
const Tag = wabi.types.Tag;
const Word = wabi.types.Word;
const Mem = wabi.mem.Mem;
const Offset = Mem.Offset;
const Ref = wabi.ref.Ref;

const word_align = @alignOf(Word);

const Eval = packed struct {
    pub inline fn copy(
        _: anytype,
        _: anytype,
        _: anytype,
    ) !void {}
};

const Emp = packed struct {
    pub inline fn copy(
        _: anytype,
        _: anytype,
        _: anytype,
    ) !void {}
};

const Apply = packed struct {
    args: Ref,
    env: Ref,

    const Self = @This();
    pub fn copy(
        self: *Self,
        ctx: anytype,
        comptime IPtr: type,
    ) !void {
        try self.args.copy(ctx, IPtr);
        try self.env.copy(ctx, IPtr);
    }
};

fn Record(T: type) type {
    return packed struct {
        tag: Tag,
        tail_offset: Offset,
        head: T,

        const Self = @This();

        pub inline fn copy(
            self: *Self,
            ctx: anytype,
            comptime IPtr: type,
        ) !Word {
            try self.head.copy(ctx, IPtr);
            return @sizeOf(Self);
        }

        pub fn move(self: *Self, ctx: anytype) !*Self {
            const res = try ctx.dest.one(Self);
            res.* = self.*;
            return res;
        }
    };
}

fn fromType(comptime T: type) Tag {
    return switch (T) {
        Emp => .CEmp,
        Eval => .CEval,
        Apply => .CApply,
        else => @compileError("unknown tag for type"),
    };
}

fn fromTag(comptime t: Tag) type {
    return switch (t) {
        .CEmp => Emp,
        .CEval => Eval,
        .CApply => Apply,
        else => unreachable,
    };
}

// fn buildOptions(comptime t: Tag, I: ?type) type {
//     return struct {
//         const Self = @This();
//         const T = fromTag(t);
//         const R = Record(T);
//         pub fn new(
//             opts: Self,
//             mem: *Mem,
//             stack: anytype,
//         ) !*R {
//             const res = try mem.one(R);
//             res.*.tag = t;
//             res.*.tail_offset = mem.offsetOf(stack);
//             if (I) |i| i.init(res.head.*, mem, opts);
//             return res;
//         }
//     };
// }

pub fn Opts(types: anytype) type {
    return union(enum) {
        emp: struct {
            const Self = @This();
            const R = Record(Emp);
            pub fn new(
                _: Self,
                mem: *Mem,
                stack: anytype,
            ) !*R {
                const res = try mem.one(R);
                res.* = .{
                    .tag = .CEmp,
                    .tail_offset = mem.offsetOf(stack),
                    .head = .{},
                };
                return res;
            }
        },
        eval: struct {
            const Self = @This();
            const R = Record(Eval);
            pub fn new(
                _: Self,
                mem: *Mem,
                stack: anytype,
            ) !*R {
                const res = try mem.one(R);
                res.* = .{
                    .tag = .CEval,
                    .tail_offset = mem.offsetOf(stack),
                    .head = .{},
                };
                return res;
            }
        },
        apply: struct {
            args: types.Val,
            env: types.Env,
            const Self = @This();
            const R = Record(Apply);
            pub fn new(
                opts: Self,
                mem: *Mem,
                stack: anytype,
            ) !*R {
                const res = try mem.one(R);
                res.*.tag = .CApply;
                res.*.tail_offset = mem.offsetOf(stack);
                res.*.head.args.set(mem, opts.args);
                res.*.head.env.set(mem, opts.env);
                return res;
            }
        },
    };
}

pub const Stack = packed union {
    top: Record(void),
    emp: Record(Emp),
    eval: Record(Eval),
    apply: Record(Apply),

    const Self = @This();

    const EmpRecord = Record(Emp);

    pub const Ptr = *align(word_align) Self;

    pub fn emp(mem: *Mem) !Ptr {
        const res = try mem.one(EmpRecord);
        res.* = .{
            .tag = .CEmp,
            .tail_offset = 0,
            .head = .{},
        };
        return @ptrCast(res);
    }

    pub inline fn tag(self: Ptr) Tag {
        return self.top.tag;
    }

    pub inline fn isEmp(self: Ptr) bool {
        return self.tag() == .CEmp;
    }
    pub inline fn isFull(self: Ptr) bool {
        return self.tag() != .CEmp;
    }

    pub fn pop(self: Ptr, mem: *Mem) ?Ptr {
        if (self.isEmp()) {
            return null;
        } else {
            // todo: self.top.pop(mem);
            return mem.fromOffset(Ptr, self.top.tail_offset);
        }
    }

    pub fn push(
        self: Ptr,
        mem: *Mem,
        opts: anytype,
    ) !Ptr {
        switch (opts) {
            inline else => |type_opts| {
                const v = try type_opts.new(mem, self);
                return @ptrCast(@alignCast(v));
            },
        }
    }

    pub fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        switch (self.tag()) {
            inline .CEmp, .CEval, .CApply => |t| {
                const T = comptime Record(fromTag(t));
                const cur: *T = @ptrCast(self);
                const nxt = try ctx.dest.one(T);
                nxt.* = cur.*;
                return @ptrCast(@alignCast(nxt));
            },
            else => unreachable,
        }
    }

    pub fn copy(
        self: Ptr,
        ctx: anytype,
        comptime IPtr: type,
    ) !Word {
        switch (self.tag()) {
            .CEmp => {
                return try self.emp.copy(ctx, IPtr);
            },
            inline .CEval, .CApply => |t| {
                const T = comptime Record(fromTag(t));
                const record: *T = @ptrCast(self);
                const tail = try ctx.move(ctx.orig.fromOffset(Ptr, self.top.tail_offset));
                record.tail_offset = ctx.dest.offsetOf(tail);
                return try record.copy(ctx, IPtr);
            },
            else => unreachable,
        }
    }
};

const O = Opts(.{
    .Val = *wabi.num.Fix,
    .Env = *wabi.env.Env,
});

const Ctx = struct {
    orig: *Mem,
    dest: *Mem,
    scan: Word = 0,

    const Self = @This();
    pub fn move(self: Self, val: anytype) !@TypeOf(val) {
        return try val.move(self);
    }
};

test "empty and pop" {
    var mem = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const e = try Stack.emp(&mem);
    try std.testing.expect(e.isEmp());
    try std.testing.expect(!e.isFull());
    try std.testing.expect(e.pop(&mem) == null);
}

test "push eval and pop" {
    var mem = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const e = try Stack.emp(&mem);
    const x = try e.push(&mem, O{ .eval = .{} });

    try std.testing.expect(x.tag() == .CEval);
    try std.testing.expect(x.isFull());
    try std.testing.expect(!x.isEmp());
    try std.testing.expect(x.pop(&mem).?.isEmp());
}

test "push apply and pop" {
    var mem = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const env = try wabi.env.Env.root(&mem, 2);
    const args = try wabi.num.Fix.from(&mem, 7634);
    const e = try Stack.emp(&mem);
    const x = try e.push(&mem, O{
        .apply = .{
            .args = args,
            .env = env,
        },
    });

    try std.testing.expect(x.tag() == .CApply);
    try std.testing.expectEqualDeep(x.apply.head.args.get(&mem, *wabi.num.Fix), args);
    try std.testing.expectEqualDeep(x.apply.head.env.get(&mem, *wabi.env.Env), env);
    try std.testing.expect(x.isFull());
    try std.testing.expect(!x.isEmp());
    try std.testing.expect(x.pop(&mem).?.isEmp());
}

test "move & copy emp" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    const ctx: Ctx = .{ .orig = &orig, .dest = &dest };

    const cur = try Stack.emp(&orig);
    const nxt = try cur.move(ctx);

    try std.testing.expect(nxt.tag() == .CEmp);
    try std.testing.expectEqual(try nxt.copy(ctx, *wabi.num.Fix), @sizeOf(Record(Emp)));
}

test "move and copy apply" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    const ctx: Ctx = .{ .orig = &orig, .dest = &dest };

    const env = try wabi.env.Env.root(&orig, 2);
    const args = try wabi.num.Fix.from(&orig, 7634);
    const e = try Stack.emp(&orig);
    const cur = try e.push(&orig, O{
        .apply = .{
            .args = args,
            .env = env,
        },
    });

    const nxt = try cur.move(ctx);

    try std.testing.expect(nxt.tag() == .CApply);
    try std.testing.expectEqualDeep(nxt.apply.head.args.get(&orig, *wabi.num.Fix), args);
    try std.testing.expectEqualDeep(nxt.apply.head.env.get(&orig, *wabi.env.Env), env);
    try std.testing.expect(nxt.isFull());
    try std.testing.expect(!nxt.isEmp());
    try std.testing.expect(nxt.pop(&orig).?.isEmp());
    try std.testing.expectEqual(try nxt.copy(ctx, wabi.num.Fix.Ptr), @sizeOf(Record(Apply)));
}
