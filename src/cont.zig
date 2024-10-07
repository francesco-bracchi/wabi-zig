const std = @import("std");
const types = @import("types.zig");
const Tag = types.Tag;
const Word = types.Word;
const Mem = @import("mem.zig").Mem;
const word_align = @alignOf(Word);
const RefTo = @import("ref.zig").RefTo;
const VectorList = @import("list.zig").VectorList;
const Env = @import("env.zig").Env;
const Sym = @import("sym.zig").Sym;
const Copy = @import("copy.zig").Copy;

pub fn Header(C: type) type {
    return packed struct {
        tag: Tag,
        prev_offset: Mem.Offset,

        const Self = @This();
        pub const Ptr = *align(word_align) Self;

        const CPtr = *align(word_align) C;

        pub fn init(
            self: Ptr,
            mem: *Mem,
            tag: Tag,
            prv: ?CPtr,
        ) void {
            self.tag = tag;
            if (prv) |p| {
                self.prev_offset = mem.offsetOf(p);
            } else {
                self.prev_offset = mem.offsetOf(self);
            }
        }

        pub fn prev(
            self: Ptr,
            mem: *Mem,
        ) ?CPtr {
            const cand = mem.fromOffset(CPtr, self.prev_offset);
            // todo: check this in terms of types
            return if (cand == self) null else cand;
        }

        pub fn copy(
            self: Ptr,
            ctx: anytype,
        ) !void {
            // warning this one is fine only if the continuation is not "small"
            const orig_prev = ctx.orig.fromOffset(CPtr, self.prev_offset);
            const dest_prev = try ctx.move(orig_prev);
            self.prev_offset = ctx.dest.offsetOf(dest_prev);
        }
    };
}

fn Eval(C: type) type {
    return packed struct {
        header: Header(C),

        const Self = @This();

        pub const Ptr = *align(word_align) Self;

        const CPtr = *align(word_align) C;

        // todo: it can be .always but can be hard because
        // we must move the previous in `move`
        // probably restructure as loop (the move of a contiinuation)?
        pub const smallness = .always;

        pub fn push(
            prv: ?CPtr,
            mem: *Mem,
            _: anytype,
        ) !Ptr {
            const self = try mem.one(Self);
            self.header.init(mem, .KEval, prv);
            return self;
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
            const p0 = ctx.orig.fromOffset(Ptr, self.header.prev_offset);
            if (p0.header.prev_offset != self.header.prev_offset) {
                try self.header.copy(ctx);
            } else {
                self.header.prev_offset = ctx.dest.offsetOf(self);
            }
            return @sizeOf(Self);
        }
    };
}

fn Apply(C: type, V: type) type {
    return packed struct {
        header: Header(C),
        args: RefTo(VectorList(V), .{ .nullable = false }),
        env: RefTo(Env(V), .{ .nullable = false }),

        const Self = @This();

        pub const Ptr = *align(word_align) Self;
        pub const smallness = .never;

        pub fn push(
            prv: anytype,
            mem: *Mem,
            opts: anytype,
        ) !Ptr {
            const self = try mem.one(Self);
            self.header.init(mem, .KApply, prv);
            self.args.set(mem, opts.args);
            self.env.set(mem, opts.env);
            return self;
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
            try self.header.copy(ctx);
            try self.args.copy(ctx);
            try self.env.copy(ctx);
            return @sizeOf(Self);
        }
    };
}
//todo: check if env is really required
pub fn Call(C: type, V: type) type {
    return packed struct {
        header: Header(C),
        combiner: RefTo(V, .{ .nullable = false }),
        env: RefTo(Env(V), .{ .nullable = false }),

        const Self = @This();

        pub const Ptr = *align(word_align) Self;
        pub const smallness = .never;

        pub fn push(
            prv: anytype,
            mem: *Mem,
            opts: anytype,
        ) !Ptr {
            const self = try mem.one(Self);
            self.header.init(mem, .KCall, prv);
            self.combiner.set(mem, opts.combiner);
            self.env.set(mem, opts.env);
            return self;
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
            try self.header.copy(ctx);
            try self.combiner.copy(ctx);
            try self.env.copy(ctx);
            return @sizeOf(Self);
        }
    };
}

pub fn Cont(V: type) type {
    return packed union {
        header: CHeader,
        eval: CEval,
        apply: CApply,
        call: CCall,

        const Self = @This();
        pub const Ptr = *align(word_align) Self;

        const CHeader = Header(Self);
        const CEval = Eval(Self);
        const CApply = Apply(Self, V);
        const CCall = Call(Self, V);

        pub const smallness = .sometimes;

        pub fn isSmall(self: Ptr) bool {
            return self.header.tag == .KEval;
        }

        pub fn eval(
            self: ?Ptr,
            mem: *Mem,
            args: anytype,
        ) !Ptr {
            return @as(Ptr, @ptrCast(try CEval.push(self, mem, args)));
        }

        pub fn apply(
            self: ?Ptr,
            mem: *Mem,
            args: anytype,
        ) !Ptr {
            return @as(Ptr, @ptrCast(try CApply.push(self, mem, args)));
        }

        pub fn call(
            self: ?Ptr,
            mem: *Mem,
            args: anytype,
        ) !Ptr {
            return @as(Ptr, @ptrCast(try CCall.push(self, mem, args)));
        }

        pub inline fn move(
            self: Ptr,
            ctx: anytype,
        ) !Ptr {
            switch (self.header.tag) {
                .KEval => {
                    return @ptrCast(try self.eval.move(ctx));
                },
                .KApply => {
                    return @ptrCast(try self.apply.move(ctx));
                },
                else => unreachable,
            }
        }

        pub fn copy(
            self: Ptr,
            ctx: anytype,
        ) !Word {
            return switch (self.header.tag) {
                .KEval => {
                    return try self.eval.copy(ctx);
                },
                .KApply => {
                    return try self.apply.copy(ctx);
                },
                else => |t| {
                    std.debug.print("tag: {}\n", .{t});
                    return error.Burf;
                },
            };
        }
    };
}

const Elem = packed struct {
    tag: Tag,
    val: types.UInt,

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub const smallness = .always;

    pub fn from(mem: *Mem, val: anytype) !Ptr {
        const self = try mem.one(Self);
        self.tag = .Fix;
        self.val = val;
        return self;
    }

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

const Val = packed union {
    tagged: packed struct(Word) {
        tag: Tag,
        _: types.UInt,
    },
    elem: Elem,
    cont: Cont(Self),

    pub const smallness = .sometimes;

    const Self = @This();

    pub const Ptr = *align(word_align) Self;

    pub fn eval(self: ?Ptr)

    pub fn isSmall(self: Ptr) bool {
        switch (self.tagged.tag) {
            .KEval, .KApply, .KCall => {
                return self.cont.isSmall();
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

    pub inline fn move(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        switch (self.tagged.tag) {
            .KEval, .KApply, .KCall => {
                return @ptrCast(try self.cont.move(ctx));
            },
            else => {
                return @ptrCast(try self.elem.move(ctx));
            },
        }
    }

    pub inline fn copy(
        self: Ptr,
        ctx: anytype,
    ) !Ptr {
        switch (self.tagged.tag) {
            .KEval, .KApply, .KCall => {
                return @ptrCast(try self.cont.copy(ctx));
            },
            else => {
                return @ptrCast(try self.elem.copy(ctx));
            },
        }
    }
};

test "copy eval" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur = try Cont(Val).eval(null, &orig, .{});
    const nxt = try ctx.one(cur);

    try std.testing.expect(nxt.header.tag == .KEval);
}

// test "copy apply" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const args = try VectorList(Val).from(&orig, .{});
//     const env = try Env(Val).root(&orig, 2);
//     const cur: Val.Ptr = try Cont(Val).apply(null, &orig, .{
//         .args = args,
//         .env = env,
//     });
//     // const nxt = try ctx.one(cur);

//     // try std.testing.expectEqual(nxt.cont.header.tag, .KApply);
//     try std.testing.expect(cur.cont == 0);
// }

// test "copy call" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const combiner = try Elem.from(&orig, 123);
//     const env = try Env(Elem).root(&orig, 3);
//     const cur = try Call(Elem).push(null, &orig, .{
//         .combiner = combiner,
//         .env = env,
//     });
//     var nxt = try ctx.one(cur);

//     try std.testing.expect(nxt.header.tag == .KCall);

//     try std.testing.expectEqual(combiner.get(&dest), combiner);
//     try std.testing.expect(nxt.env.get(&dest).tag == .Env);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);
// }

// pub const Args = packed struct {
//     header: Header,
//     args: RefTo(VectorList, .{ .nullable = false }),
//     part: RefTo(VectorList, .{ .nullable = false }),
//     env: RefTo(Env, .{ .nullable = false }),

//     const Self = @This();

//     pub const Ptr = *align(word_align) Self;
//     pub const smallness = .never;

//     pub fn push(
//         prv: anytype,
//         mem: *Mem,
//         opts: anytype,
//     ) !Ptr {
//         const self = try mem.one(Self);
//         self.header.init(mem, .KArgs, prv);
//         self.args.set(mem, opts.args);
//         self.part.set(mem, opts.part);
//         self.env.set(mem, opts.env);
//         return self;
//     }

//     pub fn move(
//         self: Ptr,
//         ctx: anytype,
//     ) !Ptr {
//         return try ctx.memMove(self);
//     }

//     pub fn copy(
//         self: Ptr,
//         ctx: anytype,
//     ) !Word {
//         try self.header.copy(ctx);
//         try self.args.copy(ctx);
//         try self.part.copy(ctx);
//         try self.env.copy(ctx);
//         return @sizeOf(Self);
//     }
// };

// test "copy args" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const args = try Val.fromString(&orig, "(1 2)");
//     const emp = try Val.fromString(&orig, "()");
//     const env = try wabi.env.Env.root(&orig, 3);
//     const cur = try Args.push(null, &orig, .{
//         .args = @as(VectorList.Ptr, @ptrCast(args)),
//         .part = @as(VectorList.Ptr, @ptrCast(emp)),
//         .env = env,
//     });
//     var nxt = try ctx.one(cur);

//     try std.testing.expect(nxt.header.tag == .KArgs);

//     var data: [1024]u8 = undefined;
//     try Val.cast(nxt.args.get(&dest)).toString(&dest, &data);
//     try std.testing.expect(std.mem.eql(u8, data[0..5], "(1 2)"));

//     try std.testing.expect(nxt.part.get(&dest).len == 0);

//     try std.testing.expect(nxt.env.get(&dest).tag == .Env);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);
// }

// pub const Def = packed struct {
//     header: Header,
//     sym: RefTo(Sym, .{ .nullable = false }),
//     env: RefTo(Env, .{ .nullable = false }),

//     const Self = @This();

//     pub const Ptr = *align(word_align) Self;
//     pub const smallness = .never;

//     pub fn push(
//         prv: anytype,
//         mem: *Mem,
//         opts: anytype,
//     ) !Ptr {
//         const self = try mem.one(Self);
//         self.header.init(mem, .KDef, prv);
//         self.sym.set(mem, opts.sym);
//         self.env.set(mem, opts.env);
//         return self;
//     }

//     pub fn move(
//         self: Ptr,
//         ctx: anytype,
//     ) !Ptr {
//         return try ctx.memMove(self);
//     }

//     pub fn copy(
//         self: Ptr,
//         ctx: anytype,
//     ) !Word {
//         try self.header.copy(ctx);
//         try self.sym.copy(ctx);
//         try self.env.copy(ctx);
//         return @sizeOf(Self);
//     }
// };

// test "copy def" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const bin = try orig.intern(wabi.bin.Block, @constCast("sym-def"));
//     const sym = try Sym.from(&orig, bin);
//     const env = try wabi.env.Env.root(&orig, 3);
//     const cur = try Def.push(null, &orig, .{
//         .sym = sym,
//         .env = env,
//     });
//     var nxt = try ctx.one(cur);

//     try std.testing.expect(nxt.header.tag == .KDef);

//     try std.testing.expect(std.mem.eql(u8, nxt.sym.get(&dest).binOf(&dest).data(), "sym-def"));
//     try std.testing.expect(nxt.env.get(&dest).tag == .Env);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);
// }

// pub const Select = packed struct {
//     header: Header,
//     env: RefTo(Env, .{ .nullable = false }),
//     then_branch: RefTo(Val, .{ .nullable = false }),
//     else_branch: RefTo(Val, .{ .nullable = false }),

//     const Self = @This();
//     pub const Ptr = *align(word_align) Self;
//     pub const smallness = .never;

//     pub fn push(
//         prv: anytype,
//         mem: *Mem,
//         opts: anytype,
//     ) !Ptr {
//         const self = try mem.one(Self);
//         self.header.init(mem, .KSel, prv);
//         self.env.set(mem, opts.env);
//         self.then_branch.set(mem, opts.then_branch);
//         self.else_branch.set(mem, opts.else_branch);
//         return self;
//     }

//     pub fn move(
//         self: Ptr,
//         ctx: anytype,
//     ) !Ptr {
//         return try ctx.memMove(self);
//     }

//     pub fn copy(
//         self: Ptr,
//         ctx: anytype,
//     ) !Word {
//         try self.header.copy(ctx);
//         try self.env.copy(ctx);
//         try self.then_branch.copy(ctx);
//         try self.else_branch.copy(ctx);
//         return @sizeOf(Self);
//     }
// };

// test "copy select" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const env = try wabi.env.Env.root(&orig, 3);
//     const then_branch = try Val.fromString(&orig, "1");
//     const else_branch = try Val.fromString(&orig, "2");
//     const cur = try Select.push(null, &orig, .{
//         .env = env,
//         .then_branch = then_branch,
//         .else_branch = else_branch,
//     });
//     var nxt = try ctx.one(cur);

//     try std.testing.expect(nxt.header.tag == .KSel);

//     try std.testing.expect(nxt.env.get(&dest).tag == .Env);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);

//     try std.testing.expect(nxt.then_branch.get(&dest).fix.val == 1);
//     try std.testing.expect(nxt.else_branch.get(&dest).fix.val == 2);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);
// }

// pub const Prog = packed struct {
//     header: Header,
//     env: RefTo(Env, .{ .nullable = false }),
//     exs: RefTo(VectorList, .{ .nullable = false }),
//     cur: Word,

//     const Self = @This();

//     pub const Ptr = *align(word_align) Self;
//     pub const smallness = .never;

//     pub fn push(
//         prv: anytype,
//         mem: *Mem,
//         opts: anytype,
//     ) !Ptr {
//         const self = try mem.one(Self);
//         self.header.init(mem, .KProg, prv);
//         self.env.set(mem, opts.env);
//         self.exs.set(mem, opts.exs);
//         self.cur = opts.cur;
//         return self;
//     }

//     pub fn move(
//         self: Ptr,
//         ctx: anytype,
//     ) !Ptr {
//         return try ctx.memMove(self);
//     }

//     pub fn copy(
//         self: Ptr,
//         ctx: anytype,
//     ) !Word {
//         try self.header.copy(ctx);
//         try self.env.copy(ctx);
//         try self.exs.copy(ctx);
//         return @sizeOf(Self);
//     }
// };

// test "copy prog" {
//     var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer orig.deinit();
//     var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
//     defer dest.deinit();
//     var ctx = try Copy.init(&orig, &dest, std.testing.allocator);
//     defer ctx.deinit();

//     const env = try wabi.env.Env.root(&orig, 3);
//     const exs = try Val.fromString(&orig, "(1 2 3)");
//     const cur = try Prog.push(null, &orig, .{
//         .env = env,
//         .exs = &exs.list,
//         .cur = 1,
//     });
//     var nxt = try ctx.one(cur);

//     try std.testing.expect(nxt.header.tag == .KProg);

//     try std.testing.expect(nxt.env.get(&dest).tag == .Env);
//     try std.testing.expect(nxt.env.get(&dest).uid == env.uid);

//     var data: [1024]u8 = undefined;
//     try Val.cast(nxt.exs.get(&dest)).toString(&dest, &data);
//     try std.testing.expect(std.mem.eql(u8, data[0..7], "(1 2 3)"));
//     try std.testing.expect(nxt.cur == 1);
// }

// pub const Cont = packed union {
//     header: Header,
//     eval: Eval,
//     apply: Apply,
//     args: Args,
//     call: Call,
//     def: Def,
//     sel: Select,
//     prog: Prog,

//     const Self = @This();
//     pub const Ptr = *align(word_align) Self;

//     pub const smallness = .sometimes;

//     pub fn isContTag(t: Tag) bool {
//         return switch (t) {
//             .KEval,
//             .KApply,
//             .KArgs,
//             .KCall,
//             .KDef,
//             .KSel,
//             .KProg,
//             => true,
//             else => false,
//         };
//     }
//     pub fn isSmall(self: Ptr) bool {
//         //todo: catch cases of tag not of cont?
//         return self.tag() == .KEval;
//     }

//     pub inline fn tag(self: Ptr) Tag {
//         std.debug.assert(isContTag(self.header.tag));
//         return self.header.tag;
//     }
//     pub fn push(
//         self: ?Ptr,
//         mem: *Mem,
//         comptime T: type,
//         args: anytype,
//     ) !Ptr {
//         const cur: Ptr = @ptrCast(try T.push(self, mem, args));
//         return cur;
//     }

//     pub fn pop(
//         self: Ptr,
//         mem: *Mem,
//     ) ?Ptr {
//         return if (self.header.prev(mem)) |c| @ptrCast(c) else null;
//     }
// };

// test "push eval" {
//     var mem = try Mem.init(std.testing.allocator, .{ .mem_size = 1024 });
//     defer mem.deinit();
//     const b = try Cont.push(null, &mem, Eval, .{});
//     try std.testing.expect(b.tag() == .KEval);
//     try std.testing.expect(b.pop(&mem) == null);
// }

// test "push eval twice" {
//     var mem = try Mem.init(std.testing.allocator, .{ .mem_size = 1024 });
//     defer mem.deinit();
//     const b = try Cont.push(null, &mem, Eval, .{});
//     const c = try b.push(&mem, Eval, .{});
//     try std.testing.expect(c.tag() == .KEval);
//     try std.testing.expect(c.pop(&mem).? == b);
// }
