// todo: support env merge? (i.e. multipliplication and sum)
// todo: support env multiple inheritance withnamespaces?
// todo: write copy/move copy tests

const std = @import("std");
const wabi = @import("wabi.zig");
const Word = wabi.types.Word;
const Mem = wabi.mem.Mem;
const Offset = Mem.Offset;
const Tag = wabi.types.Tag;
const Sym = wabi.sym.Sym;
const Fix = wabi.num.Fix;
const Block = wabi.bin.Block;
const Ref = wabi.ref.Ref;
const Val = wabi.val.Val;

const testing = std.testing;
const word_align = @alignOf(Word);

const shift_for_word = @ctz(@as(u64, @intCast(@sizeOf(Word))));

// todo: make this parametric on the type `Val`, so it can be tested indipendently
pub const Env = packed struct {
    tag: Tag,
    prv: Offset,
    uid: Word,
    len: Word,
    cpt: Word,
    dta: Word,

    const Self = @This();

    pub const smallness = .never;

    var count: Word = 0;

    pub const TableSize = @Type(.{
        .Int = .{
            .signedness = .unsigned,
            .bits = @popCount(@as(usize, @intCast(wabi.types.wordSize)) - 1),
        },
    });

    pub const Ptr = *align(word_align) Self;

    pub fn root(mem: *Mem, pow: anytype) !Ptr {
        return new(null, mem, pow);
    }

    pub fn extend(self: Ptr, mem: *Mem, pow: anytype) !Ptr {
        return new(self, mem, pow);
    }

    pub fn new(
        prev: ?Ptr,
        mem: *Mem,
        pow: TableSize,
    ) !Ptr {
        const cpt = @as(Word, @intCast(1)) << pow;
        const self = try mem.one(Self);
        const self_data = try mem.many(Word, 2 * cpt);
        const uid = count;
        const prv = if (prev) |p| mem.offsetOf(p) else mem.offsetOf(self);
        const data_offset = mem.offsetOf(&self_data[0]);

        for (self_data) |*w| w.* = 0;
        // todo: mixup to make it unique even cross Mem
        count += 1;
        self.* = .{
            .tag = .Env,
            .prv = prv,
            .uid = uid,
            .len = 0,
            .cpt = @intCast(cpt),
            .dta = @intCast(data_offset),
        };
        return self;
    }

    // todo: put it in a global (comptime?) name
    const num: Word = 107;
    const den: Word = 1_000;

    pub inline fn offsetHash(offset: anytype, mask: anytype) Word {
        return (offset >> shift_for_word) & mask;
    }
    pub fn def(
        self: Ptr,
        mem: *Mem,
        sym: Sym.Ptr,
        val: Val.Ptr,
    ) !void {
        // std.debug.print("cpt: {}, len: {}, grow?: {}\n", .{ self.cpt, self.len, (self.cpt - self.len - 1) * den <= num });
        // cpt = 2, len = 2;
        if ((self.cpt - self.len - 1) * den <= num) try self.grow(mem);
        try self.def_(mem, sym, val);
    }

    pub fn data(self: Ptr, mem: *Mem) struct { keys: []Sym, vals: []Ref } {
        const cpt = self.cpt;
        const nKeys = mem.fromOffset([*]align(word_align) Sym, @as(Offset, @intCast(self.dta)));
        const nVals = mem.fromOffset([*]align(word_align) Ref, @as(Offset, @intCast(self.dta)));
        return .{
            .keys = nKeys[0..cpt],
            .vals = nVals[cpt .. cpt * 2],
        };
    }

    fn def_(
        self: Ptr,
        mem: *Mem,
        key: Sym.Ptr,
        val: Val.Ptr,
    ) !void {
        const myData = self.data(mem);
        const keys = myData.keys;
        const vals = myData.vals;
        const mask = self.cpt - 1;
        var hash = offsetHash(key.bin_offset, mask);
        if (keys[hash].tag == .Sym) {
            // round robin insert
            var k0 = keys[hash];
            var v0 = vals[hash];
            keys[hash] = key.*;
            vals[hash].set(mem, val);

            while (keys[hash].tag == .Sym) : (hash = (hash + 1) & mask) {
                // todo: redefing is fine? is correct? why not force to set?
                if (keys[hash].bin_offset == k0.bin_offset) {
                    keys[hash] = k0;
                    vals[hash] = v0;
                    return;
                }
            }
            keys[hash] = k0;
            vals[hash] = v0;
            self.len += 1;
            return;
        }
        keys[hash] = key.*;
        vals[hash].set(mem, val);
        self.len += 1;
    }

    fn grow(
        self: Ptr,
        mem: *Mem,
    ) !void {
        const self_cpt = self.cpt;
        const self_data = self.data(mem);
        const new_cpt = self_cpt << 1;
        const new_data_ptr = try mem.many(Word, 2 * new_cpt);
        const new_keys = @as([*]Sym, @ptrCast(new_data_ptr))[0..new_cpt];
        const new_vals = @as([*]Ref, @ptrCast(new_data_ptr))[new_cpt .. new_cpt * 2];
        const new_mask = new_cpt - 1;
        const self_keys = self_data.keys;
        const self_vals = self_data.vals;

        for (new_data_ptr) |*w| w.* = 0;

        var j: Word = 0;
        while (j < self_cpt) : (j += 1) if (self_keys[j].tag == .Sym) {
            const key = self_keys[j];
            const val = self_vals[j];
            var hash = offsetHash(key.bin_offset, new_mask);
            while (new_keys[hash].tag == .Sym) hash = (hash + 1) & new_mask;
            new_keys[hash] = key;
            new_vals[hash].word = val.word;
        };
        self.cpt = new_cpt;
        self.dta = mem.offsetOf(new_data_ptr);
    }

    pub fn previous(
        self: Ptr,
        mem: *Mem,
    ) ?Ptr {
        const p = mem.fromOffset(Ptr, self.prv);
        return if (p == self) null else p;
    }

    fn lookupCur(
        self: Ptr,
        mem: *Mem,
        key: Sym.Ptr,
    ) ?Val.Ptr {
        const self_data = self.data(mem);
        const keys = self_data.keys;
        const vals = self_data.vals;
        const mask = self.cpt - 1;

        var j = offsetHash(key.bin_offset, mask);
        while (keys[j].tag == .Sym) : (j = (j + 1) & mask) {
            if (keys[j].bin_offset == key.bin_offset) return vals[j].get(mem);
        }
        return null;
    }

    //todo: lookup should return null if not found, not an error.
    pub fn lookup(
        self: Ptr,
        mem: *Mem,
        key: Sym.Ptr,
    ) !Val.Ptr {
        var env: ?Ptr = self;
        while (env) |e| : (env = e.previous(mem)) if (e.lookupCur(mem, key)) |val| return val;
        return error.UnboundSymbol;
    }

    pub fn move(
        cur: Ptr,
        ctx: anytype,
    ) !Ptr {
        const nxt = try ctx.dest.one(Self);
        // allocates data but do not actually copy:
        // it is allocated next to the env so that when copied
        // this data could be skipped safely, otherwise it must be
        // tagged, or "copy" is not able to skip it
        var pool = try ctx.dest.many(Word, 2 * cur.cpt);
        for (pool) |*w| w.* = 0;
        nxt.* = cur.*;
        return nxt;
    }

    pub fn copy(
        self: Ptr,
        ctx: anytype,
    ) @typeInfo(@TypeOf(ctx)).Pointer.child.Error!Word {
        const cpt = self.cpt;
        const mask = cpt - 1;
        const cur_data = self.data(ctx.orig);
        const cur_keys = cur_data.keys;
        const cur_vals = cur_data.vals;
        self.dta = ctx.dest.offsetOf(self) + @sizeOf(Self);
        const nxt_data = self.data(ctx.dest);
        const nxt_keys = nxt_data.keys;
        const nxt_vals = nxt_data.vals;
        for (cur_keys, cur_vals) |*key, *ref| if (key.tag == .Sym) {
            const cur_bin = key.binOf(ctx.orig);
            //todo: use nxt_bin as offset
            const nxt_bin = try ctx.dest.intern(Block, cur_bin.data());
            // const nxt_bin = try nxt_bin.move)
            // intern cur_bin?
            const nxt_bin_offset = ctx.dest.offsetOf(nxt_bin);
            var hash = offsetHash(nxt_bin_offset, mask);
            while (nxt_keys[hash].tag == .Sym) hash = (hash + 1) & mask;
            nxt_keys[hash] = .{
                .tag = .Sym,
                .bin_offset = nxt_bin_offset,
            };
            nxt_vals[hash] = ref.*;
            try nxt_vals[hash].copy(ctx);
        };

        const prev_orig = ctx.orig.fromOffset(Ptr, self.prv);
        const prev_dest = try ctx.move(Val.cast(prev_orig));
        self.prv = ctx.dest.offsetOf(prev_dest);

        return @sizeOf(Self) + 2 * @sizeOf(Word) * cpt;
    }
};

test "create env" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();

    const env = try Env.root(&mem, 3);
    try testing.expect(env.cpt == 8);
    try testing.expect(env.prv == mem.offsetOf(env));
}

test "actually extend env " {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 1_024 });
    defer mem.deinit();
    const env = try Env.root(&mem, 3);
    const env1 = try env.extend(&mem, 4);
    try testing.expect(env1.cpt == 16);
    try testing.expect(env1.prv == mem.offsetOf(env));
}

test "env def" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const sym = try Sym.fromString(&mem, "foo");
    const env = try Env.root(&mem, 3);
    const num = try Val.from(&mem, Fix, 3);
    try env.def(&mem, sym, num);
    try testing.expect(env.cpt == 8);
    try testing.expect(env.prv == mem.offsetOf(env));
}

test "env double def ()" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const sym = try Sym.fromString(&mem, "foo");
    const env = try Env.root(&mem, 3);
    const three = try Val.from(&mem, Fix, 3);
    const five = try Val.from(&mem, Fix, 5);
    try env.def(&mem, sym, three);
    try env.def(&mem, sym, five);
    try testing.expect(env.len == 1);
}

test "env grow & def" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const env = try Env.root(&mem, 1);
    const sym0 = try Sym.fromString(&mem, "foo");
    const sym1 = try Sym.fromString(&mem, "bar");
    const sym2 = try Sym.fromString(&mem, "baz");
    const num0 = try Val.from(&mem, Fix, 3);
    const num1 = try Val.from(&mem, Fix, 5);
    const num2 = try Val.from(&mem, Fix, 7);
    try testing.expect(env.cpt == 2);
    try env.def(&mem, sym0, num0);
    try env.def(&mem, sym1, num1);
    try env.def(&mem, sym2, num2);
    try testing.expect(env.cpt == 4);
}

test "lookup on empty" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const env = try Env.root(&mem, 3);
    const sym = try Sym.fromString(&mem, "foo");

    const res = env.lookup(&mem, sym);
    try testing.expectError(error.UnboundSymbol, res);
}

test "simple lookup" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const env = try Env.root(&mem, 3);
    const sym = try Sym.fromString(&mem, "foo");
    const num = try Val.from(&mem, Fix, 3);
    try env.def(&mem, sym, num);

    const num0 = try env.lookup(&mem, sym);
    try testing.expectEqual(num.word, num0.word);
}

test "lookup in deeper env" {
    var mem = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer mem.deinit();

    const env0 = try Env.root(&mem, 3);
    const env = try env0.extend(&mem, 4);
    const sym = try Sym.fromString(&mem, "foo");
    const num = try Val.from(&mem, Fix, 3);
    try env0.def(&mem, sym, num);
    const num0 = try env.lookup(&mem, sym);
    try testing.expectEqual(num.word, num0.word);
}

test "copy env" {
    var orig = try Mem.init(std.testing.allocator, .{ .mem_size = 4_096 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 4_096 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur = try Env.root(&orig, 3);
    const sym = try Sym.fromString(&orig, "foo");
    const num = try Val.from(&orig, Fix, 3);
    try cur.def(&orig, sym, num);

    const nxts = try ctx.many(.{ cur, sym });
    const nxt = nxts[0];
    const dest_sym = nxts[1];

    const dest_num = try nxt.lookup(&dest, dest_sym);
    try std.testing.expect(dest_num.word == num.word);
}

test "copy grown env" {
    var orig = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 1_024 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur = try Env.root(&orig, 1);
    const sym0 = try Sym.fromString(&orig, "foo");
    const sym1 = try Sym.fromString(&orig, "bar");
    const num = try Val.from(&orig, Fix, 3);
    const str = try Val.from(&orig, Block, "hello");
    try cur.def(&orig, sym0, num);
    try cur.def(&orig, sym1, str);

    const nxt = try ctx.one(cur);
    const data = nxt.data(&dest);

    var cnt: u32 = 0;
    for (data.keys) |*k| if (k.tag == .Sym) {
        cnt += 1;
    };
    try testing.expect(cnt == 2);

    for (data.vals) |*v| if (v.a_ref.tag == .Ref)
        try testing.expect(std.mem.eql(u8, v.get(&dest).block.data(), "hello"));
}

test "copy env previous" {
    var orig = try Mem.init(testing.allocator, .{ .mem_size = 4_096 });
    defer orig.deinit();
    var dest = try Mem.init(std.testing.allocator, .{ .mem_size = 4_096 });
    defer dest.deinit();
    var ctx = try wabi.copy.Copy.init(&orig, &dest, std.testing.allocator);
    defer ctx.deinit();

    const cur_root = try Env.root(&orig, 2);
    const cur = try Env.extend(cur_root, &orig, 3);

    const nxt = try ctx.one(cur);

    try testing.expect(nxt.prv != cur.prv);
}
