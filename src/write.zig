const std = @import("std");
const wabi = @import("wabi.zig");
const Mem = wabi.mem.Mem;
const Val = wabi.val.Val;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const Fix = wabi.num.Fix;
const Env = wabi.env.Env;
const Block = wabi.bin.Block;
const VectorList = wabi.list.VectorList;
const Builtin = wabi.comb.Builtin;
const Derived = wabi.comb.Derived;
const Control = wabi.comb.Control;
const Meta = wabi.meta.Meta;
const Cont = wabi.cont.Cont;

const testing = std.testing;

pub fn ValueWriter(
    comptime Writer: type,
) type {
    return struct {
        writer: Writer,
        mem: *Mem,

        const Self = @This();
        const Error = Writer.Error;

        fn writeSymbol(
            self: *Self,
            sym: Sym.Ptr,
        ) !void {
            const bin = sym.binOf(self.mem);
            // todo: escape endin chars switch(c) { ' ' ... => write('/')}
            for (bin.data()) |c| try self.writer.writeByte(c);
        }

        fn writeKeyword(
            self: *Self,
            kwd: Kwd.Ptr,
        ) !void {
            const bin = kwd.binOf(self.mem);
            try self.writer.writeByte(':');
            // todo: escape endin chars switch(c) { ' ' ... => write('/')}
            for (bin.data()) |c| try self.writer.writeByte(c);
        }

        fn writeList(
            self: *Self,
            list: VectorList.Ptr,
        ) !void {
            try self.writer.writeByte('(');
            const len0 = list.len;
            for (list.refs(), 0..) |*r, j| {
                const elem = r.get(self.mem);
                try self.writeVal(elem);
                if (j + 1 < len0) try self.writer.writeByte(' ');
            }
            try self.writer.writeByte(')');
        }

        fn writeBlock(
            self: *Self,
            bin: Block.Ptr,
        ) !void {
            try self.writer.writeByte('"');
            for (bin.data()) |c| {
                if (c == '"') try self.writer.writeByte('\\');
                try self.writer.writeByte(c);
            }

            try self.writer.writeByte('"');
        }

        fn writeFix(
            self: *Self,
            fix: Fix.Ptr,
        ) !void {
            const val = fix.val;
            // doto implement our own code
            try std.fmt.format(self.writer, "{any}", .{val});
        }
        fn writeEnv(
            self: *Self,
            env: Env.Ptr,
        ) !void {
            try std.fmt.format(self.writer, "[env ", .{});
            for (env.data(self.mem).keys) |*k| if (k.tag == .Sym) {
                try self.writeSymbol(k);
                try self.writer.writeByte(' ');
            };
            try std.fmt.format(self.writer, "]", .{});
        }

        fn writeBOpr(
            self: *Self,
            opr: Builtin.Ptr,
        ) !void {
            try std.fmt.format(self.writer, "[o^ {}]", .{opr.uid});
        }

        fn writeBApp(
            self: *Self,
            app: Builtin.Ptr,
        ) !void {
            try std.fmt.format(self.writer, "[.\\ {}]", .{app.uid});
        }

        fn writeDOpr(
            self: *Self,
            opr: Derived.Ptr,
        ) !void {
            const patt = opr.pattern.get(self.mem);
            const body = opr.body.get(self.mem);
            const env_name = &opr.caller_env_name;
            try self.writer.writeAll("[fx ");
            try self.writeSymbol(env_name);
            try self.writer.writeAll(" ");
            try self.writeVal(patt);
            try self.writer.writeAll(" ");

            const len0 = body.len;
            for (body.refs(), 0..) |*r, j| {
                const elem = r.get(self.mem);
                try self.writeVal(elem);
                if (j + 1 < len0) try self.writer.writeByte(' ');
            }
            try self.writer.writeAll("]");
        }

        fn writeDApp(
            self: *Self,
            app: Derived.Ptr,
        ) !void {
            const patt = app.pattern.get(self.mem);
            const body = app.body.get(self.mem);
            try self.writer.writeAll("[fn ");
            try self.writeVal(patt);
            try self.writer.writeAll(" ");

            const len0 = body.len;
            for (body.refs(), 0..) |*r, j| {
                const elem = r.get(self.mem);
                try self.writeVal(elem);
                if (j + 1 < len0) try self.writer.writeByte(' ');
            }
            try self.writer.writeAll("]");
        }

        const Errors = error{
            AccessDenied,
            BrokenPipe,
            ConnectionResetByPeer,
            DiskQuota,
            FileTooBig,
            InputOutput,
            LockViolation,
            NoSpaceLeft,
            NotOpenForWriting,
            OperationAborted,
            SystemResources,
            Unexpected,
            WouldBlock,
        };

        fn writeControl(
            self: *Self,
            val: Control.Ptr,
        ) !void {
            const mem = self.mem;

            var cont = val.cont.get(mem);
            const rev_meta = val.rev_meta.get(mem);

            try self.writer.writeAll("[k ");
            while (cont) |k| {
                try self.writeControlElem(k);
                const cont1 = k.pop(mem);
                if (cont1) |_| try self.writer.writeByte(' ');
                cont = cont1;
            }
            var meta = Meta.revConj(rev_meta, mem, null) catch unreachable;
            while (meta) |m| {
                cont = m.cont.get(mem);
                while (cont) |k| {
                    try self.writeControlElem(k);
                    const cont1 = k.pop(mem);
                    if (cont1) |_| try self.writer.writeByte(' ');
                    cont = cont1;
                }
                const meta1 = m.pop(mem);
                if (meta1) |_| try self.writer.writeByte(' ');
                meta = meta1;
            }
            try self.writer.writeByte(']');
        }

        fn writeControlElem(
            self: *Self,
            cont: Cont.Ptr,
        ) !void {
            switch (cont.tag()) {
                .KEval => {
                    try self.writer.writeAll("{eval}");
                },
                .KApply => {
                    try self.writer.writeAll("{apply ");
                    try self.writeList(cont.apply.args.get(self.mem));
                    try self.writer.writeAll("}");
                },
                .KCall => {
                    try self.writer.writeAll("{call ");
                    try self.writeVal(Val.cast(cont.call.combiner.get(self.mem)));
                    try self.writer.writeAll("}");
                },
                .KSel => {
                    try self.writer.writeAll("{sel}");
                },
                .KArgs => {
                    try self.writer.writeAll("{args ");
                    try self.writer.print("{} ", .{cont.args.args.get(self.mem).len});
                    try self.writeList(cont.args.part.get(self.mem));
                    try self.writer.writeAll("}");
                },
                .KDef => {
                    try self.writer.writeAll("{def ");
                    try self.writeSymbol(cont.def.sym.get(self.mem));
                    try self.writer.writeAll("}");
                },
                .KProg => {
                    try self.writer.writeAll("{prog ");
                    try self.writer.print("{} ", .{cont.prog.cur});
                    try self.writeList(cont.prog.exs.get(self.mem));
                    try self.writer.writeAll("}");
                },
                else => {
                    unreachable;
                },
            }
        }

        fn writeNul(
            self: *Self,
            _: anytype,
        ) !void {
            try self.writer.writeAll("null");
        }

        fn writeRef(
            self: *Self,
            _: anytype,
        ) !void {
            try self.writer.writeAll("ref");
        }

        pub fn writeVal(
            self: *Self,
            val: Val.Ptr,
        ) Self.Error!void {
            // todo: convert to:
            // switch (val.tag()) {
            //     inline else => |t| {
            //         const T = wabi.type_of.typeOfTag(t);
            //         try T.write(@as(T.Ptr, @ptrCast(val)));
            //     },
            // }
            switch (val.tag()) {
                .Sym => try self.writeSymbol(&val.sym),
                .Kwd => try self.writeKeyword(&val.kwd),
                .LVec => try self.writeList(&val.list),
                .BBlk => try self.writeBlock(&val.block),
                .Fix => try self.writeFix(&val.fix),
                .Env => try self.writeEnv(&val.env),
                .BOpr => try self.writeBOpr(&val.builtin),
                .BApp => try self.writeBApp(&val.builtin),
                .DOpr => try self.writeDOpr(&val.derived),
                .DApp => try self.writeDApp(&val.derived),
                .Kont => try self.writeControl(&val.control),
                .Nul => try self.writeNul(&val),
                .Ref => try self.writeRef(&val),
                .Fwd => unreachable,
                else => |t| {
                    try std.fmt.format(self.writer, "[unk {}]", .{t});
                },
            }
        }

        pub fn init(writer: Writer, mem: *Mem) Self {
            return .{
                .writer = writer,
                .mem = mem,
            };
        }
    };
}

pub fn valueWriter(
    writer: anytype,
    mem: *Mem,
) ValueWriter(@TypeOf(writer)) {
    const VWriter = ValueWriter(@TypeOf(writer));
    return VWriter.init(writer, mem);
}

const config = .{
    .mem_size = 8_192,
};

test "write symbol" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "foo");
    const sym = try Val.from(&mem, Sym, bin);
    try write.writeVal(sym);
    try testing.expect(std.mem.eql(u8, buf[0..3], "foo"));
}

test "write keyword" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "foo");
    const sym = try Val.from(&mem, Kwd, bin);
    try write.writeVal(sym);
    try testing.expect(std.mem.eql(u8, buf[0..4], ":foo"));
}

test "write bin" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "foo");
    try write.writeVal(bin);
    try testing.expect(std.mem.eql(u8, buf[0..5], "\"foo\""));
}

test "write bin with escape" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "a\"");
    try write.writeVal(bin);
    try testing.expect(std.mem.eql(u8, buf[0..5], &[_]u8{ '"', 'a', '\\', '"', '"' }));
}

test "write empty list" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const lst = try Val.from(&mem, VectorList, .{});
    try write.writeVal(lst);
    try testing.expect(std.mem.eql(u8, buf[0..2], "()"));
}

test "write 1 elem list" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [512]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "foo");
    const lst = try Val.from(&mem, VectorList, [_]Val.Ptr{bin});
    try write.writeVal(lst);
    try testing.expect(std.mem.eql(u8, buf[0..7], "(\"foo\")"));
}

test "write 2 elem list" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const bin = try Val.from(&mem, Block, "foo");
    const lst = try Val.from(&mem, VectorList, [_]Val.Ptr{ bin, bin });
    try write.writeVal(lst);
    try testing.expect(std.mem.eql(u8, buf[0..13], "(\"foo\" \"foo\")"));
}

test "write zero" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const fix = try Val.from(&mem, Fix, 0);
    try write.writeVal(fix);
    try testing.expect(std.mem.eql(u8, buf[0..1], "0"));
}

test "write single digit" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const fix = try Val.from(&mem, Fix, 7);
    try write.writeVal(fix);
    try testing.expect(std.mem.eql(u8, buf[0..1], "7"));
}

test "write two digits" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const fix = try Val.from(&mem, Fix, 21);
    try write.writeVal(fix);
    try testing.expect(std.mem.eql(u8, buf[0..2], "21"));
}

test "write negative number" {
    var mem = try Mem.init(testing.allocator, config);
    defer mem.deinit();
    var buf: [256]u8 = undefined;

    var str = std.io.fixedBufferStream(&buf);
    const writer = str.writer();
    var write = valueWriter(writer, &mem);
    const fix = try Val.from(&mem, Fix, -23);
    try write.writeVal(fix);
    try testing.expect(std.mem.eql(u8, buf[0..3], "-23"));
}

// doto: test write derive opr/app
