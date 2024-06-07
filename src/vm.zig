const std = @import("std");
const wabi = @import("wabi.zig");
const Allocator = std.mem.Allocator;
const VectorList = wabi.list.VectorList;
const Val = wabi.val.Val;
const Mem = wabi.mem.Mem;
const Size = wabi.mem.Size;
const Env = wabi.env.Env;
const testing = std.testing;
const Block = wabi.bin.Block;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const ValueReader = wabi.read.ValueReader;
const ValueWriter = wabi.write.ValueWriter;
const Cont = wabi.cont.Cont;
const Call = wabi.cont.Call;
const Apply = wabi.cont.Apply;
const Eval = wabi.cont.Eval;
const Args = wabi.cont.Args;
const Prog = wabi.cont.Prog;
const Builtin = wabi.comb.Builtin;
const Derived = wabi.comb.Derived;
const Control = wabi.comb.Control;
const Mark = wabi.cont.Mark;
const Meta = wabi.meta.Meta;
const builtins = wabi.builtin.builtins;
const env0 = wabi.builtin.env0;
const Copy = wabi.copy.Copy;

// TODO: my errors
pub const Vm = struct {
    mem: Mem,
    ctrl: Val.Ptr,
    env: Env.Ptr,
    cont: ?Cont.Ptr,
    mark: ?Val.Ptr,
    meta: ?Meta.Ptr,
    on_error: ?Val.Ptr = null,
    //todo: change name and put the default value to the config
    // gas: usize = 1024 * 1024,
    well_known: struct {
        nil: Val.Ptr,
        true_kw: Val.Ptr,
        false_kw: Val.Ptr,
        empty_list: Val.Ptr,
        ignore: Val.Ptr,
        ampersand: Val.Ptr,
    },

    const Self = @This();

    //todo move in a config file
    var debug = false;

    pub fn initFromMem(
        m: Mem,
    ) !Self {
        var mem = m;
        const binNil = try mem.intern(Block, @constCast("nil"));
        const nil = try Val.from(&mem, Kwd, binNil);
        const empty_list = try Val.from(&mem, VectorList, .{});
        const true_bin = try mem.intern(Block, @constCast("t"));
        const true_kw = try Val.from(&mem, Kwd, true_bin);
        const false_bin = try mem.intern(Block, @constCast("f"));
        const false_kw = try Val.from(&mem, Kwd, false_bin);
        const ignore_bin = try mem.intern(Block, @constCast("_"));
        const ignore_sym = try Val.from(&mem, Sym, ignore_bin);
        const ampersand_bin = try mem.intern(Block, @constCast("&"));
        const ampersand_kwd = try Val.from(&mem, Kwd, ampersand_bin);
        const env = try Env.root(&mem, 1);
        return .{
            .ctrl = nil,
            .env = env,
            .cont = null,
            .mark = null,
            .meta = null,
            .mem = mem,
            // .gas = config.gas,
            .well_known = .{
                .nil = nil,
                .empty_list = empty_list,
                .true_kw = true_kw,
                .false_kw = false_kw,
                .ignore = ignore_sym,
                .ampersand = ampersand_kwd,
            },
        };
    }

    pub fn init(
        //todo: give config some useful defaults
        config: struct {
            allocator: Allocator,
            mem_size: Mem.Size,
            // gas: usize = 1024 * 1024,
        },
    ) !Vm {
        const mem = try Mem.init(config.allocator, .{
            .mem_size = config.mem_size,
        });
        const vm = Self.initFromMem(mem);
        return vm;
    }

    pub fn deinit(
        self: *Self,
    ) void {
        self.mem.deinit();
    }

    pub fn reader(
        self: *Self,
        zigReader: anytype,
    ) ValueReader(@TypeOf(zigReader)) {
        const VReader = ValueReader(@TypeOf(zigReader));
        return VReader.init(zigReader, &self.mem);
    }

    pub fn writer(
        self: *Self,
        zigWriter: anytype,
    ) ValueWriter(@TypeOf(zigWriter)) {
        const VWriter = ValueWriter(@TypeOf(zigWriter));
        return VWriter.init(zigWriter, &self.mem);
    }

    fn unfinished(self: *Self) bool {
        return self.cont != null or self.meta != null;
    }

    pub fn reduceAll(self: *Self) !void {
        while (self.unfinished()) try self.reduce();
    }

    fn buildError(
        self: *Self,
        e: anytype,
    ) !Val.Ptr {
        return try Val.from(&self.mem, wabi.num.Fix, errCode(e));
    }

    fn errCode(
        e: anytype,
    ) u32 {
        return switch (e) {
            error.MemoryOverflow => 1,
            // error.OutOfGas => 2,
            error.ArgumentError => 3,
            error.ArityError => 4,
            error.ReadError => 5,
            error.UnboundSymbol => 7,
            error.EmptyContinuation => 9,
            error.MarkNotFoundError => 10,
            error.SymbolAlreadyBound => 11,
            error.UnknownBuiltin => 12,
            error.ImpossibleError => 13,
            error.InvalidCombiner => 14,
            error.GenericError => 15,
            error.OutOfMemory => 16,
        };
    }

    fn onError(self: *Self, err: anytype) !void {
        if (self.on_error) |on_error| {
            const ctrl0 = self.ctrl;
            const code = try self.buildError(err);
            const args = try VectorList.create(&self.mem, 2);
            try args.set(&self.mem, 0, code);
            try args.set(&self.mem, 1, ctrl0);
            const cont = try Cont.push(self.cont, &self.mem, Call, .{
                .combiner = on_error,
                .env = self.env,
            });
            self.ctrl = @ptrCast(args);
            self.cont = cont;
        } else {
            self.cont = null;
            self.meta = null;
            return err;
        }
    }
    fn reduceAndCatch(
        self: *Self,
    ) !void {
        self.reduceAll() catch |e| {
            // std.debug.print("BEFORE {} Free:{}\n", .{ e, self.mem.len - self.mem.heap });
            // self.printVal(self.ctrl);
            if (e == error.MemoryOverflow) self.mem.useReserve();
            // std.debug.print("AFTER Free:{}\n", .{self.mem.len - self.mem.heap});
            try self.onError(e);
        };
    }

    pub fn run(self: *Self) !void {
        while (self.unfinished()) try self.reduceAndCatch();
    }

    // todo: reduce to a loop?
    fn reduce(self: *Self) !void {
        // if (self.cpu == 0) return error.OutOfCPU;
        // self.cpu -= 1;
        if (Self.debug) {
            std.debug.print("\nctrl: ", .{});
            self.printVal(self.ctrl);
            std.debug.print("\n", .{});
            try self.printCont();
            std.debug.print("\n", .{});
        }
        if (self.cont) |cnt| switch (cnt.tag()) {
            .KEval => {
                try self.reduceEval(cnt);
            },
            .KApply => {
                try self.reduceApply(cnt);
            },
            .KCall => {
                try self.reduceCall(cnt);
            },
            .KArgs => {
                try self.reduceArgs(cnt);
            },
            .KSel => {
                try self.reduceSel(cnt);
            },
            .KDef => {
                try self.reduceDef(cnt);
            },
            .KProg => {
                try self.reduceProg(cnt);
            },
            else => {
                unreachable;
            },
        } else if (self.meta) |actual_meta| {
            if (Self.debug) {
                std.debug.print("META: ", .{});
            }
            self.cont = actual_meta.cont.get(&self.mem);
            self.mark = actual_meta.mark.get(&self.mem);
            self.meta = actual_meta.pop(&self.mem);
            if (Self.debug) {
                try self.printCont();
                std.debug.print("\n", .{});
            }
        }
    }

    const SEError = error{UnboundSymbol};

    // todo: selfeval is for env. (move to Env struct)
    pub fn selfEval(
        self: *Self,
        val: Val.Ptr,
    ) SEError!?Val.Ptr {
        const res = try self.selfEvalEnv(self.env, val);
        return res;
    }
    // todo: selfeval is for env. (move to Env struct)
    pub fn selfEvalEnv(
        self: *Self,
        env: Env.Ptr,
        val: Val.Ptr,
    ) SEError!?Val.Ptr {
        switch (val.tag()) {
            .Sym => {
                const val1 = env.lookup(&self.mem, &val.sym) catch {
                    return null;
                };
                return val1;
            },
            .LVec => {
                if (val.list.len == 0) {
                    return val;
                } else {
                    return null;
                }
            },
            else => {
                return val;
            },
        }
    }

    fn reduceEval(
        self: *Self,
        eval_cont: Cont.Ptr,
    ) !void {
        const ctrl: Val.Ptr = self.ctrl;
        const env: Env.Ptr = self.env;
        const mem = &self.mem;
        var cont = eval_cont.pop(mem);

        switch (ctrl.tag()) {
            .Sym => {
                // eval symbol
                const val: Val.Ptr = try env.lookup(mem, @ptrCast(self.ctrl));
                self.ctrl = val;
                self.cont = cont;
            },
            .LVec => {
                // eval application
                const list: *VectorList = &ctrl.list;
                if (list.len == 0) {
                    self.cont = cont;
                } else {
                    const comb = list.get(mem, 0);
                    //todo: vectorlists as vlists
                    const args = try list.popLeftU(mem);
                    cont = try Cont.push(cont, mem, Apply, .{
                        .args = args,
                        .env = env,
                    });

                    if (self.selfEvalEnv(env, comb) catch null) |comb1| {
                        self.ctrl = comb1;
                        self.cont = cont;
                    } else {
                        cont = try Cont.push(cont, mem, Eval, .{});
                        self.ctrl = comb;
                        self.cont = cont;
                    }
                }
            },
            else => {
                // self eval
                self.cont = cont;
            },
        }
    }

    fn reduceApply(
        self: *Self,
        apply_cont: Cont.Ptr,
    ) !void {
        const mem = &self.mem;
        var ctrl = self.ctrl;
        var cont = apply_cont.pop(mem);
        const args = apply_cont.apply.args.get(mem);
        const env = apply_cont.apply.env.get(mem);

        cont = try Cont.push(cont, mem, Call, .{
            .combiner = self.ctrl,
            .env = env,
        });

        if (args.len > 0 and ctrl.isApplicative()) {
            // applicative (and some input arguments)
            var data: [16]Val.Ptr = undefined;
            var j: usize = 0;
            const mx = if (args.len < data.len) args.len else data.len;
            while (j < mx) : (j += 1) {
                const arg_j = args.get(mem, j);
                if (try self.selfEvalEnv(env, arg_j)) |v| {
                    data[j] = v;
                } else {
                    break;
                }
            }
            if (j < args.len) {
                // compound expression
                const part = try VectorList.from(mem, data[0..j]);
                cont = try Cont.push(cont, mem, Args, .{
                    .args = args,
                    .part = part,
                    .env = env,
                });
                cont = try Cont.push(cont, mem, Eval, .{});
                ctrl = args.get(mem, j);
                self.ctrl = ctrl;
                self.cont = cont;
            } else {
                // args are literals or symbols
                ctrl = try Val.from(mem, VectorList, data[0..j]);
                self.ctrl = ctrl;
                self.cont = cont;
            }
        } else {
            // operative or args.len == 0;
            self.ctrl = Val.cast(args);
            self.cont = cont;
        }
    }

    fn reduceArgs(
        self: *Self,
        args_cont: Cont.Ptr,
    ) !void {
        // todo: loop over missing args for selfevalauble
        const mem = &self.mem;
        var ctrl = self.ctrl;
        var cont = args_cont.pop(mem);
        const args = args_cont.args.args.get(mem);
        var part = args_cont.args.part.get(mem);
        const env = args_cont.args.env.get(mem);

        part = try part.pushRight(mem, ctrl);

        var data: [16]Val.Ptr = undefined;
        const part_len = part.len;
        const args_len = args.len;
        const delta = args_len - part_len;

        var j: usize = 0;
        while (j < delta and j < data.len) : (j += 1) {
            const arg_j = args.get(mem, part_len + j);
            if (try self.selfEvalEnv(env, arg_j)) |v| {
                data[j] = v;
            } else {
                break;
            }
        }
        part = try part.concatA(mem, data[0..j]);

        if (args.len == part.len) {
            // all arguments are evaluated
            ctrl = Val.cast(part);
            self.cont = cont;
            self.ctrl = ctrl;
            return;
        }
        // the next argument is complex: go through args/evalEval continuation
        cont = try Cont.push(cont, mem, Args, .{
            .args = args,
            .part = part,
            .env = env,
        });
        cont = try Cont.push(cont, mem, Eval, .{});
        ctrl = args.get(mem, part.len);
        self.ctrl = ctrl;
        self.cont = cont;
        self.env = env;
    }

    fn reduceCall(
        self: *Self,
        call_cont: Cont.Ptr,
    ) !void {
        // self.gas -= 1;
        // if (self.gas == 0) return error.OutOfGas;
        const mem = &self.mem;
        var ctrl = self.ctrl;
        const combiner = call_cont.call.combiner.get(mem);
        if (ctrl.tag() != .LVec) return error.ArgumentError;

        switch (combiner.tag()) {
            .BOpr, .BApp => try self.callBuiltin(&combiner.builtin, &ctrl.list, call_cont),
            .LVec => try self.callList(&combiner.list, &ctrl.list, call_cont),
            // .Env => try self.callEnv(&combiner.env, &ctrl.list, cont),
            .DOpr, .DApp => try self.callDerived(&combiner.derived, &ctrl.list, call_cont),
            .Kont => try self.callControl(&combiner.control, &ctrl.list, call_cont),
            else => {
                return error.InvalidCombiner;
            },
        }
    }

    fn reduceDef(self: *Self, def_cont: Cont.Ptr) !void {
        const mem = &self.mem;
        const ctrl = self.ctrl;
        const env = def_cont.def.env.get(mem);
        const sym = def_cont.def.sym.get(mem);
        try env.def(mem, sym, ctrl);
        const cont = def_cont.pop(mem);
        self.cont = cont;
    }

    pub fn isFalsey(self: *Self, val: Val.Ptr) bool {
        return (val.*.word == self.well_known.false_kw.*.word) or
            (val.*.word == self.well_known.nil.*.word);
    }

    pub fn isTruey(self: *Self, val: Val.Ptr) bool {
        return !self.isFalsey(val);
    }

    fn reduceSel(
        self: *Self,
        select_cont: Cont.Ptr,
    ) !void {
        const mem = &self.mem;
        var cont = select_cont.pop(mem);
        const env = select_cont.sel.env.get(mem);
        const expr = if (self.isTruey(self.ctrl))
            select_cont.sel.then_branch.get(mem)
        else
            select_cont.sel.else_branch.get(mem);

        if (try self.selfEvalEnv(env, expr)) |val| {
            self.ctrl = val;
            self.cont = cont;
        } else {
            cont = try Cont.push(cont, mem, Eval, .{});
            self.ctrl = expr;
            self.cont = cont;
            self.env = env;
        }
    }

    fn reduceProg(self: *Self, prog_cont: Cont.Ptr) !void {
        const mem = &self.mem;
        const env = prog_cont.prog.env.get(mem);
        const exs = prog_cont.prog.exs.get(mem);
        const cur = prog_cont.prog.cur;
        const nxt = cur + 1;
        const ctrl = exs.get(mem, nxt);
        var cont = prog_cont.pop(mem);

        if (nxt + 1 < exs.len) {
            cont = try Cont.push(cont, mem, Prog, .{
                .env = env,
                .exs = exs,
                .cur = nxt,
            });
        }
        cont = try Cont.push(cont, mem, Eval, .{});
        self.ctrl = ctrl;
        self.cont = cont;
        self.env = env;
    }

    fn callBuiltin(
        self: *Self,
        combiner: *Builtin,
        args: *VectorList,
        call_cont: Cont.Ptr,
    ) !void {
        const uid = combiner.uid;
        if (uid >= builtins.len) return error.UnknownBuiltin;
        try builtins[uid](self, args, call_cont);
    }

    fn callList(
        self: *Self,
        combiner: *VectorList,
        args: *VectorList,
        call_cont: Cont.Ptr,
    ) !void {
        const mem = &self.mem;
        if (args.len != 1)
            return error.ArityError;

        const val = args.get(mem, 0);
        if (val.tag() != .Fix)
            return error.ArgumentError;

        const cont = call_cont.pop(mem);
        const j = val.fix.val;
        if (j < 0 or j >= combiner.len) {
            self.ctrl = self.well_known.nil;
            self.cont = cont;
            return;
        }

        const ctrl = combiner.get(mem, @as(usize, @intCast(j)));
        self.cont = cont;
        self.ctrl = ctrl;
    }

    fn extend(self: *Self, env: Env.Ptr, n: anytype) !Env.Ptr {
        const mem = &self.mem;
        if (n <= 3) return try env.extend(mem, 2);
        if (n <= 7) return try env.extend(mem, 3);
        if (n <= 15) return try env.extend(mem, 4);
        return try env.extend(mem, 5);
    }

    fn callDerived(
        self: *Self,
        combiner: Derived.Ptr,
        args: VectorList.Ptr,
        call_cont: Cont.Ptr,
    ) !void {
        const mem = &self.mem;
        const dynamic_env = self.env;
        const static_env = self.mem.fromOffset(Env.Ptr, combiner.env_offset);
        const pattern = combiner.pattern.get(mem);
        var body = combiner.body.get(mem);
        var env_name = combiner.caller_env_name;

        var env = try self.extend(static_env, args.len);
        // this can be static
        if (combiner.tag == .DOpr) try env.def(mem, &env_name, Val.cast(dynamic_env));
        try self.bindPattern(env, pattern, args);
        if (body.len == 0) return error.ImpossibleError;
        var cont = call_cont.pop(mem);
        if (body.len > 1) cont = try Cont.push(cont, mem, Prog, .{
            .env = env,
            .exs = body,
            .cur = 0,
        });
        cont = try Cont.push(cont, mem, Eval, .{});

        const ctrl = body.get(mem, 0);
        // todo: 3 must be defined from pattern
        self.env = env;
        self.ctrl = ctrl;
        self.cont = cont;
    }

    fn bindPattern(
        self: *Self,
        env: Env.Ptr,
        patt: Val.Ptr,
        args: *VectorList,
    ) !void {
        const mem = &self.mem;
        switch (patt.tag()) {
            .LVec => {
                for (patt.list.refs(), 0..) |*ref, j| {
                    const sym = ref.get(mem);
                    if (sym.word == self.well_known.ignore.word) continue;
                    if (sym.word == self.well_known.ampersand.word) {
                        //verify the shape of the tail in `fn` combiner
                        if (patt.list.len != j + 2) return error.ArityError;
                        const syms = patt.list.get(mem, j + 1);
                        if (syms.tag() != .Sym) return error.ArgumentError;
                        if (j > args.len) return error.ArityError;
                        const rest = try args.sub(mem, j, args.len);
                        try env.def(mem, &syms.sym, Val.cast(rest));
                        return;
                    }
                    if (sym.tag() != .Sym) return error.ArgumentError;
                    if (j >= args.len) return error.ArityError;
                    const val = args.get(mem, j);
                    try env.def(mem, &sym.sym, val);
                }
            },
            .Sym => {
                try env.def(mem, &patt.sym, Val.cast(args));
            },
            else => {
                return error.ArgumentError;
            },
        }
    }

    fn callEnv(
        self: *Self,
        env: Env.Ptr,
        args: *VectorList,
        call_cont: Cont.Ptr,
    ) !void {
        _ = .{ self, env, args, call_cont };
        unreachable;
    }

    fn callControl(
        self: *Self,
        cont_combiner: Control.Ptr,
        args: *VectorList,
        call_cont: Cont.Ptr,
    ) !void {
        if (args.len != 1) return error.ArityError;

        const mem = &self.mem;
        const val = args.get(mem, 0);
        var cont = call_cont.pop(mem);
        var mark = self.mark;
        var meta = self.meta;
        meta = try Meta.push(meta, mem, .{
            .cont = cont,
            .mark = mark,
        });
        cont = cont_combiner.cont.get(mem);
        mark = cont_combiner.cont_mark.get(mem);

        const rev_meta = cont_combiner.rev_meta.get(mem);
        meta = try Meta.revConj(rev_meta, mem, meta);

        self.ctrl = val;
        self.meta = meta;
        self.cont = cont;
        self.mark = mark;
    }

    pub fn evalString(
        self: *Self,
        in: anytype,
        out: []u8,
    ) ![]u8 {
        const mem = &self.mem;
        var str_in = std.io.fixedBufferStream(in);
        var str_out = std.io.fixedBufferStream(out);
        var read_in = self.reader(str_in.reader());
        var write_out = self.writer(str_out.writer());
        const ctrl = try read_in.readVal();
        const cont = try Cont.push(null, mem, Eval, .{});
        // HERE
        try env0(self);
        self.ctrl = ctrl;
        self.cont = cont;
        try self.run();

        try write_out.writeVal(self.ctrl);
        return out[0..str_out.pos];
    }

    pub fn printVal(self: *Self, val: Val.Ptr) void {
        var out = std.io.getStdOut();
        var write_out = self.writer(out.writer());
        write_out.writeVal(val) catch unreachable;
    }

    pub fn printCont(self: *Self) !void {
        if (self.cont) |cont|
            switch (cont.tag()) {
                .KEval => std.debug.print("{}\n", .{cont.eval}),
                .KApply => std.debug.print("{}\n", .{cont.apply}),
                .KArgs => std.debug.print("{}\n", .{cont.args}),
                .KCall => std.debug.print("{}\n", .{cont.call}),
                .KDef => std.debug.print("{}\n", .{cont.def}),
                .KSel => std.debug.print("{}\n", .{cont.sel}),
                .KProg => std.debug.print("{}\n", .{cont.prog}),
                else => std.debug.print("unk\n", .{}),
            };

        // var cont = self.cont;
        // std.debug.print("\n--- begin cont ", .{});
        // if (self.mark) |m| {
        //     self.printVal(m);
        // } else {
        //     std.debug.print("NONE", .{});
        // }
        // std.debug.print("---\n", .{});

        // while (cont) |c| {
        //     std.debug.print("{}", .{c.tag()});
        //     switch (c.tag()) {
        //         .KProg => {
        //             std.debug.print(" ", .{});
        //             self.printVal(Val.cast(c.prog.exs.get(&self.mem)));
        //             std.debug.print(" {}", .{c.prog.cur});
        //         },
        //         else => {},
        //     }
        //     std.debug.print("\n", .{});
        //     cont = c.pop(&self.mem);
        // }
        // std.debug.print("--- end cont ---\n", .{});
    }

    fn printCtrl(self: *Self, ctrl: Val.Ptr) !void {
        var out = std.io.getStdOut();
        const out_writer = out.writer();
        var value_writer = self.writer(out_writer);
        std.debug.print("ctrl: ", .{});
        try value_writer.writeVal(ctrl);
        std.debug.print("\n", .{});
    }
};

const vmConfig = .{
    .allocator = testing.allocator,
    .mem_size = 1_024 * 1_024,
};

test "init vm" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();

    try testing.expectEqual(vm.mem.space.len, vmConfig.mem_size);
}

test "eval literal" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("10", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "10"));
}

test "eval symbol" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("+", out_buf[0..100]);
    // std.debug.print("res: |{s}|\n", .{res});
    try testing.expect(std.mem.eql(u8, res, "[.\\ 0]"));
}

test "eval empty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("()", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "()"));
}

test "eval sum of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+)", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "eval sum of a number" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+ 101)", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "101"));
}

test "eval sum of 2 numbers" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+ 79 83)", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "162"));
}

test "eval sum of more than one number" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 1_024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+ 79 83 11)", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "173"));
}

test "nested expression" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+ (+))", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "generic nested" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(+ 2 (+ 5 4) 0 (- (+ 5 5) 2))", out_buf[0..100]);
    try testing.expect(std.mem.eql(u8, res, "19"));
}

test "define" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(def a (+ 1 2))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "3"));
    const bin = try vm.mem.intern(Block, @constCast("a"));
    const sym = try Val.from(&vm.mem, Sym, bin);
    const val = try vm.env.lookup(&vm.mem, &sym.sym);
    try testing.expectEqual(val.fix.val, 3);
}

test "conditional execution" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if 10 :kw 33)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":kw"));
}

test "call function" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (def inc (fn (x) (+ x 1))) (inc 123))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "124"));
}

test "call operator" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (def oper (fx _ (x) x)) (oper (foo bar)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(foo bar)"));
}

test "list as combiner" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (def l ((fn xs xs) 1 2 3)) (l 1))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "2"));
}

test "list as combiner errors" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 100;
    var out_buf: [1024]u8 = undefined;
    try testing.expectError(
        error.ArgumentError,
        vm.evalString("(do (def l ((fn xs xs) 1 2 3)) (l :k))", out_buf[0..1024]),
    );
    try testing.expectError(
        error.ArityError,
        vm.evalString("(do (def l ((fn xs xs) 1 2 3)) (l 1 1))", out_buf[0..1024]),
    );
}

test "list as combiner out of bounds" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    // vm.cpu = 100;
    var out_buf: [1024]u8 = undefined;
    const n0 = try vm.evalString("(do (def l ((fn xs xs) 1 2 3)) (l -1))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, n0, ":nil"));
    const n1 = try vm.evalString("(do (def l ((fn xs xs) 1 2 3)) (l 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, n1, ":nil"));
}
