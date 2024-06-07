const std = @import("std");
const wabi = @import("wabi.zig");
const Vm = wabi.vm.Vm;
const Val = wabi.val.Val;
const VectorList = wabi.list.VectorList;
const SInt = wabi.types.SInt;
const Env = wabi.env.Env;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const Builtin = wabi.comb.Builtin;
const Derived = wabi.comb.Derived;
const Meta = wabi.meta.Meta;
const Cont = wabi.cont.Cont;
const Eval = wabi.cont.Eval;
const Prog = wabi.cont.Prog;
const Select = wabi.cont.Select;
const Control = wabi.comb.Control;
const Tag = wabi.types.Tag;

const testing = std.testing;

const Error = error{
    ArgumentError,
    MemoryOverflow,
    EmptyContinuation,
    ArityError,
    MarkNotFoundError,
    SymbolAlreadyBound,
    UnboundSymbol,
    GenericError,
    ReadError,
    OutOfMemory,
};

const Fun = *const fn (
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void;

fn sum(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var res: SInt = 0;
    for (args.refs()) |*r| {
        const v = r.get(&vm.mem);
        switch (v.tag()) {
            .Fix => res += v.fix.val,
            else => return error.ArgumentError,
        }
    }
    const ctrl = try Val.from(&vm.mem, wabi.num.Fix, res);
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = ctrl;
}

fn mul(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var res: SInt = 1;
    for (args.refs()) |*r| {
        const v = r.get(&vm.mem);
        switch (v.tag()) {
            .Fix => res *= v.fix.val,
            else => return error.ArgumentError,
        }
    }
    const ctrl = try Val.from(&vm.mem, wabi.num.Fix, res);
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = ctrl;
}

fn sub(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    switch (args.len) {
        0 => {
            return error.ArityError;
        },
        1 => {
            const val = args.get(&vm.mem, 0);
            switch (val.tag()) {
                .Fix => {
                    const res = try Val.from(&vm.mem, wabi.num.Fix, -val.fix.val);
                    vm.cont = call_cont.pop(&vm.mem);
                    vm.ctrl = res;
                    return;
                },
                else => {
                    return error.ArgumentError;
                },
            }
        },
        else => {
            const val = args.get(&vm.mem, 0);
            var res: SInt = val.fix.val;
            for (args.refs()[1..]) |*r| {
                const v = r.get(&vm.mem);
                switch (v.tag()) {
                    .Fix => res -= v.fix.val,
                    else => return error.ArgumentError,
                }
            }
            const resV = try Val.from(&vm.mem, wabi.num.Fix, res);
            vm.cont = call_cont.pop(&vm.mem);
            vm.ctrl = resV;
        },
    }
}

fn div(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    switch (args.len) {
        0 => {
            return error.ArityError;
        },
        1 => {
            const val = args.get(&vm.mem, 0);
            switch (val.tag()) {
                .Fix => {
                    // actually 1 / args[0];
                    const res = try Val.from(&vm.mem, wabi.num.Fix, 0);
                    vm.cont = call_cont.pop(&vm.mem);
                    vm.ctrl = res;
                    return;
                },
                else => {
                    return error.ArgumentError;
                },
            }
        },
        else => {
            const val = args.get(&vm.mem, 0);
            var res: SInt = val.fix.val;
            for (args.refs()[1..]) |*r| {
                const v = r.get(&vm.mem);
                switch (v.tag()) {
                    .Fix => res = @divTrunc(res, v.fix.val),
                    else => return error.ArgumentError,
                }
            }
            const resV = try Val.from(&vm.mem, wabi.num.Fix, res);
            vm.cont = call_cont.pop(&vm.mem);
            vm.ctrl = resV;
        },
    }
}

fn left(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const arg = args.get(&vm.mem, 0);
    switch (arg.tag()) {
        .LVec => {
            const ctrl = if (arg.list.left(&vm.mem)) |v| v else vm.well_known.nil;
            vm.cont = call_cont.pop(&vm.mem);
            vm.ctrl = ctrl;
        },
        else => {
            if (arg.word == vm.well_known.nil.word) {
                vm.ctrl = vm.well_known.nil;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
            return error.ArgumentError;
        },
    }
}

fn popLeft(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const lst = args.get(&vm.mem, 0);
    switch (lst.tag()) {
        .LVec => {
            const ctrl = if (try lst.list.popLeft(&vm.mem)) |v| Val.cast(v) else vm.well_known.nil;
            vm.cont = call_cont.pop(&vm.mem);
            vm.ctrl = ctrl;
        },
        else => {
            if (lst.word == vm.well_known.nil.word) {
                vm.cont = call_cont.pop(&vm.mem);
                vm.ctrl = vm.well_known.nil;
                return;
            }
            return error.ArgumentError;
        },
    }
}

fn listStar(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (try args.destructRight(&vm.mem)) |dst| {
        const elems = dst.left;
        const lst = dst.right;
        switch (lst.tag()) {
            .LVec => {
                const ctrl = try elems.concat(&vm.mem, &lst.list);
                vm.cont = call_cont.pop(&vm.mem);
                vm.ctrl = Val.cast(ctrl);
            },
            else => {
                return error.ArgumentError;
            },
        }
    } else {
        return error.ArityError;
    }
}

fn right(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const arg = args.get(&vm.mem, 0);
    switch (arg.tag()) {
        .LVec => {
            const ctrl = if (arg.list.right(&vm.mem)) |v| v else vm.well_known.nil;
            vm.ctrl = ctrl;
            vm.cont = call_cont.pop(&vm.mem);
        },
        else => {
            if (arg.word == vm.well_known.nil.word) {
                vm.ctrl = vm.well_known.nil;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
            return error.ArgumentError;
        },
    }
}

fn popRight(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArgumentError;
    const lst = args.get(&vm.mem, 0);
    switch (lst.tag()) {
        .LVec => {
            const ctrl = if (try lst.list.popRight(&vm.mem)) |v| Val.cast(v) else vm.well_known.nil;
            vm.cont = call_cont.pop(&vm.mem);
            vm.ctrl = ctrl;
        },
        else => {
            if (lst.word == vm.well_known.nil.word) {
                vm.ctrl = vm.well_known.nil;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
            return error.ArgumentError;
        },
    }
}

fn starList(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (try args.destructLeft(&vm.mem)) |dst| {
        const lst = dst.left;
        const elems = dst.right;
        switch (lst.tag()) {
            .LVec => {
                const lst_ = &lst.list;
                const ctrl = try lst_.concat(&vm.mem, elems);
                vm.cont = call_cont.pop(&vm.mem);
                vm.ctrl = Val.cast(ctrl);
            },
            else => {
                return error.ArgumentError;
            },
        }
    } else {
        return error.ArityError;
    }
}

fn concatList(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    const arg_refs = args.refs();
    const mem = &vm.mem;
    const total_len = blk: {
        var total: usize = 0;
        for (arg_refs) |*ref| {
            const lst = ref.get(mem);
            if (lst.tag() != .LVec) return error.ArgumentError;
            total += lst.list.len;
        }
        break :blk total;
    };
    var res = try VectorList.create(mem, total_len);
    const res_refs = res.refs();
    var j: usize = 0;
    for (arg_refs) |*ref| {
        for (ref.get(mem).list.refs()) |eref| {
            res_refs[j].word = eref.word;
            j += 1;
        }
    }
    std.debug.assert(j == total_len);
    vm.ctrl = Val.cast(res);
    vm.cont = call_cont.pop(&vm.mem);
}

fn length(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var res: SInt = 0;
    for (args.refs()) |*ref| {
        const lst = ref.get(&vm.mem);
        switch (lst.tag()) {
            .LVec => {
                // todo: handle the case of a length that doesn't fit in a `SInt`
                res += @as(SInt, @intCast(lst.list.len));
            },
            else => {
                if (lst.word == vm.well_known.nil.word) continue;
                return error.ArgumentError;
            },
        }
    }
    const ctrl = try Val.from(&vm.mem, wabi.num.Fix, res);
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = ctrl;
}

fn define(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 2) return error.ArityError;
    const maybeSym = args.get(&vm.mem, 0);
    const expr = args.get(&vm.mem, 1);
    switch (maybeSym.tag()) {
        .Sym => {
            // todo: optimize if expr is selfEval...
            const sym = &maybeSym.sym;
            var cont = call_cont.pop(&vm.mem);
            cont = try Cont.push(cont, &vm.mem, wabi.cont.Def, .{
                .sym = sym,
                .env = vm.env,
            });
            cont = try Cont.push(cont, &vm.mem, wabi.cont.Eval, .{});
            vm.ctrl = expr;
            vm.cont = cont;
        },
        else => {
            return error.ArgumentError;
        },
    }
}

fn cond(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var j: usize = 0;
    const l0 = args.len;
    const c1 = call_cont.pop(&vm.mem);
    while (j <= l0) : (j += 2) {
        switch (l0 - j) {
            0 => {
                // (if)
                vm.ctrl = vm.well_known.nil;
                vm.cont = c1;
                return;
            },
            1 => {
                // (if <e>)
                const expr = args.get(&vm.mem, j);
                if (try vm.selfEval(expr)) |v1| {
                    vm.ctrl = v1;
                    vm.cont = c1;
                    return;
                }
                const c2 = try Cont.push(c1, &vm.mem, Eval, .{});
                vm.ctrl = expr;
                vm.cont = c2;
            },
            else => {
                // (if when then ...)
                const when = args.get(&vm.mem, j);
                const then = args.get(&vm.mem, j + 1);

                if (try vm.selfEval(when)) |w1| {
                    if (vm.isFalsey(w1)) continue;
                    // when is true
                    if (try vm.selfEval(then)) |t1| {
                        // then is selfeval

                        vm.ctrl = t1;
                        vm.cont = c1;
                        return;
                    }
                    // then is not selfeval
                    const c2 = try Cont.push(c1, &vm.mem, Eval, .{});
                    vm.ctrl = then;
                    vm.cont = c2;
                    return;
                }
                const _else = switch (l0 - 2 - j) {
                    0 => vm.well_known.nil,
                    1 => args.get(&vm.mem, j + 2),
                    else => blk: {
                        const cx = call_cont.call.combiner.get(&vm.mem);
                        const as = try args.sub(&vm.mem, j + 1, l0);
                        try as.set(&vm.mem, j, Val.cast(cx));
                        break :blk Val.cast(as);
                    },
                };
                const c2 = try Cont.push(c1, &vm.mem, Select, .{
                    .env = vm.env,
                    .then_branch = then,
                    .else_branch = _else,
                });
                const c3 = try Cont.push(c2, &vm.mem, Eval, .{});
                vm.ctrl = when;
                vm.cont = c3;
                return;
            },
        }
    }
}

fn do(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    switch (args.len) {
        0 => {
            vm.ctrl = vm.well_known.nil;
            vm.cont = call_cont.pop(&vm.mem);
        },
        1 => {
            const ctrl = args.get(&vm.mem, 0);
            var cont = call_cont.pop(&vm.mem);
            cont = try Cont.push(cont, &vm.mem, wabi.cont.Eval, .{});
            vm.cont = cont;
            vm.ctrl = ctrl;
        },
        else => {
            const ctrl = args.get(&vm.mem, 0);
            var cont = call_cont.pop(&vm.mem);
            cont = try Cont.push(cont, &vm.mem, wabi.cont.Prog, .{
                .env = vm.env,
                .exs = args,
                .cur = 0,
            });
            cont = try Cont.push(cont, &vm.mem, wabi.cont.Eval, .{});
            vm.cont = cont;
            vm.ctrl = ctrl;
        },
    }
}

fn funct(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len <= 1) return error.ArityError;
    const patt = args.get(&vm.mem, 0);
    const body = try args.sub(&vm.mem, 1, args.len);
    const func = Val.cast(try Derived.applicative(&vm.mem, .{
        .env = vm.env,
        .env_name = &vm.well_known.ignore.sym,
        .pattern = patt,
        .body = body,
    }));
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = func;
}

fn fexpr(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    // (fx e (a b) c)
    if (args.len <= 2) return error.ArityError;
    const e_nm = args.get(&vm.mem, 0);
    if (e_nm.tag() != .Sym) return error.ArgumentError;
    const patt = args.get(&vm.mem, 1);
    const body = try args.sub(&vm.mem, 2, args.len);
    const func = Val.cast(try Derived.operative(&vm.mem, .{
        .env = vm.env,
        .env_name = &e_nm.sym,
        .pattern = patt,
        .body = body,
    }));
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = func;
}

fn wrap(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const fxpr = args.get(&vm.mem, 0);
    switch (fxpr.tag()) {
        .BApp, .DApp, .Kont, .LVec => {
            vm.ctrl = fxpr;
        },
        .BOpr => {
            const fnct = try vm.mem.one(Builtin);
            fnct.tag = .BApp;
            vm.ctrl = Val.cast(fnct);
        },
        .DOpr => {
            const fnct = try vm.mem.one(Derived);
            fnct.* = fxpr.derived;
            fnct.tag = .DApp;
            vm.ctrl = Val.cast(fnct);
        },
        else => {
            return error.ArgumentError;
        },
    }
    vm.cont = call_cont.pop(&vm.mem);
}
fn unwrap(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const fnct = args.get(&vm.mem, 0);
    switch (fnct.tag()) {
        // todo: for a kont wrap in an fexpr
        .BOpr, .DOpr, .Kont, .LVec => {
            vm.ctrl = fnct;
        },
        .BApp => {
            const fxpr = try vm.mem.one(Builtin);
            fxpr.* = fnct.builtin;
            fxpr.tag = .BOpr;
            vm.ctrl = Val.cast(fxpr);
        },
        .DApp => {
            const fxpr = try vm.mem.one(Derived);
            fxpr.* = fnct.derived;
            fxpr.tag = .DOpr;
            vm.ctrl = Val.cast(fxpr);
        },
        else => {
            return error.ArgumentError;
        },
    }
    vm.cont = call_cont.pop(&vm.mem);
}

fn eval(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len < 2) return error.ArityError;

    const env = args.get(&vm.mem, 0);
    const xpr = args.get(&vm.mem, 1);
    if (env.tag() != .Env) return error.ArgumentError;
    var cont = call_cont.pop(&vm.mem);
    if (args.len > 2) cont = try Cont.push(cont, &vm.mem, wabi.cont.Prog, .{
        .env = &env.env,
        .exs = args,
        .cur = 1,
    });
    cont = try Cont.push(cont, &vm.mem, wabi.cont.Eval, .{});

    vm.cont = cont;
    vm.ctrl = xpr;
    vm.env = &env.env;
}

fn MkTest(
    comptime testFn: fn (val: Val.Ptr) Error!bool,
) type {
    return struct {
        fn function(
            vm: *Vm,
            args: VectorList.Ptr,
            call_cont: Cont.Ptr,
        ) Error!void {
            const cont = call_cont.pop(&vm.mem);
            for (args.refs()) |*ref| {
                const val = ref.get(&vm.mem);
                const tst = try testFn(val);
                if (tst) continue;

                vm.cont = cont;
                vm.ctrl = vm.well_known.false_kw;
                return;
            }
            vm.cont = cont;
            vm.ctrl = vm.well_known.true_kw;
        }
    };
}

fn mkTest(
    comptime testFn: fn (val: Val.Ptr) Error!bool,
) Fun {
    return MkTest(testFn).function;
}

fn isOperative(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .BOpr, .DOpr => true,
        else => false,
    };
}

fn isApplicative(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .BApp, .DApp => true,
        else => false,
    };
}

fn isBuiltin(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .BOpr, .BApp => true,
        else => false,
    };
}

fn isDerived(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .DOpr, .DApp => true,
        else => false,
    };
}

fn isNumber(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .Fix => true,
        else => false,
    };
}

fn isBinary(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .BBlk => true,
        else => false,
    };
}

fn isSymbol(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .Sym => true,
        else => false,
    };
}

fn isKeyword(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .Kwd => true,
        else => false,
    };
}

fn isEnv(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .Env => true,
        else => false,
    };
}

fn isList(val: Val.Ptr) Error!bool {
    return switch (val.tag()) {
        .LVec => true,
        else => false,
    };
}

fn nameOf(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var s0 = try wabi.bin.Block.from(&vm.mem, "");
    for (args.refs()) |*ref| {
        const val = ref.get(&vm.mem);
        switch (val.tag()) {
            .Sym => {
                const bin = val.sym.binOf(&vm.mem);
                s0 = try s0.concat(&vm.mem, bin);
            },
            .Kwd => {
                const bin = val.kwd.binOf(&vm.mem);
                s0 = try s0.concat(&vm.mem, bin);
            },
            .BBlk => {
                s0 = try s0.concat(&vm.mem, &val.block);
            },
            else => {
                return error.ArgumentError;
            },
        }
    }
    vm.ctrl = Val.cast(s0);
    vm.cont = call_cont.pop(&vm.mem);
}

fn equal(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len > 1) {
        const refs = args.refs();
        const stone = refs[0].get(&vm.mem);
        for (refs[1..refs.len]) |*ref| {
            const other = ref.get(&vm.mem);
            const eq = stone.isEqual(other, &vm.mem);
            if (!eq) {
                vm.ctrl = vm.well_known.false_kw;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
        }
    }
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = vm.well_known.true_kw;
}

//todo: write in a more compact form
fn numEqual(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    switch (args.len) {
        0 => {
            vm.ctrl = vm.well_known.true_kw;
            vm.cont = call_cont.pop(&vm.mem);
            return;
        },
        1 => {
            const arg0 = args.get(&vm.mem, 0);
            if (arg0.tag() != .Fix) return error.ArgumentError;

            vm.ctrl = vm.well_known.true_kw;
            vm.cont = call_cont.pop(&vm.mem);
            return;
        },
        else => {
            const arg0 = args.get(&vm.mem, 0);
            if (arg0.tag() != .Fix) return error.ArgumentError;

            for (args.refs()[1..args.len]) |*ref| {
                const elem = ref.get(&vm.mem);
                if (elem.tag() != .Fix) return error.ArgumentError;
                //todo: not the case anymore for bignums
                if (elem.word != elem.word) {
                    vm.ctrl = vm.well_known.false_kw;
                    vm.cont = call_cont.pop(&vm.mem);
                    return;
                }
            }
            vm.ctrl = vm.well_known.false_kw;
            vm.cont = call_cont.pop(&vm.mem);
            return;
        },
    }
}

fn numGt(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var cand: ?Val.Ptr = null;

    for (args.refs()[0..args.len]) |*ref| {
        const arg = ref.get(&vm.mem);
        if (arg.tag() != .Fix) return error.ArgumentError;
        if (cand) |c| {
            if (c.fix.val <= arg.fix.val) {
                vm.ctrl = vm.well_known.false_kw;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
        }
        cand = arg;
    }

    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = vm.well_known.true_kw;
}

fn numLt(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var cand: ?Val.Ptr = null;

    for (args.refs()[0..args.len]) |*ref| {
        const arg = ref.get(&vm.mem);
        if (arg.tag() != .Fix) return error.ArgumentError;
        if (cand) |c| {
            if (c.fix.val >= arg.fix.val) {
                vm.ctrl = vm.well_known.false_kw;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
        }
        cand = arg;
    }

    vm.ctrl = vm.well_known.true_kw;
    vm.cont = call_cont.pop(&vm.mem);
}

fn numGtEq(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var cand: ?Val.Ptr = null;

    for (args.refs()[0..args.len]) |*ref| {
        const arg = ref.get(&vm.mem);
        if (arg.tag() != .Fix) return error.ArgumentError;
        if (cand) |c| {
            if (c.fix.val < arg.fix.val) {
                vm.ctrl = vm.well_known.false_kw;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
        }
        cand = arg;
    }

    vm.ctrl = vm.well_known.true_kw;
    vm.cont = call_cont.pop(&vm.mem);
}

fn numLtEq(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var cand: ?Val.Ptr = null;

    for (args.refs()[0..args.len]) |*ref| {
        const arg = ref.get(&vm.mem);
        if (arg.tag() != .Fix) return error.ArgumentError;
        if (cand) |c| {
            if (c.fix.val > arg.fix.val) {
                vm.ctrl = vm.well_known.false_kw;
                vm.cont = call_cont.pop(&vm.mem);
                return;
            }
        }
        cand = arg;
    }

    vm.ctrl = vm.well_known.true_kw;
    vm.cont = call_cont.pop(&vm.mem);
}

fn markOper(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len <= 1) return error.ArityError;
    const mem = &vm.mem;
    var meta = vm.meta;
    var cont = call_cont.pop(&vm.mem);
    var mark = vm.mark;
    var ctrl = vm.ctrl;

    meta = try Meta.push(meta, mem, .{
        .cont = cont,
        .mark = mark,
    });

    cont = null;
    mark = args.get(mem, 0);
    ctrl = args.get(mem, 1);

    if (args.len > 2) {
        cont = try Cont.push(cont, mem, Prog, .{
            .env = vm.env,
            .exs = args,
            .cur = 1,
        });
    }
    cont = try Cont.push(cont, mem, Eval, .{});

    vm.cont = cont;
    vm.ctrl = ctrl;
    vm.meta = meta;
    vm.cont = cont;
    vm.mark = mark;
}

fn abortOper(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len <= 2) return error.ArityError;
    const mem = &vm.mem;
    var meta = vm.meta;
    var cont = call_cont.pop(&vm.mem);
    var mark = vm.mark;

    const mark0 = args.get(mem, 0);
    const kname = args.get(mem, 1);

    if (kname.tag() != .Sym) return error.ArgumentError;

    var rev_meta: ?Meta.Ptr = null;

    while (meta) |actual_meta| {
        if (mark) |m| if (m.isEqual(mark0, mem)) break;

        mark = actual_meta.mark.get(mem);
        // to_do optimize (i.e. do not do cont.get(mem))
        rev_meta = try Meta.push(rev_meta, mem, .{
            .cont = actual_meta.cont.get(mem),
            .mark = mark,
        });
        meta = actual_meta.pop(mem);
    } else {
        return error.MarkNotFoundError;
    }

    const app = try Control.new(&vm.mem, .{
        .cont = cont,
        .cont_mark = if (rev_meta) |_| vm.mark else null,
        .rev_meta = rev_meta,
    });

    if (rev_meta) |rm| rm.mark.set(mem, null);

    var env = try vm.env.extend(mem, 1);
    try env.def(mem, &kname.sym, Val.cast(app));
    cont = null;

    if (args.len > 3) {
        cont = try Cont.push(cont, mem, Prog, .{
            .env = env,
            .exs = args,
            .cur = 2,
        });
    }
    cont = try Cont.push(cont, mem, Eval, .{});

    const ctrl = args.get(&vm.mem, 2);

    vm.ctrl = ctrl;
    vm.env = env;
    vm.cont = cont;
    vm.meta = meta;
}

/// todo: deprecated
fn clock(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 0) return error.ArityError;
    const t = std.time.milliTimestamp();
    const st = @as(SInt, @truncate(t)) & ~(@as(SInt, 1) << (@bitSizeOf(SInt) - 1));
    const fix = try Val.from(&vm.mem, wabi.num.Fix, st);
    vm.ctrl = fix;
    vm.cont = call_cont.pop(&vm.mem);
}

/// todo: deprecate
/// todo: do not use so many pointers, use loops over the current environment
fn load(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    const mem = &vm.mem;
    var cont = call_cont.pop(mem);
    var all_exps: [1024]Val.Ptr = undefined;
    const doOpr = Val.cast(try Builtin.from(mem, doOper, .BOpr));
    all_exps[0] = doOpr;
    var j: usize = 1;
    for (args.refs()) |*ref| {
        var file_exps: [1024]Val.Ptr = undefined;
        var maybe_bin = ref.get(mem);
        if (maybe_bin.tag() != .BBlk) return error.ArgumentError;
        const bin = maybe_bin.block.data();
        const file = std.fs.cwd().openFile(bin, .{
            .mode = .read_only,
        }) catch {
            return error.ReadError;
        };
        defer file.close();
        var buf_reader = std.io.bufferedReader(file.reader());
        const in_stream = buf_reader.reader();
        var val_reader = vm.reader(in_stream);

        file_exps[0] = doOpr;
        var k: usize = 1;
        while (val_reader.readVal()) |val| {
            file_exps[k] = val;
            k += 1;
        } else |e| {
            if (e != error.ReadError) return e;
        }
        all_exps[j] = try Val.from(mem, VectorList, file_exps[0..k]);
        j += 1;
    }
    const ctrl = try Val.from(mem, VectorList, all_exps[0..j]);
    cont = try Cont.push(cont, mem, Eval, .{});
    vm.cont = cont;
    vm.ctrl = ctrl;
}

//not acqually primitive but useful being primitive
// (def env (fx e _ e))
// (def new-env (fn () (env)))
// (def ext (fn (e) (eval e (list new-env))))
fn envExtend(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    const mem = &vm.mem;
    if (args.len != 1) return error.ArityError;
    const arg = args.get(mem, 0);
    if (arg.tag() != .Env) return error.ArgumentError;
    // var env: ?*Env = &arg.env;
    const env = try arg.env.extend(mem, 2);
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = Val.cast(env);
}

fn kwdCast(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const arg = args.get(&vm.mem, 0);
    vm.ctrl = switch (arg.tag()) {
        .Kwd => arg,
        .Sym => blk: {
            const bin = arg.sym.binOf(&vm.mem);
            break :blk try Val.from(&vm.mem, Kwd, bin);
        },
        .BBlk => blk: {
            const arr = arg.block.data();
            const bin = try vm.mem.intern(wabi.bin.Block, arr);
            break :blk try Val.from(&vm.mem, Kwd, bin);
        },
        else => {
            return error.ArgumentError;
        },
    };
    vm.cont = call_cont.pop(&vm.mem);
}

fn symCast(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const arg = args.get(&vm.mem, 0);
    vm.ctrl = switch (arg.tag()) {
        .Sym => arg,
        .Kwd => blk: {
            const bin = arg.sym.binOf(&vm.mem);
            break :blk try Val.from(&vm.mem, Sym, bin);
        },
        .BBlk => blk: {
            const arr = arg.block.data();
            const bin = try vm.mem.intern(wabi.bin.Block, arr);
            break :blk try Val.from(&vm.mem, Sym, bin);
        },
        else => {
            return error.ArgumentError;
        },
    };
    vm.cont = call_cont.pop(&vm.mem);
}

pub const magic = [_]u8{ 'W', 'A', 'B', 0, 0, 0, 0, 0 };

// todo: make it of 3, i.e.
// (dump "foo.fasl" expr (control :io k (fn (if (not it) (signal :io-error) (k it)))))
fn spitExpr(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 2) return error.ArityError;
    const allocator = vm.mem.allocator;
    const file_name = args.get(&vm.mem, 0);
    const expr = args.get(&vm.mem, 1);

    if (file_name.tag() != .BBlk) return error.ArgumentError;
    const file = std.fs.cwd().createFile(file_name.block.data(), .{ .read = true }) catch {
        return error.GenericError;
    };
    defer file.close();
    const len = vm.mem.heap;
    const total = len + magic.len;
    file.setEndPos(total) catch {
        return error.GenericError;
    };

    const data = std.os.mmap(null, total, std.os.PROT.WRITE, std.os.MAP.SHARED, file.handle, 0) catch {
        return error.GenericError;
    };
    defer std.os.munmap(data);
    for (magic, data[0..magic.len]) |m, *d| d.* = m;

    var dest = wabi.mem.Mem{
        .space = data,
        .heap = magic.len,
        .len = @intCast(data.len),
        .reserve = 0,
        // we can use another allocator ...
        .allocator = allocator,
        .intern_table = wabi.mem.Mem.HashMap.init(allocator),
    };
    defer dest.intern_table.deinit();
    var orig = vm.mem;

    var ctx = try wabi.copy.Copy.init(&orig, &dest, allocator);
    defer ctx.deinit();
    // todo: set the resulting offset somewhere
    _ = try ctx.one(expr);

    file.setEndPos(dest.heap) catch {
        return error.GenericError;
    };
    vm.ctrl = vm.well_known.true_kw;
    vm.cont = call_cont.pop(&vm.mem);
}

fn slurpExpr(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const allocator = vm.mem.allocator;
    const file_name = args.get(&vm.mem, 0);

    if (file_name.tag() != .BBlk) return error.ArgumentError;
    const file = std.fs.cwd().openFile(file_name.block.data(), .{ .mode = .read_only }) catch {
        std.debug.print("failed to open the file", .{});
        return error.GenericError;
    };
    defer file.close();
    const total = file.getEndPos() catch {
        std.debug.print("failed to get the end pos", .{});
        return error.GenericError;
    };
    const data = std.os.mmap(null, total, std.os.PROT.READ, std.os.MAP.SHARED, file.handle, 0) catch {
        std.debug.print("failed mmap the file", .{});
        return error.GenericError;
    };
    defer std.os.munmap(data);

    var orig = wabi.mem.Mem{
        .space = data,
        .len = @intCast(data.len),
        .reserve = 0,
        .heap = magic.len,
        // we can use another allocator ...
        .allocator = allocator,
        // do we need it?
        .intern_table = wabi.mem.Mem.HashMap.init(allocator),
    };
    defer orig.intern_table.deinit();

    var ctx = try wabi.copy.Copy.init(&orig, &vm.mem, allocator);
    defer ctx.deinit();
    const orig_expr = orig.fromOffset(Val.Ptr, magic.len);
    // todo: set the resulting offset somewhere
    const dest_expr = try ctx.one(orig_expr);
    vm.cont = call_cont.pop(&vm.mem);
    vm.ctrl = dest_expr;
}

fn readVal(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    //todo: use IO handles...
    if (args.len != 0) return error.ArityError;
    const in = std.io.getStdIn().reader();
    var val_reader = vm.reader(in);
    vm.ctrl = if (try val_reader.read()) |exp| exp else vm.well_known.nil;
    vm.cont = call_cont.pop(&vm.mem);
}

fn printVal(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var out = std.io.getStdOut();
    var byte_writer = out.writer();
    var val_writer = vm.writer(byte_writer);
    for (args.refs(), 0..) |*ref, j| {
        const val = ref.get(&vm.mem);
        val_writer.writeVal(val) catch return error.GenericError;
        if (j + 1 < args.len)
            _ = byte_writer.write(" ") catch return error.GenericError;
    }
    vm.ctrl = vm.well_known.nil;
    vm.cont = call_cont.pop(&vm.mem);
}

fn writeBin(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    var out = std.io.getStdOut();
    var byte_writer = out.writer();
    for (args.refs()) |*ref| {
        const val = ref.get(&vm.mem);
        if (val.tag() != .BBlk) return error.ArgumentError;
        _ = byte_writer.write(val.block.data()) catch return error.GenericError;
    }
    vm.ctrl = vm.well_known.true_kw;
    vm.cont = call_cont.pop(&vm.mem);
}

fn readString(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    const mem = &vm.mem;
    if (args.len != 1) return error.ArityError;
    const str = args.get(mem, 0);
    if (str.tag() != .BBlk) return error.ArgumentError;

    const data = str.block.data();
    var stream = std.io.fixedBufferStream(data);
    const reader = stream.reader();
    var read = wabi.read.valueReader(reader, mem);
    vm.ctrl = try read.readVal();
    vm.cont = call_cont.pop(&vm.mem);
}

fn onError(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len == 0) {
        const callback = if (vm.on_error) |oe| oe else vm.well_known.nil;
        vm.ctrl = callback;
        vm.cont = call_cont.pop(&vm.mem);
        return;
    }
    if (args.len != 1) return error.ArityError;
    const callback = args.get(&vm.mem, 0);
    if (Val.isEqual(callback, vm.well_known.nil, &vm.mem)) {
        vm.on_error = null;
        vm.ctrl = vm.well_known.nil;
        vm.cont = call_cont.pop(&vm.mem);
        return;
    }
    _ = switch (callback.tag()) {
        .BApp, .BOpr, .DApp, .DOpr, .Kont => .{},
        else => {
            return error.ArgumentError;
        },
    };

    vm.on_error = callback;
    vm.ctrl = callback;
    vm.cont = call_cont.pop(&vm.mem);
}

fn pEval(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (wabi.sys.current) |*sys| {
        if (args.len < 2) return error.ArityError;

        const env = args.get(&vm.mem, 0);
        if (env.tag() != .Env) return error.ArgumentError;
        //todo: move this sub into enqueueEval (do not consume memory)?
        const exps = try args.sub(&vm.mem, 1, args.len);
        sys.enqueueEval(&vm.mem, &env.env, exps, .{
            .mem_size = @intCast(vm.mem.space.len),
            .reserve = @intCast(vm.mem.reserve),
        }) catch {
            return error.GenericError;
        };
        vm.cont = call_cont.pop(&vm.mem);
        vm.ctrl = vm.well_known.true_kw;
    } else return error.GenericError;
}

fn memFree(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len > 0) return error.ArityError;

    const free = try Val.from(&vm.mem, wabi.num.Fix, @as(wabi.types.SInt, @intCast(vm.mem.len - vm.mem.heap)));
    vm.ctrl = free;
    vm.cont = call_cont.pop(&vm.mem);
}

fn memFreeSet(
    vm: *Vm,
    args: VectorList.Ptr,
    call_cont: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const new_free = args.get(&vm.mem, 0);
    if (new_free.tag() != .Fix) return error.ArgumentError;
    const free = new_free.fix.val;
    if (free < 0) return error.ArgumentError;
    vm.mem.update(.{
        .mem_size = @intCast(vm.mem.heap + vm.mem.reserve + @as(u59, @intCast(free))),
        .reserve = @intCast(vm.mem.reserve),
    }) catch return error.GenericError;
    vm.ctrl = new_free;
    vm.cont = call_cont.pop(&vm.mem);
}

fn sysExit(
    vm: *Vm,
    args: VectorList.Ptr,
    _: Cont.Ptr,
) Error!void {
    if (args.len != 1) return error.ArityError;
    const code_val = args.get(&vm.mem, 0);
    if (code_val.tag() != .Fix) return error.ArgumentError;
    const code = code_val.fix.val;
    if (code >= 256 or code < 0) return error.ArgumentError;
    std.os.exit(@intCast(code));
}

// todo: split (left and right) and (list-set! as j v)
pub const namedBuiltins = .{
    .{ "+", sum, .BApp },
    .{ "-", sub, .BApp },
    .{ "*", mul, .BApp },
    .{ "/", div, .BApp },
    .{ "left", left, .BApp },
    .{ "popl", popLeft, .BApp },
    .{ "list*", listStar, .BApp },
    .{ "right", right, .BApp },
    .{ "popr", popRight, .BApp },
    .{ "*list", starList, .BApp },
    .{ "conc", concatList, .BApp },
    .{ "len", length, .BApp },
    .{ "def", define, .BOpr },
    .{ "if", cond, .BOpr },
    .{ "do", do, .BOpr },
    .{ "fn", funct, .BOpr },
    .{ "fx", fexpr, .BOpr },
    .{ "wrap", wrap, .BApp },
    .{ "unwrap", unwrap, .BApp },
    .{ "eval", eval, .BApp },
    .{ "fx?", mkTest(isOperative), .BApp },
    .{ "fn?", mkTest(isApplicative), .BApp },
    .{ "builtin?", mkTest(isBuiltin), .BApp },
    .{ "derived?", mkTest(isDerived), .BApp },
    .{ "num?", mkTest(isNumber), .BApp },
    .{ "bin?", mkTest(isBinary), .BApp },
    .{ "sym?", mkTest(isSymbol), .BApp },
    .{ "kwd?", mkTest(isKeyword), .BApp },
    .{ "env?", mkTest(isEnv), .BApp },
    .{ "list?", mkTest(isList), .BApp },
    .{ "name", nameOf, .BApp },
    .{ "=", equal, .BApp },
    .{ "prompt", markOper, .BOpr },
    .{ "control", abortOper, .BOpr },
    .{ "==", numEqual, .BApp },
    .{ ">", numGt, .BApp },
    .{ "<", numLt, .BApp },
    .{ ">=", numGtEq, .BApp },
    .{ "<=", numLtEq, .BApp },
    .{ "clock", clock, .BOpr },
    .{ "pr", printVal, .BApp },
    .{ "rd", readVal, .BApp },
    .{ "load", load, .BApp },
    .{ "ext", envExtend, .BApp },
    .{ "kwd", kwdCast, .BApp },
    .{ "sym", symCast, .BApp },
    .{ "spit", spitExpr, .BApp },
    .{ "slurp", slurpExpr, .BApp },
    .{ "wrt", writeBin, .BApp },
    .{ "rd-str", readString, .BApp },
    .{ "on-error", onError, .BApp },
    // todo: convert into pdo
    .{ "peval", pEval, .BApp },
    .{ "mem/free", memFree, .BOpr },
    .{ "mem/free-set", memFreeSet, .BOpr },
    .{ "exit", sysExit, .BApp },
    //    .{ "set!", envSet, .BOpr },
};

pub const builtins = blk: {
    const len = namedBuiltins.len;
    var res: [len]Fun = undefined;
    for (namedBuiltins, 0..) |pair, j| {
        res[j] = pair[1];
    }
    break :blk res;
};

pub const builtinNames = blk: {
    const len = namedBuiltins.len;
    var res: [len][]const u8 = undefined;
    for (namedBuiltins, 0..) |pair, j| {
        _ = .{ j, pair };
        const n = pair[0];
        res[j] = n[0..];
    }
    break :blk res;
};

pub const builtinTags = blk: {
    const len = namedBuiltins.len;
    var res: [len]Tag = undefined;
    for (namedBuiltins, 0..) |pair, j|
        res[j] = pair[2];

    break :blk res;
};

// pub const builtinNames = blk: {
//     const len = namedBuiltins.len;
//     var res: [len][]u8 = undefined;
//     inline for (namedBuiltins, 0..) |pair, j| {
//         const name = pair[0];
//         res[j] = name[0..name.len];
//     }
//     break :blk res;
// };

pub fn env0(vm: *Vm) !void {
    const env = try Env.root(&vm.mem, 8);
    for (builtinNames, builtinTags, 0..) |name, tag, j| {
        // const sym = try vm.intern(Sym, name);
        switch (tag) {
            inline .BOpr, .BApp => |t0| {
                const blk = try vm.mem.intern(wabi.bin.Block, @constCast(name));
                const sym = try Sym.from(&vm.mem, blk);
                const btfun = try wabi.comb.Builtin.from(&vm.mem, @as(wabi.comb.Builtin.Uid, @intCast(j)), t0);
                try env.def(&vm.mem, sym, Val.cast(btfun));
            },
            else => unreachable,
        }
        _ = .{ name, tag, j };
    }
    vm.env = env;
}

const condOper = blk: {
    for (namedBuiltins, 0..) |pair, j| {
        if (std.mem.eql(u8, pair[0], "if")) break :blk j;
    }
};

const doOper = blk: {
    for (namedBuiltins, 0..) |pair, j| {
        if (std.mem.eql(u8, pair[0], "do")) break :blk j;
    }
};

test "builtins exists" {
    _ = builtins;
    _ = builtinNames;
    _ = builtinTags;
}

const vmConfig = .{
    .allocator = std.testing.allocator,
    .mem_size = 512 * 1_024,
};

test "populate" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit();
    try env0(&vm);
    try std.testing.expect(vm.env.len > 1);
}

test "mul empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(*)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "1"));
}

test "multipy one" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 9)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "9"));
}

test "multipy many" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 3 4 7)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "84"));
}

test "list* of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(list* ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "()"));
}

test "list* with some elements" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(list* 1 2 3 ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(1 2 3)"));
}

test "left of :nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(left :nil)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}
test "left of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(left ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "left of non empty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(left ((fn xs xs) :fst 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":fst"));
}

test "popl of nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popl :nil)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}
test "popl of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popl ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "popl of non empty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popl ((fn xs xs) :fst 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(2 3)"));
}

test "*list of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(*list ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "()"));
}

test "*list with some elements" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(*list () 1 2 3)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(1 2 3)"));
}

test "right of nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(right :nil)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "right of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(right ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "right of non empty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(right ((fn xs xs) :fst 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "3"));
}

test "popr of nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popr :nil)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "popr of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popr ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "popr of non empty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(popr ((fn xs xs) :fst 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(:fst 2)"));
}

test "len of nothing" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(len)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "len of empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(len ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "len of nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(len :nil)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "len of nonempty list" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(len ((fn xs xs) 1 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "3"));
}

test "variadic len" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(len ((fn xs xs) 1 2 3) ((fn xs xs) :a :b))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "5"));
}

test "if empty" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "if singleton" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if 33)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "33"));
}

test "if singleton non selfeval" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (+ 30 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "33"));
}

test "if true" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :t 13)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "13"));
}

test "if true not selfeval" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (= 1 1) (+ 10 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "13"));
}

test "if false" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :f 13)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "if false not selfeval" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (= 0 1) (+ 10 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "if nil" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :nil 13)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "if then else" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :t 13 37)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "13"));
}

test "if then else not selfeval " {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (= 10 10) (+ 10 3) (+ 30 7))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "13"));
}

test "if then else (else)" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :f 13 37)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "37"));
}

test "if then else (else) not selfveal" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (= 0 1) (+ 10 3) (+ 30 7))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "37"));
}

test "if then else (multiple even)" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :f 13 :f 37 :else 0)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}
test "if then else (multiple even) not selfeval" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if (= 0 1) (+ 10 3) (= 1 0) (+ 30 7) :else 0)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "0"));
}

test "if then else (odds)" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :f 13 :f 37 :else)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":else"));
}

test "if whenis selfeval but not then" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(if :x (+ 10 13))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "23"));
}

test "empty (do)" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":nil"));
}

test "singleton do" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (+ 7 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "10"));
}
test "longer do" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (+ 7 3) :second (- 37 11))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "26"));
}

test "fn too short" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  100;
    var out_buf: [1024]u8 = undefined;
    try std.testing.expectError(
        error.ArityError,
        vm.evalString("(fn)", out_buf[0..1024]),
    );
    try std.testing.expectError(
        error.ArityError,
        vm.evalString("(fn (a b))", out_buf[0..1024]),
    );
}

test "fn single expression" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(fn a 10)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fn a 10]"));
}

test "fn" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(fn (a b) 10 20)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fn (a b) 10 20]"));
}

test "fx too short" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  100;
    var out_buf: [1024]u8 = undefined;
    try std.testing.expectError(
        error.ArityError,
        vm.evalString("(fx)", out_buf[0..1024]),
    );
    try std.testing.expectError(
        error.ArityError,
        vm.evalString("(fx e)", out_buf[0..1024]),
    );
    try std.testing.expectError(
        error.ArityError,
        vm.evalString("(fx e (a b))", out_buf[0..1024]),
    );
}

test "fx env name is not a symbol" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  100;
    var out_buf: [1024]u8 = undefined;
    try std.testing.expectError(
        error.ArgumentError,
        vm.evalString("(fx :nil (a b) a)", out_buf[0..1024]),
    );
}

test "fx" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(fx e (a b) 10 20)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fx e (a b) 10 20]"));
}

test "wrap" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(wrap (fx e a a))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fn a a]"));
}

test "wrap twice" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(wrap (wrap (fx e a a)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fn a a]"));
}

test "unwrap" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(unwrap (fn a a))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fx _ a a]"));
}

test "unwrap twice" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(unwrap (unwrap (fn a a)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "[fx _ a a]"));
}

test "eval multiple" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (def opr (fx e (a b c) (eval e a b))) (opr 1 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "2"));
}

test "predicate?" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    var res = try vm.evalString("(fx?)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
    res = try vm.evalString("(fx? (fx _ _ 10))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
    res = try vm.evalString("(fx? (fx _ _ 10) if)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
    res = try vm.evalString("(fx? (fx _ _ 10) if +)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":f"));
}

test "name" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(name \"foo\" :bar)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "\"foobar\""));
}

test "trivial equality" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    var res = try vm.evalString("(=)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
    res = try vm.evalString("(= :foo)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
}

test "equality not" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= :x 10)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":f"));
}

test "equality" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= :x :x)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
}

test "binary equality not" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= \"foo\" \"bar\")", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":f"));
}

test "binary equality yes" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= \"foo\" \"foo\")", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
}

test "list equality not" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= ((fn xs xs) 1 2 3) ((fn xs xs) 1 2 4))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":f"));
}

test "list equality yes" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= ((fn xs xs) 1 2 3) ((fn xs xs) 1 2 (+ 1 2)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
}

test "combiner equality yes" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (def x (fn (a) (+ a 1))) (def y x) (= y x))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":t"));
}
test "combiner equality not" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  50;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(= (fn (x) (+ x 1)) (fn (x) (+ x 1)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":f"));
}

test "prompt" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (prompt :a :b :c) :d)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":d"));
}

test "abort inside a do" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(do (prompt :a (control :a k 10) :c) :d)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":d"));
}

test "abort expr" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(prompt :a (control :a _ :x) :c)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, ":x"));
}

test "abort inside another expression" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 2 (prompt :k (+ 1 (control :k k 5))))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "10"));
}

test "resume aborted" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 2 (prompt :k (+ 1 (control :k k (k 5)))))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "12"));
}

test "resume aborted twice" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 2 (prompt :mark (+ 1 (control :mark k (k (k 5))))))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "14"));
}

test "regression use case" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(prompt :amb (control :amb ret (ret 10)))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "10"));
}

test "concat" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    var res = try vm.evalString("(conc)", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "()"));
    res = try vm.evalString("(conc ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "()"));
    res = try vm.evalString("(conc () ())", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "()"));
    res = try vm.evalString("(conc ((fn xs xs) 1 2 3))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(1 2 3)"));
    res = try vm.evalString("(conc ((fn xs xs) 1 2 3) ((fn xs xs) 4 5 6))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "(1 2 3 4 5 6)"));
}

test "regression test" {
    var vm = try Vm.init(vmConfig);
    defer vm.deinit(); // vm.cpu =  1024;
    var out_buf: [1024]u8 = undefined;
    const res = try vm.evalString("(* 2 (prompt :x (prompt :y (+ 1 (control :y ky (+ 2 (control :x kx (ky (kx 3)))))))))", out_buf[0..1024]);
    try testing.expect(std.mem.eql(u8, res, "12"));
}
