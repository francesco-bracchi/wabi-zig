const std = @import("std");
const wabi = @import("wabi.zig");

const Allocator = std.mem.Allocator;

const Mem = wabi.mem.Mem;
const MemPool = wabi.mem_pool.MemPool;
const Vm = wabi.vm.Vm;
const Copy = wabi.copy.Copy;

const Val = wabi.val.Val;
const Env = wabi.env.Env;
const VectorList = wabi.list.VectorList;
const Block = wabi.bin.Block;

const Cont = wabi.cont.Cont;
const Call = wabi.cont.Call;
const Eval = wabi.cont.Eval;
const Prog = wabi.cont.Prog;

const Mutex = std.Thread.Mutex;
const Queue = std.TailQueue;
const Condition = std.Thread.Condition;
const Thread = std.Thread;

pub const Sys = struct {
    mem_pool: MemPool,
    copy_allocator: Allocator,
    run_queue: Q = .{},
    mutex: Mutex = .{},
    condition: Condition = .{},
    threads: []Thread,
    running: bool = false,

    const Q = Queue(Vm);

    const Self = @This();

    fn mkVm(
        self: *Self,
        orig: *Mem,
        fun: Val.Ptr,
        args: Val.Ptr,
        config: Mem.Config,
    ) !Vm {
        var mem = try self.mem_pool.acquire(config);
        std.debug.print("mem: heap:{} avail:{} total:{} reserve: {}\n", .{ mem.heap, mem.len, mem.space.len, mem.reserve });
        var ctx = try Copy.init(orig, &mem, self.copy_allocator);
        defer ctx.deinit();
        ctx.reset();
        const next_fun = try ctx.add(fun);
        const next_args = try ctx.add(args);
        try ctx.run();
        const next_env = try Env.root(&mem, 0);
        const next_cont = try Cont.push(null, &mem, Call, .{
            .combiner = next_fun,
            .env = next_env,
        });
        var vm = try Vm.initFromMem(mem);
        vm.ctrl = @ptrCast(next_args);
        vm.cont = next_cont;
        vm.env = next_env;
        return vm;
    }
    pub fn mkRootVm(
        self: *Self,
        orig: *Mem,
        fun: Val.Ptr,
        args: anytype,
        config: Mem.Config,
    ) !Vm {
        var mem = try self.mem_pool.acquire(config);
        var ctx = try Copy.init(orig, &mem, self.copy_allocator);
        defer ctx.deinit();
        const next_fun = try ctx.one(fun);

        const next_args = args: {
            const list = try VectorList.create(&mem, args.len);
            for (args, list.refs()) |arg, *ref| {
                // std.debug.print("create: {s}\n", .{arg});
                const bin = try Val.from(&mem, Block, arg);
                ref.set(&mem, bin);
            }
            break :args list;
        };

        const next_env = try Env.root(&mem, 0);
        const next_cont = try Cont.push(null, &mem, Call, .{
            .combiner = next_fun,
            .env = next_env,
        });
        var vm = try Vm.initFromMem(mem);
        vm.ctrl = @ptrCast(next_args);
        vm.cont = next_cont;
        vm.env = next_env;
        return vm;
    }

    pub fn enqueueEval(
        self: *Self,
        orig: *Mem,
        env: Env.Ptr,
        exps: VectorList.Ptr,
        config: Mem.Config,
    ) !void {
        if (exps.len < 1) return error.GenericError;

        self.mutex.lock();
        defer self.mutex.unlock();
        var mem = try self.mem_pool.acquire(config);
        var ctx = try Copy.init(orig, &mem, self.copy_allocator);
        defer ctx.deinit();

        ctx.reset();
        const next_exps = try ctx.add(exps);
        const next_env = try ctx.add(env);
        try ctx.run();

        const first_exp = next_exps.get(&mem, 0);
        var cont: ?Cont.Ptr = null;

        if (exps.len > 1) cont = try Cont.push(cont, &mem, Prog, .{
            .env = next_env,
            .exs = next_exps,
            .cur = 0,
        });
        cont = try Cont.push(cont, &mem, wabi.cont.Eval, .{});

        var vm = try Vm.initFromMem(mem);
        vm.ctrl = first_exp;
        vm.env = next_env;
        vm.cont = cont;

        var node = try self.copy_allocator.create(Q.Node);
        node.data = vm;
        self.run_queue.append(node);
        self.condition.signal();
    }

    pub fn enqueueVm(
        self: *Self,
        vm: Vm,
    ) !void {
        // std.debug.print("enqueueVm\n", .{});
        self.mutex.lock();
        defer self.mutex.unlock();
        var node = try self.copy_allocator.create(Q.Node);
        node.data = vm;
        self.run_queue.append(node);
        // std.debug.print("enqueueVm signalling\n", .{});
        self.condition.signal();
    }

    fn maybeNextVm(self: *Self) ?Vm {
        self.mutex.lock();
        defer self.mutex.unlock();
        // std.debug.print("waiting..\n", .{});
        self.condition.timedWait(&self.mutex, 1000 * 1000 * 1000) catch {
            // std.debug.print("timed out {} (len: {})\n", .{ Thread.getCurrentId(), self.run_queue.len });
            return null;
        };
        // std.debug.print("woken..\n", .{});
        if (self.run_queue.pop()) |node| {
            defer self.copy_allocator.destroy(node);
            // std.debug.print("a VM found\n", .{});
            return node.data;
        } else {
            // std.debug.print("NO VM found:\n", .{});
            return null;
        }
    }

    fn nextVm(self: *Self) ?Vm {
        while (self.running) if (self.maybeNextVm()) |vm| return vm;
        return null;
    }

    pub fn run(self: *Self) void {
        // std.debug.print("starting thread {}\n", .{Thread.getCurrentId()});
        while (self.nextVm()) |vm0| {
            // std.debug.print("found vm to run {}\n", .{Thread.getCurrentId()});
            var vm = vm0;
            vm.run() catch |e| {
                std.debug.print("error in sys.run: {any}\n", .{e});
                return;
            };
        }
    }

    pub fn init(allocator: Allocator, config: struct { num_threads: usize = 4 }) !Self {
        const mem_pool = try MemPool.init(
            allocator,
            allocator,
            .{
                .min_len = 2,
                .max_len = null,
            },
        );
        const threads: []Thread = try allocator.alloc(Thread, config.num_threads);

        return .{
            .mem_pool = mem_pool,
            .copy_allocator = allocator,
            .threads = threads,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stop();
        self.mem_pool.deinit();
        self.copy_allocator.free(self.threads);
    }

    pub fn start(self: *Self) !void {
        if (!self.running) {
            self.running = true;
            for (self.threads) |*t|
                t.* = try Thread.spawn(.{}, run, .{self});
            self.condition.broadcast();
        }
    }

    pub fn stop(self: *Self) void {
        self.running = false;
    }

    pub fn wait(self: *Self) void {
        if (self.running) for (self.threads) |*t| t.join();
    }
};

pub var current: ?Sys = null;

pub fn start() !void {
    if (current) |*sys| {
        try sys.start();
    } else return error.UndefinedCurrentSystem;
}

pub fn stop() !void {
    if (current) |*sys| {
        try sys.stop();
    } else return error.UndefinedCurrentSystem;
}

pub fn init(allocator: Allocator, config: anytype) !void {
    const sys = try Sys.init(allocator, config);
    current = sys;
}

pub fn wait() !void {
    if (current) |*sys| {
        sys.wait();
    } else return error.UndefinedCurrentSystem;
}

pub fn deinit() void {
    if (current) |*sys| {
        sys.deinit();
    }
}

test "init" {
    try init(std.testing.allocator, .{});
    try start();
    deinit();
}
