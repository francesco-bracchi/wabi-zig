const std = @import("std");
const wabi = @import("wabi.zig");

const Mem = wabi.mem.Mem;
const Size = u32;
const Allocator = std.mem.Allocator;
const Mutex = std.Thread.Mutex;
const Queue = std.TailQueue;

pub const MemPool = struct {
    mem_allocator: Allocator,
    free: Q,
    min_len: usize,
    max_len: ?usize,

    used_count: usize = 0,
    len: usize = 0,

    const Q = std.PriorityQueue(Mem, void, byMemSize);

    const Self = @This();

    fn byMemSize(
        _: void,
        a: Mem,
        b: Mem,
    ) std.math.Order {
        return std.math.order(b.space.len, a.space.len);
    }

    pub fn init(
        mem_allocator: Allocator,
        queue_allocator: Allocator,
        opts: struct {
            min_len: usize = 2,
            max_len: ?usize = 16,
            mem_size: Mem.Size = 1024 * 1024,
        },
    ) !Self {
        if (opts.max_len) |opts_max_len| std.debug.assert(opts.min_len <= opts_max_len);
        var free = Q.init(queue_allocator, {});
        try free.ensureTotalCapacity(opts.min_len);

        // todo: in a specific function?
        for (0..opts.min_len) |_| {
            const mem = try Mem.init(mem_allocator, .{ .mem_size = opts.mem_size });
            try free.add(mem);
        }
        return .{
            .mem_allocator = mem_allocator,
            .free = free,
            .used_count = 0,
            .min_len = opts.min_len,
            .max_len = opts.max_len,
            .len = 0,
        };
    }

    pub fn deinit(
        self: *Self,
    ) void {
        var j = self.free.iterator();
        while (j.next()) |mem| Mem.deinit(@constCast(&mem));
        self.free.deinit();
    }

    const RESERVE: Mem.Size = 8 * 1024;

    pub fn acquire(
        self: *Self,
        config: Mem.Config,
    ) !Mem {
        if (self.max_len) |m_len| if (self.used_count >= m_len) {
            return error.MemPoolFull;
        };

        if (self.free.removeOrNull()) |m| {
            // std.debug.print("MEM FOUND {}\n", .{config});
            var mem = m;

            try mem.update(config);
            mem.reset();
            self.used_count += 1;
            return mem;
        } else {
            // std.debug.print("MEM NOT FOUND {}\n", .{config});
            const mem = try Mem.init(self.mem_allocator, config);
            self.used_count += 1;
            return mem;
        }
    }
    pub fn release(self: *Self, m: Mem) void {
        var mem = m;
        if (self.free.len >= self.min_len) {
            // save the smaller one...
            mem.deinit();
        } else {
            self.free.add(mem) catch unreachable;
        }
        self.used_count -= 1;
    }
};

test "init" {
    var pool = try MemPool.init(std.testing.allocator, std.testing.allocator, .{});
    defer pool.deinit();
    try std.testing.expect(pool.free.len == pool.min_len);
    try std.testing.expect(pool.used_count == 0);
}

test "acquire/release reuse" {
    var pool = try MemPool.init(std.testing.allocator, std.testing.allocator, .{});
    defer pool.deinit();
    const size = 1024 * 1024;
    var mem = try pool.acquire(.{ .mem_size = size });
    defer pool.release(mem);
    try std.testing.expect(mem.space.len == size);
    try std.testing.expect(mem.space.len == size);
}

test "acquire/release create and free" {
    var pool = try MemPool.init(std.testing.allocator, std.testing.allocator, .{
        .min_len = 0,
    });
    defer pool.deinit();
    const size = 1024 * 1024;
    var mem = try pool.acquire(.{ .mem_size = size });
    defer pool.release(mem);
    _ = try mem.alloc(16);
    try std.testing.expect(mem.heap == 16);
}

test "mem_pool is full" {
    var pool = try MemPool.init(std.testing.allocator, std.testing.allocator, .{
        .min_len = 0,
        .max_len = 0,
    });
    defer pool.deinit();
    const size = 1024 * 1024;
    try std.testing.expectError(error.MemPoolFull, pool.acquire(.{ .mem_size = size }));
}

test "acquired is the biggest" {
    var pool = try MemPool.init(std.testing.allocator, std.testing.allocator, .{});
    defer pool.deinit();
    const size = 2 * 1024 * 1024;
    const m0 = try pool.acquire(.{ .mem_size = size });
    pool.release(m0);
    const m1 = try pool.acquire(.{ .mem_size = 1024 });
    defer pool.release(m1);
    try std.testing.expect(m0.space.ptr == m1.space.ptr);
    try std.testing.expect(m0.space.len == size);
    try std.testing.expect(m1.space.len == 1024);
}
