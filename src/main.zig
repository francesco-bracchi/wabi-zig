const std = @import("std");
const wabi = @import("wabi.zig");
const Vm = wabi.vm.Vm;
const Val = wabi.val.Val;
const VectorList = wabi.list.VectorList;
const Block = wabi.bin.Block;
const Mem = wabi.mem.Mem;

const MEM_SIZE = 1024 * 1024 * 1;

fn memSize(_: anytype) !Mem.Size {
    if (std.os.getenv("WABI_MEM")) |str_val| {
        return try std.fmt.parseInt(Mem.Size, str_val, 10);
    } else {
        return @intCast(MEM_SIZE);
    }
}

fn run(vm: *Vm) void {
    vm.run() catch |e| {
        std.process.exit(switch (e) {
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
        });
    };
}

fn buildZigote(
    file_name: anytype,
    args: anytype,
) !Vm {
    return if (wabi.sys.current) |*sys| vm: {
        const file = try std.fs.cwd().openFile(
            file_name,
            .{ .mode = .read_only },
        );
        defer file.close();
        const size = try file.getEndPos();
        const data = try std.os.mmap(
            null,
            size,
            std.os.PROT.READ,
            std.os.MAP.SHARED,
            file.handle,
            0,
        );
        defer std.os.munmap(data);
        var orig_mem = Mem{
            .space = data,
            .len = @intCast(data.len),
            .reserve = 0,
            .heap = @intCast(data.len),
            .allocator = sys.copy_allocator,
            .intern_table = wabi.mem.Mem.HashMap.init(sys.copy_allocator),
        };
        defer orig_mem.intern_table.deinit();

        const orig_fun = orig_mem.fromOffset(Val.Ptr, 8);
        break :vm try sys.mkRootVm(
            &orig_mem,
            orig_fun,
            args,
            .{
                .mem_size = 16 * 1024 * 1024,
                .reserve = 1 * 1024 * 1024,
            },
        );
    } else return error.GenericError;
}

pub fn main() !void {
    var std_err = std.io.getStdErr().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();

    const args = std.process.argsAlloc(allocator) catch std.process.exit(255);
    errdefer std.process.argsFree(allocator, args);
    if (args.len <= 1) {
        std_err.print("missing file\n", .{}) catch {};
        std.process.exit(255);
    }
    try wabi.sys.init(allocator, .{});
    // defer wabi.sys.deinit();

    // const file_name = args[1];

    // const vm = buildZigote(file_name, args[2..]) catch std.process.exit(255);
    // try wabi.sys.start();
    // if (wabi.sys.current) |*sys| try sys.enqueueVm(vm);
    // std.process.argsFree(allocator, args);
    // try wabi.sys.wait();
}
