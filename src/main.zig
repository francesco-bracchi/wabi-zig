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
        std.os.exit(switch (e) {
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
        var data = try std.os.mmap(
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

    const args = std.process.argsAlloc(allocator) catch std.os.exit(255);
    errdefer std.process.argsFree(allocator, args);
    if (args.len <= 1) {
        std_err.print("missing file\n", .{}) catch {};
        std.os.exit(255);
    }
    try wabi.sys.init(allocator, .{});
    defer wabi.sys.deinit();

    const file_name = args[1];

    var vm = buildZigote(file_name, args[2..]) catch std.os.exit(255);
    try wabi.sys.start();
    if (wabi.sys.current) |*sys| try sys.enqueueVm(vm);
    std.process.argsFree(allocator, args);
    try wabi.sys.wait();
}

// pub fn main() !void {
//     // var std_in = std.io.getStdIn().reader();
//     // var std_out = std.io.getStdOut().writer();
//     var std_err = std.io.getStdErr().writer();

//     var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//     defer _ = gpa.deinit();
//     const allocator = gpa.allocator();

//     const args = try std.process.argsAlloc(allocator);
//     errdefer std.process.argsFree(allocator, args);

//     if (args.len <= 1) {
//         try std_err.print("missing file\n", .{});
//         std.os.exit(1);
//     }
//     const file_name = args[1];
//     const file = try std.fs.cwd().openFile(file_name, .{ .mode = .read_only });
//     errdefer file.close();
//     const size = try file.getEndPos();
//     var data = try std.os.mmap(null, size, std.os.PROT.READ, std.os.MAP.SHARED, file.handle, 0);
//     errdefer std.os.munmap(data);

//     var orig = wabi.mem.Mem{
//         .space = data,
//         .heap = 8, // todo: read magic
//         .allocator = allocator,
//         .intern_table = wabi.mem.Mem.HashMap.init(allocator),
//     };
//     errdefer orig.intern_table.deinit();

//     var vm = try Vm.init(.{
//         .allocator = allocator,
//         .mem_size = @intCast(size * 2),
//     });

//     defer vm.deinit();

//     var ctx = try wabi.copy.Copy.init(&orig, &vm.mem, allocator);
//     errdefer ctx.deinit();

//     const orig_fun = orig.fromOffset(Val.Ptr, 8);
//     const dest_fun = try ctx.one(orig_fun);
//     const empty_env = try wabi.env.Env.root(&vm.mem, 0);
//     const cont = try wabi.cont.Cont.push(null, &vm.mem, wabi.cont.Call, .{
//         .combiner = dest_fun,
//         .env = empty_env,
//     });

//     // var val_reader = vm.reader(std_in);
//     // var val_writer = vm.writer(std_out);

//     // try std_out.print("file: \"{s}\"\n", .{file_name});

//     const val_args = try VectorList.create(&vm.mem, args.len - 2);
//     const val_refs = val_args.refs();
//     for (args[2..], val_refs) |arg, *ref| {
//         const bin = try Val.from(&vm.mem, Block, arg);
//         ref.set(&vm.mem, bin);
//     }

//     // try val_writer.writeVal(Val.cast(val_args));

//     // _ = .{ val_reader, val_writer, args, size };
//     // slurp
//     std.os.munmap(data);
//     file.close();
//     std.process.argsFree(allocator, args);

//     vm.cont = cont;
//     vm.ctrl = Val.cast(val_args);
// }
