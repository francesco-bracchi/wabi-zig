const std = @import("std");
const wabi = @import("wabi.zig");
const Vm = wabi.vm.Vm;
const Val = wabi.val.Val;

pub fn main() !void {
    var vm = try Vm.init(.{
        .allocator = std.heap.page_allocator,
        .mem_size = 1 << 32 - 10,
    });
    defer vm.deinit();
    try wabi.builtin.env0(&vm);
    const std_in = std.io.getStdIn().reader();
    var std_out = std.io.getStdOut().writer();
    var val_reader = vm.reader(std_in);
    var val_writer = vm.writer(std_out);
    const env0 = vm.env;
    while (true) : (vm.env = env0) {
        try std_out.writeAll("wg> ");
        const ctrl = val_reader.readVal() catch |e| {
            if (e == error.ReadError) break;
            return e;
        };
        const cont = try wabi.cont.Cont.push(vm.cont, &vm.mem, wabi.cont.Eval, .{
            .env = vm.env,
        });
        vm.ctrl = ctrl;
        vm.cont = cont;
        vm.run() catch |e| {
            try std_out.print("{}\n", .{e});
            vm.printVal(vm.ctrl);
            try std_out.print("\n", .{});
            try vm.printCont();
            return;
        };
        try val_writer.writeVal(vm.ctrl);
        try std_out.writeAll("\n");
    }
}
