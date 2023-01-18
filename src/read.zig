const std = @import("std");
const wabi = @import("wabi.zig");
const Mem = wabi.mem.Mem;
const Val = wabi.val.Val;
const Sym = wabi.sym.Sym;
const Kwd = wabi.kwd.Kwd;
const Block = wabi.bin.Block;
const UInt = wabi.types.UInt;
const SInt = wabi.types.SInt;
const Fix = wabi.num.Fix;
const VectorList = wabi.list.VectorList;
const testing = std.testing;

// todo: use Vm.intern for symbols and keyword

pub fn ValueReader(
    comptime Reader: type,
) type {
    return struct {
        reader: Reader,
        mem: *Mem,
        lookahead: ?u8,

        const buf_size = 65_535;
        const Self = @This();

        const Error = error{
            ReadError,
            MemoryOverflow,
            OutOfMemory,
        };

        fn peek(self: *Self) ?u8 {
            if (self.lookahead) |p| {
                return p;
            } else {
                self.lookahead = self.reader.readByte() catch null;
                return self.lookahead;
            }
        }
        fn pop(self: *Self) ?u8 {
            if (self.lookahead) |p| {
                self.lookahead = null;
                return p;
            } else {
                return self.reader.readByte() catch null;
            }
        }

        fn next(self: *Self) void {
            _ = self.pop();
        }

        fn isWs(ch: u8) bool {
            return switch (ch) {
                ' ', '\t', '\n', '\r' => true,
                else => false,
            };
        }

        fn skipWs(self: *Self) void {
            var cur = self.peek();
            while (cur) |c| : ({
                self.next();
                cur = self.peek();
            })
                if (!isWs(c)) return;
        }

        fn readBin(self: *Self) !Val.Ptr {
            self.next();
            const res = try readBinRest(self);
            return res;
        }

        fn readBinRest(self: *Self) !Val.Ptr {
            var j: usize = 0;
            var buffer: [buf_size]u8 = undefined;
            while (true) : (j += 1) {
                if (self.pop()) |c| {
                    switch (c) {
                        '"' => {
                            break;
                        },
                        '\\' => {
                            if (self.pop()) |x| switch (x) {
                                'n' => {
                                    buffer[j] = '\n';
                                },
                                't' => {
                                    buffer[j] = '\t';
                                },
                                else => {
                                    buffer[j] = x;
                                    continue;
                                },
                            } else {
                                return error.ReadError;
                            }
                        },
                        else => {
                            buffer[j] = c;
                            continue;
                        },
                    }
                } else {
                    return error.ReadError;
                }
            }
            return try Val.from(self.mem, Block, buffer[0..j]);
        }

        fn readSymbol(
            self: *Self,
        ) !Val.Ptr {
            var buffer: [buf_size]u8 = undefined;
            const str = try self.readBinData(&buffer, "");
            const bin = try self.mem.intern(Block, str);
            const sym = try Sym.from(self.mem, bin);
            return Val.cast(sym);
        }

        fn readKeyword(
            self: *Self,
        ) !Val.Ptr {
            self.next();
            var buffer: [buf_size]u8 = undefined;
            const str = try self.readBinData(&buffer, "");
            const blk = try self.mem.intern(Block, str);
            const kwd = try Val.from(self.mem, Kwd, blk);
            return kwd;
        }

        fn readBinData(
            self: *Self,
            buffer: []u8,
            comptime prefix: anytype,
        ) ![]u8 {
            inline for (prefix, 0..) |c, j| buffer[j] = c;
            var j: usize = prefix.len;
            while (true) : (j += 1) {
                if (j >= buf_size) return error.ReadError;
                if (self.peek()) |c| {
                    switch (c) {
                        ' ', '\t', '\n', '\r', '(', ')', '{', '}', '[', ']', ';', ':' => {
                            break;
                        },
                        '\\' => {
                            self.next();
                            if (self.pop()) |e| {
                                buffer[j] = e;
                                continue;
                            } else {
                                return error.ReadError;
                            }
                        },
                        else => {
                            buffer[j] = c;
                            self.next();
                            continue;
                        },
                    }
                } else {
                    break;
                }
            }
            return buffer[0..j];
        }

        fn readList(self: *Self) !Val.Ptr {
            self.next();
            var buffer: [buf_size]Val.Ptr = undefined;
            var j: usize = 0;
            while (j < buf_size) : (j += 1) {
                self.skipWs();
                if (self.peek()) |c| if (c == ')') {
                    self.next();
                    const lst = try Val.from(self.mem, VectorList, buffer[0..j]);
                    return lst;
                };
                const elem = try self.readVal();
                buffer[j] = elem;
            }
            return error.ReadError;
        }

        fn readNum(self: *Self) !Val.Ptr {
            var num: SInt = 0;
            while (self.peek()) |c| switch (c) {
                '0'...'9' => {
                    num = num * 10 + (c - '0');
                    self.next();
                },
                else => {
                    break;
                },
            };
            return try Val.from(self.mem, Fix, num);
        }

        fn withMinusPrefix(self: *Self) !Val.Ptr {
            var buffer: [buf_size]u8 = undefined;
            const str = try self.readBinData(&buffer, "-");
            const blk = try self.mem.intern(Block, str);
            const sym = try Val.from(self.mem, Sym, blk);
            return sym;
        }

        fn readMinusOrNum(self: *Self) !Val.Ptr {
            self.next();
            if (self.peek()) |c| switch (c) {
                '0'...'9' => {
                    const r = try self.readNum();
                    r.*.fix.val = -r.fix.val;
                    return r;
                },
                else => {
                    const sym = try self.withMinusPrefix();
                    return sym;
                },
            } else {
                const sym = try self.withMinusPrefix();
                return sym;
            }
        }

        pub fn readVal(self: *Self) Error!Val.Ptr {
            self.skipWs();
            return if (self.peek()) |c| switch (c) {
                '(' => try self.readList(),
                '0'...'9' => try self.readNum(),
                '-' => try self.readMinusOrNum(),
                '"' => try self.readBin(),
                ':' => try self.readKeyword(),
                else => try self.readSymbol(),
            } else error.ReadError;
        }

        pub fn read(self: *Self) Error!?Val.Ptr {
            self.skipWs();
            return if (self.peek()) |_| try self.readVal() else null;
        }

        pub fn init(reader: Reader, mem: *Mem) Self {
            return .{
                .reader = reader,
                .mem = mem,
                .lookahead = null,
            };
        }
    };
}

pub fn valueReader(
    reader: anytype,
    mem: *Mem,
) ValueReader(@TypeOf(reader)) {
    const VReader = ValueReader(@TypeOf(reader));
    return VReader.init(reader, mem);
}

const memConfig = .{
    .mem_size = 8_192,
};

test "read the same symbol twice" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "(+ (+))";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    _ = try read.readVal();
    try testing.expect(mem.intern_table.count() == 1);
}

test "read peek and pop on empty reader" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expect(read.peek() == null);
    try testing.expect(read.pop() == null);
}

test "read peek and pop on almost empty reader" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "f";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expect(read.peek().? == 'f');
    try testing.expect(read.pop().? == 'f');
    try testing.expect(read.pop() == null);
}

test "read pop and peek on almost empty reader" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "f";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expect(read.pop().? == 'f');
    try testing.expect(read.peek() == null);
}

test "read peek doesn't consume" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "foo";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var j: usize = 0;
    while (j < 3) : (j += 1)
        try testing.expect(read.peek().? == 'f');
}

test "read pop consumes" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "foo";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expect(read.pop().? == 'f');
    try testing.expect(read.peek().? == 'o');
    try testing.expect(read.pop().? == 'o');
}

test "read skip whitespaces" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    read.skipWs();
    try std.testing.expect(true);
}

test "read skip whitespaces to the end" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "  ";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    read.skipWs();
    try std.testing.expect(read.peek() == null);
}

test "read skip whitespaces more" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = " \t\n\rfoo";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    read.skipWs();
    try std.testing.expect(read.peek().? == 'f');
}

test "read bin" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = " \"this is string\"  ";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .BBlk);
    try testing.expect(std.mem.eql(u8, val.block.data(), "this is string"));
}

test "read bin with escape" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "\"\\\"\"";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .BBlk);
    try testing.expect(std.mem.eql(u8, val.block.data(), "\""));
}

test "read never ending bin" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "\"foo";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = read.readVal();
    try testing.expectError(error.ReadError, val);
}

test "read never ending bin with ecape" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = [_]u8{ '"', 'f', '\\' };
    var str = std.io.fixedBufferStream(&buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = read.readVal();
    try testing.expectError(error.ReadError, val);
}

test "read symbol" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = " this-is-a-symbol  ";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .Sym);
    var asBin = val.sym.binOf(&mem);
    try testing.expect(std.mem.eql(u8, asBin.data(), "this-is-a-symbol"));
}

test "read symbol with escape" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "foo\\ bar";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .Sym);
    var asBin = val.sym.binOf(&mem);
    try testing.expect(std.mem.eql(u8, asBin.data(), "foo bar"));
}

test "read keyword" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = ":this-is-a-keyword";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .Kwd);
    var asBin = val.sym.binOf(&mem);
    try testing.expect(std.mem.eql(u8, asBin.data(), "this-is-a-keyword"));
}

test "read fix" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "123";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .Fix);
    try testing.expectEqual(val.fix.val, 123);
}

test "read `-`" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "-";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    var asBin = val.sym.binOf(&mem);
    try testing.expectEqual(val.tag(), .Sym);
    try testing.expect(std.mem.eql(u8, asBin.data(), "-"));
}

test "read negative number" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "-123";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .Fix);
    try testing.expectEqual(val.fix.val, -123);
}

test "read `-` prefixed symbol" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "--123";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    var asBin = val.sym.binOf(&mem);
    try testing.expectEqual(val.tag(), .Sym);
    try testing.expect(std.mem.eql(u8, asBin.data(), "--123"));
}

test "read empty list" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "()";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .LVec);
    try testing.expectEqual(val.list.len, 0);
}

test "read empty list with spaces" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = " (   )  ";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    var val = try read.readVal();
    try testing.expectEqual(val.tag(), .LVec);
    try testing.expectEqual(val.list.len, 0);
}

test "read broken parenthesis" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "(";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expectError(error.ReadError, read.readVal());
}

test "read broken parenthesis with elems and spaces" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "   ( ( foo  )  ";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    try testing.expectError(error.ReadError, read.readVal());
}

test "read tree" {
    var mem = try Mem.init(testing.allocator, memConfig);
    defer mem.deinit();
    var buf = "(def fib (n) (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))";
    var str = std.io.fixedBufferStream(buf);
    var reader = str.reader();
    var read = valueReader(reader, &mem);
    _ = try read.readVal();
    try testing.expect(true);
}
