const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const StdBitReader = std.io.BitReader;
const FixedBufferStream = std.io.FixedBufferStream;
const NonSentinelSpan = std.io.NonSentinelSpan;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;

const BUFMAX: usize = 2048;

const BitReader = StdBitReader(.Big, FixedBufferStream([]u8).Reader);

pub fn main() !void {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    const stdin_stream = std.io.getStdIn().reader();
    const line = try stdin_stream.readUntilDelimiterOrEofAlloc(gpa.allocator(), '\n', BUFMAX);
    const hex_buf = line.?;
    defer gpa.allocator().free(hex_buf);

    var bits = ArrayList(u8).init(gpa.allocator());
    defer bits.deinit();
    try bits.resize(@divExact(hex_buf.len, 2));
    _ = try std.fmt.hexToBytes(bits.items, hex_buf);

    var bits_fixed_buf = std.io.fixedBufferStream(bits.items);
    var bit_reader = std.io.bitReader(.Big, bits_fixed_buf.reader());

    var packets = ArrayList(Packet).init(gpa.allocator());
    defer packets.deinit();
    try parse(&bit_reader, &packets);

    var sum_ver: usize = 0;
    for (packets.items) |packet| sum_ver += packet.ver;
    std.debug.print("VersionSum: {}\n", .{sum_ver});

    const eval = evaluate(&packets);
    std.debug.print("{}\n", .{eval});
}

const ParseError = BitReader.Error || error{ InvalidPacket, OutOfMemory };

const Packet = struct {
    ver: u3,
    typ: u3,
    val: u64,
};

const SubpacketLen = union(enum) {
    mode0: u15,
    mode1: u11,
};

fn parse(bit_reader: *BitReader, packets: *ArrayList(Packet)) ParseError!void {
    var bits_read: usize = undefined;
    while (parsePacket(bit_reader, packets, &bits_read)) {} else |err| {
        // return err;
        switch (err) {
            error.InvalidPacket => {},
            else => return err,
        }
    }
}

fn parsePacket(bit_reader: *BitReader, packets: *ArrayList(Packet), bits_read: *usize) ParseError!void {
    bits_read.* = 0;
    var nbits: usize = undefined;

    const ver = try bit_reader.readBits(u3, 3, &nbits);
    if (nbits != 3) return ParseError.InvalidPacket;
    bits_read.* += nbits;

    const typ = try bit_reader.readBits(u3, 3, &nbits);
    if (nbits != 3) return ParseError.InvalidPacket;
    bits_read.* += nbits;

    if (typ == 4) {
        const lit = try parseLiteral(bit_reader, &nbits);
        bits_read.* += nbits;
        try packets.append(Packet{ .ver = ver, .typ = typ, .val = lit });
    } else {
        const len = try parseSubpacketLen(bit_reader, &nbits);
        bits_read.* += nbits;
        try packets.append(Packet{ .ver = ver, .typ = typ, .val = undefined });

        const packet_idx = packets.items.len - 1;
        var subpackets: u64 = 0;

        switch (len) {
            .mode0 => |n| {
                var m: usize = 0;
                while (m < n) {
                    try parsePacket(bit_reader, packets, &nbits);
                    bits_read.* += nbits;
                    m += nbits;
                    subpackets += 1;
                }
            },
            .mode1 => |n| {
                var i: usize = 0;
                while (i < n) : (i += 1) {
                    try parsePacket(bit_reader, packets, &nbits);
                    bits_read.* += nbits;
                }
                subpackets = n;
            },
        }
        // correct number of subpackets
        packets.items[packet_idx].val = subpackets;
    }
}

fn parseLiteral(bit_reader: *BitReader, bits_read: *usize) ParseError!u64 {
    bits_read.* = 0;
    var nbits: usize = undefined;

    var lit: u64 = 0;
    while (true) {
        var n: u5 = try bit_reader.readBits(u5, 5, &nbits);
        if (nbits != 5) return ParseError.InvalidPacket;
        bits_read.* += nbits;

        lit = (lit << 4) | (n & 0b01111);
        if (n >> 4 == 0) break;
    }
    return lit;
}

fn parseSubpacketLen(bit_reader: *BitReader, bits_read: *usize) ParseError!SubpacketLen {
    bits_read.* = 0;
    var nbits: usize = undefined;

    const flag = try bit_reader.readBits(u1, 1, &nbits);
    if (nbits != 1) return ParseError.InvalidPacket;
    bits_read.* += nbits;

    switch (flag) {
        0 => {
            const n = try bit_reader.readBits(u15, 15, &nbits);
            if (nbits != 15) return ParseError.InvalidPacket;
            bits_read.* += nbits;
            return SubpacketLen{ .mode0 = n };
        },
        1 => {
            const n = try bit_reader.readBits(u11, 11, &nbits);
            if (nbits != 11) return ParseError.InvalidPacket;
            bits_read.* += nbits;
            return SubpacketLen{ .mode1 = n };
        },
    }
}

fn evaluate(packets: *const ArrayList(Packet)) !u64 {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var stack = ArrayList(u64).init(gpa.allocator());
    defer stack.deinit();

    var i = packets.items.len;
    while (i > 0) : (i -= 1) {
        const packet = packets.items[i - 1];
        printPacket(&packet);
        switch (packet.typ) {
            0 => { // sum
                var accumulator: u64 = 0;
                var j: u64 = 0;
                while (j < packet.val) : (j += 1) accumulator += stack.pop();
                try stack.append(accumulator);
            },
            1 => { // product
                var accumulator: u64 = 1;
                var j: u64 = 0;
                while (j < packet.val) : (j += 1) accumulator *= stack.pop();
                try stack.append(accumulator);
            },
            2 => { // min
                var min: u64 = std.math.maxInt(u64);
                var j: u64 = 0;
                while (j < packet.val) : (j += 1) min = std.math.min(min, stack.pop());
                try stack.append(min);
            },
            3 => { // max
                var max: u64 = 0;
                var j: u64 = 0;
                while (j < packet.val) : (j += 1) max = std.math.max(max, stack.pop());
                try stack.append(max);
            },
            4 => try stack.append(packet.val), // push val
            5 => { // lt
                const v2 = stack.pop();
                const v1 = stack.pop();
                try stack.append(if (v1 < v2) 1 else 0);
            },
            6 => { // gt
                const v2 = stack.pop();
                const v1 = stack.pop();
                try stack.append(if (v1 > v2) 1 else 0);
            },
            7 => { // eq
                const v2 = stack.pop();
                const v1 = stack.pop();
                try stack.append(if (v1 == v2) 1 else 0);
            },
        }
        printStack(&stack);
    }

    return stack.pop();
}

fn printStack(stack: *const ArrayList(u64)) void {
    var count: usize = 0;
    std.debug.print("\t", .{});
    while (count < stack.items.len) : (count += 1) {
        const v = stack.items[stack.items.len - count - 1];
        std.debug.print("[ {} ]", .{v});
        if (count != 0 and count % 5 == 0) std.debug.print("\n\t", .{});
    }
    std.debug.print("\n", .{});
}

fn printPacket(packet: *const Packet) void {
    const cmd = switch (packet.typ) {
        0 => "SUM",
        1 => "PROD",
        2 => "MIN",
        3 => "MAX",
        4 => "PUSH",
        5 => "LT",
        6 => "GT",
        7 => "EQ",
    };
    std.debug.print("{s} {d}\n", .{ cmd, packet.val });
}
