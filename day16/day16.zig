const std = @import("std");

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;

const BUFMAX: usize = 2048;

pub fn main() !void {
    const stdin_stream = std.io.getStdIn().reader();
    var buf: [BUFMAX]u8 = undefined;
    const line = try stdin_stream.readUntilDelimiterOrEof(&buf, '\n');
    const hex = line.?;

    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var bin = ArrayList(u8).init(&gpa.allocator);
    defer bin.deinit();

    try bin.ensureCapacity(hex.len * 4);
    for (hex) |h| try bin.appendSlice(try toNibble(h));

    std.debug.print("{s}\n", .{bin.items});
    std.debug.print("Bits len: {d}\n", .{bin.items.len});

    var packets = ArrayList(Packet).init(&gpa.allocator);
    defer packets.deinit();

    const nbits = try parse(bin.items, &packets);
    std.debug.print("Bits read: {}\n", .{nbits});

    var sum_ver: usize = 0;
    for (packets.items) |packet| sum_ver += packet.ver;
    std.debug.print("VersionSum: {}\n", .{sum_ver});

    const eval = evaluate(&packets);
    std.debug.print("{}\n", .{eval});
}

fn evaluate(packets: *const ArrayList(Packet)) !u64 {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var stack = ArrayList(u64).init(&gpa.allocator);
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
            else => return error.InvalidParam,
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
        else => "N/A",
    };
    std.debug.print("{s} {d}\n", .{ cmd, packet.val });
}

const ParseError = error{ InvalidParam, OutOfMemory };

const Packet = struct {
    ver: u64,
    typ: u64,
    val: u64,
};

const SubpacketLen = union(enum) {
    mode0: u64,
    mode1: u64,
};

fn parse(bin: []const u8, packets: *ArrayList(Packet)) ParseError!usize {
    var ptr: usize = 0;
    while (parsePacket(bin[ptr..], packets)) |nbits| {
        if (nbits == 0) break;
        ptr += nbits;
    } else |err| return err;
    return ptr;
}

fn parsePacket(bin: []const u8, packets: *ArrayList(Packet)) ParseError!usize {
    // a 4-bit literal is the smallest possible packet which is 11 bits
    if (bin.len < 11) return 0;

    var ptr: usize = 0;
    const ver = parseVersion(bin, &ptr);
    const typ = parseType(bin, &ptr);

    if (typ == 4) {
        const lit = parseLiteral(bin, &ptr);
        try packets.append(Packet{ .ver = ver, .typ = typ, .val = lit });
    } else {
        const mode = try parseLengthMode(bin, &ptr);
        // we keep a reference to the operator and update it's
        // number of subpackets once we finished parsing them,
        // that's why val is now undefined
        try packets.append(Packet{ .ver = ver, .typ = typ, .val = undefined });
        const packet_idx = packets.items.len - 1;
        var subpackets: u64 = 0;
        switch (mode) {
            .mode0 => |n| {
                var m: usize = 0;
                while (m < n) {
                    const nbits = try parsePacket(bin[ptr + m .. ptr + n], packets);
                    if (nbits == 0) break;
                    m += nbits;
                    subpackets += 1;
                }
                ptr += m;
            },
            .mode1 => |n| {
                var i: usize = 0;
                while (i < n) : (i += 1) {
                    const nbits = try parsePacket(bin[ptr..], packets);
                    if (nbits == 0) break;
                    ptr += nbits;
                }
                subpackets = n;
            },
        }
        // correct number of subpackets
        packets.items[packet_idx].val = subpackets;
    }
    return ptr;
}

fn parseLengthMode(bin: []const u8, ptr: *usize) ParseError!SubpacketLen {
    switch (bin[ptr.*]) {
        '0' => {
            const n = parseBin(bin[ptr.* + 1 .. ptr.* + 16]);
            ptr.* += 16;
            return SubpacketLen{ .mode0 = n };
        },
        '1' => {
            const n = parseBin(bin[ptr.* + 1 .. ptr.* + 12]);
            ptr.* += 12;
            return SubpacketLen{ .mode1 = n };
        },
        else => return error.InvalidParam,
    }
}

fn parseLiteral(bin: []const u8, ptr: *usize) u64 {
    var ptr_tmp = ptr.*;
    while (bin[ptr_tmp] != '0') ptr_tmp += 5;
    ptr_tmp += 5;

    var literal: u64 = 0;
    for (bin[ptr.*..ptr_tmp]) |b, i| {
        if (i % 5 == 0) continue;
        literal *= 2;
        literal += b - '0';
    }

    ptr.* = ptr_tmp;
    return literal;
}

fn parseVersion(bin: []const u8, ptr: *usize) u64 {
    const ver = parseBin(bin[ptr.* .. ptr.* + 3]);
    ptr.* += 3;
    return ver;
}

fn parseType(bin: []const u8, ptr: *usize) u64 {
    const typ = parseBin(bin[ptr.* .. ptr.* + 3]);
    ptr.* += 3;
    return typ;
}

fn parseBin(bin: []const u8) u64 {
    var n: u64 = 0;
    for (bin) |b, i| {
        n *= 2;
        n += b - '0';
    }
    return n;
}

fn toNibble(hex: u8) !*const [4]u8 {
    return switch (hex) {
        '0' => "0000",
        '1' => "0001",
        '2' => "0010",
        '3' => "0011",
        '4' => "0100",
        '5' => "0101",
        '6' => "0110",
        '7' => "0111",
        '8' => "1000",
        '9' => "1001",
        'A' => "1010",
        'B' => "1011",
        'C' => "1100",
        'D' => "1101",
        'E' => "1110",
        'F' => "1111",
        else => error.InvalidChar,
    };
}
