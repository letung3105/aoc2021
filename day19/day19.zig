const std = @import("std");
const debug = std.debug;
const fmt = std.fmt;
const heap = std.heap;
const io = std.io;
const math = std.math;
const mem = std.mem;
const sort = std.sort;
const testing = std.testing;

const BUFFER_CAP = 64;

pub fn main() !void {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var scanners = std.ArrayList(Scanner).init(gpa.allocator());
    defer {
        for (scanners.items) |scanner| gpa.allocator().free(scanner.beacons);
        scanners.deinit();
    }

    // a buffer for storing the beacons of a scanner that is being parsed
    var beacons = std.ArrayList(Vec3).init(gpa.allocator());
    defer beacons.deinit();

    var buf: [BUFFER_CAP]u8 = undefined;
    var reader = io.getStdIn().reader();
    while (reader.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        // allocate a scanner when there's a double newline or there's
        // no more data
        if (maybe_line == null or maybe_line.?.len == 0) {
            try scanners.append(.{ .beacons = beacons.toOwnedSlice() });
            if (maybe_line == null) break else continue;
        }
        const line = maybe_line.?;

        // beginning of a new scanner
        if (mem.eql(u8, line[0..3], "---")) continue;

        var line_it = mem.split(u8, line, ",");
        try beacons.append(.{
            .x = try fmt.parseInt(isize, line_it.next().?, 10),
            .y = try fmt.parseInt(isize, line_it.next().?, 10),
            .z = try fmt.parseInt(isize, line_it.next().?, 10),
        });
    } else |err| return err;

    // keep track of the number of time a scanner position can be
    // reconstructed from the beacons
    var overlap_counts = std.AutoHashMap(Vec3, usize).init(gpa.allocator());
    defer overlap_counts.deinit();

    // keep track of all the known scanners
    var known_scanners = try std.DynamicBitSet.initEmpty(gpa.allocator(), scanners.items.len);
    defer known_scanners.deinit();

    // keep track of all the known scanners positions
    var known_scanner_positions = std.ArrayList(Vec3).init(gpa.allocator());
    defer known_scanner_positions.deinit();
    try known_scanner_positions.resize(scanners.items.len);

    // keep track of all the known beacons positions
    var known_beacon_positions = std.AutoHashMap(Vec3, void).init(gpa.allocator());
    defer known_beacon_positions.deinit();

    // we're relying on the first scanner being the origin of or coordinates system,
    // thus we add it and its beacons to the list of known stuffs
    known_scanners.set(0);
    known_scanner_positions.items[0] = .{ .x = 0, .y = 0, .z = 0 };
    for (scanners.items[0].beacons) |beacon| try known_beacon_positions.put(beacon, {});

    // keep reconstructing the beacons and scanners positions until all scanners are known
    reconstruction_loop: while (known_scanners.count() < scanners.items.len) {
        var unknown_scanners = known_scanners.iterator(.{ .kind = .unset });
        // go through all unknown scanners
        while (unknown_scanners.next()) |unknown_scanner_idx| {
            const unknown_scanner = scanners.items[unknown_scanner_idx];
            // go through all possible orientations
            var orientation: usize = 0;
            while (orientation < 24) : (orientation += 1) {
                overlap_counts.clearRetainingCapacity();
                // go though all the beacons seen by the known scanner
                var known_beacon_positions_it = known_beacon_positions.keyIterator();
                while (known_beacon_positions_it.next()) |known_beacon| {
                    // go though all the beacons seen by the unknown scanner
                    for (unknown_scanner.beacons) |unknown_beacon| {
                        // given a beacon X that can be seen by both beacons A and B, the
                        // position of B relative to A is the sum of the vector AX and the
                        // vector XB
                        // we known the position of X relative to A, i.e, the vector AX
                        // we known the position of X relative to B, i.e, the vector BX
                        // => AB = AX - BX = AX + XB
                        const potential_scanner_pos = known_beacon.sub(unknown_beacon.rotate(orientation));
                        const overlap_count = try overlap_counts.getOrPut(potential_scanner_pos);
                        if (overlap_count.found_existing) {
                            overlap_count.value_ptr.* += 1;
                            if (overlap_count.value_ptr.* >= 12) {
                                known_scanners.set(unknown_scanner_idx);
                                known_scanner_positions.items[unknown_scanner_idx] = potential_scanner_pos;
                                for (unknown_scanner.beacons) |beacon| {
                                    // now that we know AB in addition to BX, the position of X
                                    // relative to A can be calculate with AX = AB + BX
                                    const true_beacon = potential_scanner_pos.add(beacon.rotate(orientation));
                                    try known_beacon_positions.put(true_beacon, {});
                                }
                                // gotta continue from the most outer loop as we invalidated `unknown_scanners`
                                // when setting a value in `known_scanners`
                                continue :reconstruction_loop;
                            }
                        } else {
                            overlap_count.value_ptr.* = 1;
                        }
                    }
                }
            }
        }
    }
    debug.print("number of beacons is {}\n", .{known_beacon_positions.count()});

    var max_distance: isize = math.minInt(isize);
    for (known_scanner_positions.items) |pos1, i| {
        for (known_scanner_positions.items[i + 1 ..]) |pos2| {
            const v = pos1.sub(pos2);
            var distance: isize = 0;
            distance += try math.absInt(v.x);
            distance += try math.absInt(v.y);
            distance += try math.absInt(v.z);
            max_distance = math.max(max_distance, distance);
        }
    }
    debug.print("max distance between 2 scanners is {}\n", .{max_distance});
}

const Scanner = struct {
    beacons: []Vec3,
};

const Vec3 = struct {
    const Self = @This();
    x: isize,
    y: isize,
    z: isize,

    fn eql(lhs: Self, rhs: Self) bool {
        const equality = lhs.x == rhs.x and lhs.y == rhs.y and lhs.z == rhs.z;
        return equality;
    }
    fn add(lhs: Self, rhs: Self) Self {
        const addition = .{
            .x = lhs.x + rhs.x,
            .y = lhs.y + rhs.y,
            .z = lhs.z + rhs.z,
        };
        return addition;
    }
    fn sub(lhs: Self, rhs: Self) Self {
        const subtracted = .{
            .x = lhs.x - rhs.x,
            .y = lhs.y - rhs.y,
            .z = lhs.z - rhs.z,
        };
        return subtracted;
    }
    fn rotate(self: Self, orientation: usize) Self {
        const x = self.x;
        const y = self.y;
        const z = self.z;
        const rotated = switch (orientation) {
            0 => Vec3{ .x = x, .y = y, .z = z },
            1 => Vec3{ .x = x, .y = z, .z = -y },
            2 => Vec3{ .x = x, .y = -y, .z = -z },
            3 => Vec3{ .x = x, .y = -z, .z = y },

            4 => Vec3{ .x = -x, .y = y, .z = -z },
            5 => Vec3{ .x = -x, .y = z, .z = y },
            6 => Vec3{ .x = -x, .y = -y, .z = z },
            7 => Vec3{ .x = -x, .y = -z, .z = -y },

            8 => Vec3{ .x = y, .y = -x, .z = z },
            9 => Vec3{ .x = y, .y = -z, .z = -x },
            10 => Vec3{ .x = y, .y = x, .z = -z },
            11 => Vec3{ .x = y, .y = z, .z = x },

            12 => Vec3{ .x = -y, .y = -x, .z = -z },
            13 => Vec3{ .x = -y, .y = -z, .z = x },
            14 => Vec3{ .x = -y, .y = x, .z = z },
            15 => Vec3{ .x = -y, .y = z, .z = -x },

            16 => Vec3{ .x = z, .y = x, .z = y },
            17 => Vec3{ .x = z, .y = y, .z = -x },
            18 => Vec3{ .x = z, .y = -x, .z = -y },
            19 => Vec3{ .x = z, .y = -y, .z = x },

            20 => Vec3{ .x = -z, .y = x, .z = -y },
            21 => Vec3{ .x = -z, .y = y, .z = x },
            22 => Vec3{ .x = -z, .y = -x, .z = y },
            23 => Vec3{ .x = -z, .y = -y, .z = -x },

            else => @panic("Invalid orientation"),
        };
        return rotated;
    }
};

test "keep basis orthogonal after 3D vector rotations" {
    const impl = struct {
        fn cross(v1: Vec3, v2: Vec3) Vec3 {
            const cross_product = .{
                .x = v1.y * v2.z - v1.z * v2.y,
                .y = v1.z * v2.x - v1.x * v2.z,
                .z = v1.x * v2.y - v1.y * v2.x,
            };
            return cross_product;
        }
    };

    const xbasis = Vec3{ .x = 1, .y = 0, .z = 0 };
    const ybasis = Vec3{ .x = 0, .y = 1, .z = 0 };
    const zbasis = Vec3{ .x = 0, .y = 0, .z = 1 };

    var orientation: usize = 0;
    while (orientation < 24) : (orientation += 1) {
        const x = xbasis.rotate(orientation);
        const y = ybasis.rotate(orientation);
        const z = zbasis.rotate(orientation);
        try testing.expect(impl.cross(x, y).eql(z));
    }
}

test "get 24 unique orientations with 3D vector rotations" {
    var gpa = heap.GeneralPurposeAllocator(.{}){};
    defer debug.assert(!gpa.deinit());

    var rotations = std.AutoHashMap(Vec3, void).init(gpa.allocator());
    defer rotations.deinit();

    const v = Vec3{ .x = 1, .y = 2, .z = 3 };
    var orientation: usize = 0;
    while (orientation < 24) : (orientation += 1) try rotations.put(v.rotate(orientation), {});
    try testing.expect(rotations.count() == 24);
}
