const std = @import("std");

const ArrayList = std.ArrayList;
const BufSet = std.BufSet;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const StringHashMap = std.StringHashMap;

const MAX_LINE_SZ = 16;

pub fn main() !void {
    // NOTE: we can solve the problem without memory allocations like in the C version
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var graph = StringHashMap(BufSet).init(gpa.allocator());
    defer {
        // we are owning both the key and the value
        var graph_it = graph.iterator();
        while (graph_it.next()) |adj_list| {
            gpa.allocator().free(adj_list.key_ptr.*);
            adj_list.value_ptr.deinit();
        }
        graph.deinit();
    }

    var buf: [MAX_LINE_SZ]u8 = undefined;
    var stdin_stream = std.io.getStdIn().reader();
    while (stdin_stream.readUntilDelimiterOrEof(&buf, '\n')) |maybe_line| {
        if (maybe_line) |line| {
            var split = std.mem.split(u8, line, "-");
            const v1 = split.next().?;
            const v2 = split.next().?;

            // NOTE: StringArrayHashMap does not own the key that was given to it, thus
            // allocating an string for the key is needed https://github.com/ziglang/zig/issues/7765

            if (graph.contains(v1)) {
                var adj_list = graph.getPtr(v1).?;
                try adj_list.insert(v2);
            } else {
                // only allocate when the HashMap does not contains our entry
                var v1_alloc = ArrayList(u8).init(gpa.allocator());
                try v1_alloc.appendSlice(v1);
                const v1_alloc_owned = v1_alloc.toOwnedSlice();
                var adj_list = BufSet.init(gpa.allocator());
                try adj_list.insert(v2);
                try graph.put(v1_alloc_owned, adj_list);
            }

            if (graph.contains(v2)) {
                var adj_list = graph.getPtr(v2).?;
                try adj_list.insert(v1);
            } else {
                // only allocate when the HashMap does not contains our entry
                var v2_alloc = ArrayList(u8).init(gpa.allocator());
                try v2_alloc.appendSlice(v2);
                const v2_alloc_owned = v2_alloc.toOwnedSlice();
                var adj_list = BufSet.init(gpa.allocator());
                try adj_list.insert(v1);
                try graph.put(v2_alloc_owned, adj_list);
            }
        } else {
            break;
        }
    } else |err| {
        std.debug.print("Could not read input {}", .{err});
    }

    const count1 = part1(&graph);
    std.debug.print("{}\n", .{count1});
    const count2 = part2(&graph);
    std.debug.print("{}\n", .{count2});
}

fn part1(graph: *StringHashMap(BufSet)) !usize {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var visits = StringHashMap(usize).init(gpa.allocator());
    defer visits.deinit();

    var caves_it = graph.keyIterator();
    while (caves_it.next()) |cave_name| try visits.put(cave_name.*, 0);

    return explore1("start", graph, &visits);
}

fn part2(graph: *StringHashMap(BufSet)) !usize {
    var gpa = GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(!gpa.deinit());

    var visits = StringHashMap(usize).init(gpa.allocator());
    defer visits.deinit();

    var caves_it = graph.keyIterator();
    while (caves_it.next()) |cave_name| try visits.put(cave_name.*, 0);

    return explore2("start", graph, &visits);
}

fn explore1(cave_current: []const u8, graph: *StringHashMap(BufSet), num_visits: *StringHashMap(usize)) usize {
    if (std.mem.eql(u8, cave_current, "end")) return 1;
    var count: usize = 0;
    var adj_list = graph.getPtr(cave_current).?;
    var adj_list_it = adj_list.iterator();
    while (adj_list_it.next()) |cave_next| {
        if (std.mem.eql(u8, cave_next.*, "start")) continue;
        var next_num_visits = num_visits.getPtr(cave_next.*).?;
        if (isSmall(cave_next.*) and next_num_visits.* > 0) continue;
        next_num_visits.* += 1;
        count += explore1(cave_next.*, graph, num_visits);
        next_num_visits.* -= 1;
    }
    return count;
}

fn explore2(cave_current: []const u8, graph: *StringHashMap(BufSet), num_visits: *StringHashMap(usize)) usize {
    if (std.mem.eql(u8, cave_current, "end")) return 1;
    var count: usize = 0;
    var adj_list = graph.getPtr(cave_current).?;
    var adj_list_it = adj_list.iterator();
    outer: while (adj_list_it.next()) |cave_next| {
        if (std.mem.eql(u8, cave_next.*, "start")) continue;
        var next_num_visits = num_visits.getPtr(cave_next.*).?;
        if (isSmall(cave_next.*) and next_num_visits.* > 0) {
            var num_visits_it = num_visits.iterator();
            while (num_visits_it.next()) |num_visits_entry|
                if (isSmall(num_visits_entry.key_ptr.*) and
                    num_visits_entry.value_ptr.* > 1) continue :outer;
        }
        next_num_visits.* += 1;
        count += explore2(cave_next.*, graph, num_visits);
        next_num_visits.* -= 1;
    }
    return count;
}

fn isSmall(cave_name: []const u8) bool {
    if (std.mem.eql(u8, cave_name, "start") or
        std.mem.eql(u8, cave_name, "end")) return false;
    for (cave_name) |c| if (!std.ascii.isLower(c)) return false;
    return true;
}
