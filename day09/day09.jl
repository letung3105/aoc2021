parse_heights(input) = [[parse(Int, c) for c in row] for row in split(input, '\n')]

neighbours(pos, height, width) =
    filter([pos .+ offset for offset in [(1, 0), (0, 1), (-1, 0), (0, -1)]]) do pos
        pos[1] > 0 && pos[1] <= height && pos[2] > 0 && pos[2] <= width
    end

lowpoints(heightsmap) =
    [
        [
            all(neighbours((i, j), length(heightsmap), length(heights))) do pos
                heightsmap[pos[1]][pos[2]] > height
            end
            for (j, height) in enumerate(heights)
        ]
        for (i, heights) in enumerate(heightsmap)
    ]

function part01(input)
    heightsmap = strip(input) |> parse_heights
    lowpoints_markers = heightsmap |> lowpoints
    risk_level = 0
    for (heights, markers) in zip(heightsmap, lowpoints_markers)
        for (height, marker) in zip(heights, markers)
            if marker
                risk_level += height + 1
            end
        end
    end
    risk_level
end

function explore_basin(pos, heightsmap, height, width)
    visited = Set{Tuple{Int,Int}}()
    queue = [pos]
    while !isempty(queue)
        pos = pop!(queue)
        if pos in visited
            continue
        end
        push!(visited, pos)
        for neighbour in neighbours(pos, height, width)
            if heightsmap[neighbour[1]][neighbour[2]] != 9 &&
               heightsmap[neighbour[1]][neighbour[2]] > heightsmap[pos[1]][pos[2]]
                push!(queue, neighbour)
            end
        end
    end
    visited
end

function part02(input)
    heightsmap = strip(input) |> parse_heights
    lowpoints_markers = heightsmap |> lowpoints
    basins_sizes = Int[]
    for (i, markers) in enumerate(lowpoints_markers)
        for (j, marker) in enumerate(markers)
            if marker
                basin = explore_basin(
                    (i, j),
                    heightsmap,
                    length(lowpoints_markers),
                    length(markers)
                )
                push!(basins_sizes, length(basin))
            end
        end
    end
    prod(reverse(sort(basins_sizes))[1:3])
end

part01(read("input/day09_ex.txt", String))
part01(read("input/day09.txt", String))

part02(read("input/day09_ex.txt", String))
part02(read("input/day09.txt", String))
