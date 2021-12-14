function parseInput(input)
    input = split(strip(input), "\n")
    rules = Dict{String,Char}()
    for rule in input[3:end]
        state, insert = strip.(split(rule, "->"))
        rules[state] = first(insert)
    end
    input[1], rules
end

function countChars(input)
    freq = Dict{Char,Int}()
    for c in input
        freq[c] = get(freq, c, 0) + 1
    end
    freq
end

# Memory-bound because the size of the polymere grows exponentially
function countElemsWithSimulation(polymere, rules, steps)
    for _ = 1:steps
        tmp = ""
        for (c1, c2) in zip(polymere, polymere[2:end])
            tmp *= c1 * rules[c1*c2]
        end
        tmp *= last(polymere)
        polymere = tmp
    end
    charsFreq = countChars(polymere)
    counts = values(charsFreq)
    maximum(counts) - minimum(counts)
end

# CPU-bound because the number of operations grows exponentially
function countElemsRecursive(polymere, rules, steps)
    charsFreq = countChars(polymere)
    inner = function (pair, currStep)
        if currStep == 0
            return
        end
        inserted = rules[pair]
        charsFreq[inserted] = get(charsFreq, inserted, 0) + 1
        inner(pair[1] * inserted, currStep - 1)
        inner(inserted * pair[2], currStep - 1)
    end
    for (c1, c2) in zip(polymere, polymere[2:end])
        inner(c1 * c2, steps)
    end
    counts = values(charsFreq)
    maximum(counts) - minimum(counts)
end

function part01a(input)
    polymere, rules = parseInput(input)
    countElemsWithSimulation(polymere, rules, 10)
end

function part01b(input)
    polymere, rules = parseInput(input)
    countElemsRecursive(polymere, rules, 10)
end

# We know that by following the rule, a pair will get replaced by 2 new pairs
# Thus, let `x` be the frequency of a pair, then the frequency of each of the
# newly created pairs will be increased by `x`, and the frequency of the original
# pair will be decreased by `x`
function step!(pairs, rules)
    for (pair, count) in copy(pairs)
        p1 = pair[1] * rules[pair]
        p2 = rules[pair] * pair[2]
        pairs[p1] = get(pairs, p1, 0) + count
        pairs[p2] = get(pairs, p2, 0) + count
        pairs[pair] -= count
    end
end

function countElemsWithPairsFrequencies(polymere, rules, steps)
    # initialize the pairs count
    pairs = Dict{String,Int}()
    @views for (c1, c2) in zip(polymere, polymere[2:end])
        pair = c1 * c2
        pairs[pair] = get(pairs, pair, 0) + 1
    end
    # simulate and count the pairs
    for _ = 1:steps
        step!(pairs, rules)
    end
    # counts the characters based on the number of pairs
    charCounts = Dict{Char,Int}()
    for (pair, count) in pairs
        charCounts[pair[1]] = get(charCounts, pair[1], 0) + count
        charCounts[pair[2]] = get(charCounts, pair[2], 0) + count
    end
    # because the pairs overlap, characters that are not at the beginning or end
    # will be counted twice
    counts_max = maximum(values(charCounts))
    if counts_max % 2 != 0
        counts_max += 1
    end
    counts_min = minimum(values(charCounts))
    if counts_min % 2 != 0
        counts_min += 1
    end
    div(counts_max - counts_min, 2)
end

function part02(input)
    polymere, rules = parseInput(input)
    countElemsWithPairsFrequencies(polymere, rules, 40)
end

# timings for evaluation
let
    n = 16
    polymere, rules = parseInput(read("input/day14_ex.txt", String))
    @time countElemsWithSimulation(polymere, rules, n)
    @time countElemsRecursive(polymere, rules, n)
    @time countElemsWithPairsFrequencies(polymere, rules, n)
    nothing
end

# worst performance
part01a(read("input/day14_ex.txt", String))
part01a(read("input/day14.txt", String))

# slightly faster with much less allocations
part01b(read("input/day14_ex.txt", String))
part01b(read("input/day14.txt", String))

# no simulation needed
part02(read("input/day14_ex.txt", String))
part02(read("input/day14.txt", String))
