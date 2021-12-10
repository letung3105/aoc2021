parse_signals(signals) = map(Set{Char}, split(signals, ' '))

parse_line(line) = split(line, '|') .|> strip .|> parse_signals

function part01(input)
    ndigits = 0
    for line in split(strip(input), '\n')
        _, output = parse_line(line)
        filter!(o -> length(o) in [2, 3, 4, 7], output)
        ndigits += length(output)
    end
    ndigits
end

function segment_frequencies(signals)
    freq = Dict{Char,Int}([
        'a' => 0,
        'b' => 0,
        'c' => 0,
        'd' => 0,
        'e' => 0,
        'f' => 0,
        'g' => 0,
    ])
    for signal in signals
        for c in signal
            freq[c] += 1
        end
    end
    freq
end

# We can know which line maps to which segment by counting the characters.
# Then segments 'b', 'e', and 'f' will hace unique frequencies of 6, 4, and 9.
function map_segments_by_freq!(segments_mapping, signals)
    freq = segment_frequencies(signals)
    for (k, v) in freq
        if v == 4
            segments_mapping['e'] = k
        elseif v == 6
            segments_mapping['b'] = k
        elseif v == 9
            segments_mapping['f'] = k
        end
    end
    nothing
end

function map_signals_by_length!(signals_mapping, signals)
    for signal in signals
        if length(signal) == 2
            signals_mapping[1] = signal
        elseif length(signal) == 3
            signals_mapping[7] = signal
        elseif length(signal) == 4
            signals_mapping[4] = signal
        elseif length(signal) == 7
            signals_mapping[8] = signal
        end
    end
    nothing
end

# 2 is 8 with 'b' and 'f' removed
function is_signal2(signal, signals_mapping, segments_mapping)
    signal2 = setdiff(signals_mapping[8], segments_mapping['b'], segments_mapping['f'])
    issetequal(signal, signal2)
end

# 3 is 8 with 'b' and 'e' removed
function is_signal3(signal, signals_mapping, segments_mapping)
    signal3 = setdiff(signals_mapping[8], segments_mapping['b'], segments_mapping['e'])
    issetequal(signal, signal3)
end

# 9 is 8 with 'e' removed
function is_signal9(signal, signals_mapping, segments_mapping)
    signal3 = setdiff(signals_mapping[8], segments_mapping['e'])
    issetequal(signal, signal3)
end

function decode_signals(signals)
    signals = parse_signals(signals)

    # find segments 'b', 'e', 'f'
    segments_mapping = Dict{Char,Char}()
    map_segments_by_freq!(segments_mapping, signals)

    # find signals 1, 4, 7, 9
    signals_mapping = Dict{Int,Set{Char}}()
    map_signals_by_length!(signals_mapping, signals)

    for signal in signals
        if is_signal2(signal, signals_mapping, segments_mapping)
            signals_mapping[2] = signal
        elseif is_signal3(signal, signals_mapping, segments_mapping)
            signals_mapping[3] = signal
        elseif is_signal9(signal, signals_mapping, segments_mapping)
            signals_mapping[9] = signal
        end
    end

    # 0, 5, 6 is unknown
    filter!(signal -> signal ∉ values(signals_mapping), signals)
    for signal in signals
        # 5 is the only one left that has 5 segments
        if length(signal) == 5
            signals_mapping[5] = signal
            # 0 if there's 4 segments left after remove the segments for 1
        elseif length(setdiff(signal, signals_mapping[1])) == 4
            signals_mapping[0] = signal
            # 0 if there's 5 segments left after remove the segments for 1
        elseif length(setdiff(signal, signals_mapping[1])) == 5
            signals_mapping[6] = signal
        end
    end
    Dict([v => k for (k, v) in signals_mapping])
end

function decode_signals_v2(signals)
    signals = parse_signals(signals)

    # find signals 1, 4, 7, 9
    signals_mapping = Dict{Int,Set{Char}}()
    map_signals_by_length!(signals_mapping, signals)

    zero_six_nine = filter(x -> length(x) == 6, signals)
    signals_mapping[6] = first(filter(
        signal -> !isempty(setdiff(signals_mapping[7], signal)),
        zero_six_nine
    ))
    signals_mapping[9] = first(filter(
        signal -> signal != signals_mapping[6] && isempty(setdiff(signals_mapping[4], signal)),
        zero_six_nine
    ))
    signals_mapping[0] = first(filter(
        signal -> signal != signals_mapping[6] && signal != signals_mapping[9],
        zero_six_nine
    ))

    two_three_five = filter(x -> length(x) == 5, signals)
    signals_mapping[3] = first(filter(
        signal -> isempty(setdiff(signals_mapping[1], signal)),
        two_three_five,
    ))
    signals_mapping[5] = first(filter(
        signal -> isempty(setdiff(signal, signals_mapping[6])),
        two_three_five,
    ))
    signals_mapping[2] = first(filter(
        signal -> signal != signals_mapping[3] && signal != signals_mapping[5],
        two_three_five,
    ))

    Dict([v => k for (k, v) in signals_mapping])
end

function part02(input)
    s = 0
    for line in split(strip(input), '\n')
        signals, output = strip.(split(line, '|'))
        signals_mapping = decode_signals(signals)

        num = 0
        output = parse_signals(output)
        for signal in output
            num *= 10
            num += signals_mapping[signal]
        end
        s += num
    end
    s
end

function part02_v2(input)
    s = 0
    for line in split(strip(input), '\n')
        signals, output = strip.(split(line, '|'))
        signals_mapping = decode_signals_v2(signals)

        num = 0
        output = parse_signals(output)
        for signal in output
            num *= 10
            num += signals_mapping[signal]
        end
        s += num
    end
    s
end

part01(read("input/day08_ex.txt", String))
part01(read("input/day08.txt", String))

part02(read("input/day08_ex.txt", String))
part02(read("input/day08.txt", String))

part02_v2(read("input/day08_ex.txt", String))
part02_v2(read("input/day08.txt", String))