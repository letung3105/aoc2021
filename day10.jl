function parse_chunk(input)
    errors = Char[]
    incompletes = Vector{Char}[]
    for line in split(strip(input), '\n')
        stack = Char[]
        haserror = false
        for c in line
            if c == '(' || c == '[' || c == '{' || c == '<'
                push!(stack, c)
            elseif c == ')' || c == ']' || c == '}' || c == '>'
                top = pop!(stack)
                if top == '(' && c != ')' || top == '[' && c != ']' ||
                   top == '{' && c != '}' || top == '<' && c != '>'
                    push!(errors, c)
                    haserror = true
                    break
                end
            end
        end
        if !haserror
            push!(incompletes, stack)
        end
    end
    errors, incompletes
end

function part01(input)
    errors, _ = parse_chunk(input)
    score = 0
    for e in errors
        if e == ')'
            score += 3
        elseif e == ']'
            score += 57
        elseif e == '}'
            score += 1197
        elseif e == '>'
            score += 25137
        end
    end
    score
end

function part02(input)
    _, incompletes = parse_chunk(input)
    scores = []
    for incomplete in incompletes
        score = 0
        while !isempty(incomplete)
            c = pop!(incomplete)
            if c == '('
                score *= 5
                score += 1
            elseif c == '['
                score *= 5
                score += 2
            elseif c == '{'
                score *= 5
                score += 3
            elseif c == '<'
                score *= 5
                score += 4
            end
        end
        push!(scores, score)
    end
    sort!(scores)
    scores[div((end + 1), 2)]
end

part01(read("input/day10_ex.txt", String))
part02(read("input/day10_ex.txt", String))

part01(read("input/day10.txt", String))
part02(read("input/day10.txt", String))
