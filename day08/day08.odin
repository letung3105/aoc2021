package main

import "core:bufio"
import "core:fmt"
import "core:intrinsics"
import "core:os"
import "core:io"
import "core:strconv"
import "core:strings"

BUF_CAP :: 1024

SegmentSet :: bit_set['a' - 'a'..'g' - 'a'; u8]

Signal :: struct {
  segments: SegmentSet,
  segments_count: u8,
}

include :: proc (signal: ^Signal, segment: rune) {
  if segment not_in signal.segments do signal.segments_count += 1
  incl(&signal.segments, segment)
}

Entry :: struct { pattern: [10]Signal, output: [4]Signal }

parse_entry :: proc(line: ^string) -> (Entry, bool) {
  entry: Entry

  populate :: proc(signals: []Signal, signals_repr: ^string) {
    signals_repr^ = strings.trim_space(signals_repr^)
    i := 0
    for {
      signal, ok := strings.split_iterator(signals_repr, " ")
      if !ok do break
      for c in signal do include(&signals[i], c - 'a')
      i += 1
    }
  }

  if pattern, ok := strings.split_iterator(line, "|"); ok do populate(entry.pattern[:], &pattern)
  else {
    fmt.eprintln("Invalid input")
    os.exit(1)
  }
  if output, ok := strings.split_iterator(line, "|"); ok do populate(entry.output[:], &output)
  else {
    fmt.eprintln("Invalid input")
    os.exit(1)
  }

  return entry, true
}

decode :: proc(entry: ^Entry) -> [4]int {
  decoded: [10]Signal

  zero_six_nine: [3]Signal
  zero_six_nine_len := 0

  two_three_five: [3]Signal
  two_three_five_len := 0

  for signal in entry.pattern {
    if signal.segments_count == 2 do decoded[1] = signal
    if signal.segments_count == 4 do decoded[4] = signal
    if signal.segments_count == 3 do decoded[7] = signal
    if signal.segments_count == 7 do decoded[8] = signal
    if signal.segments_count == 6 {
      zero_six_nine[zero_six_nine_len] = signal
      zero_six_nine_len += 1
    }
    if signal.segments_count == 5 {
      two_three_five[two_three_five_len] = signal
      two_three_five_len += 1
    }
  }

  for signal in zero_six_nine {
    if decoded[7].segments - signal.segments != {} {
      decoded[6] = signal
    }
  }
  for signal in zero_six_nine {
    if signal != decoded[6] && decoded[4].segments - signal.segments == {} {
      decoded[9] = signal
    }
  }
  for signal in zero_six_nine {
    if signal != decoded[6] && signal != decoded[9] {
      decoded[0] = signal
    }
  }

  for signal in two_three_five {
    if decoded[1].segments - signal.segments == {} {
      decoded[3] = signal
    }
    if signal.segments - decoded[6].segments == {} {
      decoded[5] = signal
    }
  }
  for signal in two_three_five {
    if signal != decoded[3] && signal != decoded[5] {
      decoded[2] = signal
    }
  }

  results: [4]int
  for out, idx in entry.output {
    for signal, val in decoded {
      if out == signal {
        results[idx] = val
        break
      }
    }
  }

  return results
}

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.eprintln("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [BUF_CAP]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  entries := [dynamic]Entry{}
  defer delete(entries)

  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)
    if entry, ok := parse_entry(&line); ok do append(&entries, entry)
    else {
      fmt.eprintln("Invalid input")
      os.exit(1)
    }
  }
  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.eprintf("Invalid input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  count_uniques := 0
  sum := 0;
  for entry in &entries {
    n := 0
    for val in decode(&entry) {
      if val == 1 || val == 4 || val == 7 || val == 8 {
        count_uniques += 1
      }
      n *= 10
      n += val
    }
    sum += n
  }
  fmt.printf("%d\n", count_uniques)
  fmt.printf("%d\n", sum)
}
