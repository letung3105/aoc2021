package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:os"
import "core:strings"

MAX_BUFFER :: 64

// count the number of ones for each position across all the sequences
count_one_bits :: proc(sequences: []string) -> [dynamic]uint {
  one_bits_counts: [dynamic]uint
  for sequence in sequences {
    for c, index in sequence {
      if index > len(one_bits_counts) - 1 {
        append(&one_bits_counts, 0)
      }
      if c == '1' {
        one_bits_counts[index] += 1
      }
    }
  }
  return one_bits_counts
}

find_rating :: proc(sequences: []string, select_most_common: bool) -> int {
  rating_sequences := make([dynamic]string, len(sequences))
  defer delete(rating_sequences)
  copy(rating_sequences[:], sequences)

  for i := 0; i < len(sequences[0]) && len(rating_sequences) > 1; i += 1 {
    one_bits_counts := count_one_bits(rating_sequences[:])
    defer delete(one_bits_counts)

    most_common := u8('0')
    if one_bits_counts[i] * 2 >= len(rating_sequences) {
      most_common = '1'
    }

    tmp_sequences := [dynamic]string{}
    defer delete(tmp_sequences)

    for sequence in rating_sequences {
      if (select_most_common && sequence[i] == most_common) ||
         (!select_most_common && sequence[i] != most_common) {
        append(&tmp_sequences, sequence)
      }
    }

    resize(&rating_sequences, len(tmp_sequences))
    copy(rating_sequences[:], tmp_sequences[:])
  }

  rating := 0
  for bit in rating_sequences[0] {
    rating *= 2
    if bit == '1' {
      rating += 1
    }
  }
  return rating
}

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.println("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [MAX_BUFFER]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  sequences := [dynamic]string{}
  defer {
    for seq in sequences do delete(seq)
    delete(sequences)
  }

  for bufio.scanner_scan(&scanner) {
    line := bufio.scanner_text(&scanner)
    sequence := strings.clone(line)
    append(&sequences, sequence)
  }
  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.printf("Could not read input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  // PART 01

  one_bits_counts := count_one_bits(sequences[:])
  defer delete(one_bits_counts)

  gamma := uint(0)
  epsilon := uint(0)
  for count in one_bits_counts {
    gamma *= 2;
    epsilon *= 2;
    if count * 2 > len(sequences) {
      gamma += 1
    } else {
      epsilon += 1
    }
  }
  fmt.printf("%d\n", gamma * epsilon)

  // PART 02

  o2_rating := find_rating(sequences[:], true)
  co2_rating := find_rating(sequences[:], false)
  fmt.printf("%d\n", o2_rating * co2_rating)
}
