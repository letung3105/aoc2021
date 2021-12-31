package main

import "core:bufio"
import "core:fmt"
import "core:io"
import "core:math"
import "core:os"
import "core:strconv"
import "core:strings"

MAX_BUFFER :: 4 * 1024

main :: proc() {
  stdin_reader, ok := io.to_reader(os.stream_from_handle(os.stdin))
  if !ok {
    fmt.eprintln("Could not read input")
    os.exit(1)
  }
  defer io.destroy(stdin_reader)

  buf: [MAX_BUFFER]u8
  scanner: bufio.Scanner
  bufio.scanner_init_with_buffer(&scanner, stdin_reader, buf[:])
  defer bufio.scanner_destroy(&scanner)

  laternfish := [9]uint{}

  if ok := bufio.scanner_scan(&scanner); ok {
  	state := bufio.scanner_text(&scanner)
		for val, ok := strings.split_iterator(&state, ","); ok;
				val, ok = strings.split_iterator(&state, ",") {
			if num, ok := strconv.parse_int(val); ok do laternfish[num] += 1
		}
  }
  if err := bufio.scanner_error(&scanner); err != nil {
    if err != .No_Progress {
      fmt.eprintf("Invalid input. Error: '%s'\n", err)
      os.exit(1)
    }
  }

  fish_count1, fish_count2: uint
	for i in 1..256 {
		cycle(&laternfish)
		if i == 80 do fish_count1 = math.sum(laternfish[:])
		if i == 256 do fish_count2 = math.sum(laternfish[:])
	}

	fmt.printf("%d\n", fish_count1)
	fmt.printf("%d\n", fish_count2)
}

cycle :: proc(laternfish: ^[9]uint) {
	count_new := laternfish[0]
	for i in 0..7 {
		laternfish[i] = laternfish[i + 1]
	}
	laternfish[6] += count_new
	laternfish[8] = count_new
}