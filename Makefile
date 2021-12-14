all: buildrs buildhs

buildrs: setup day01rs day02rs day03rs day04rs day05rs day06rs day07rs
buildhs: setup day01hs day02hs day03hs day04hs day05hs day11hs day12hs
buildc: setup day01c day02c day11c day12c

day01rs: day01/day01.rs
	rustc -o target/$@ $?
day02rs: day02/day02.rs
	rustc -o target/$@ $?
day03rs: day03/day03.rs
	rustc -o target/$@ $?
day04rs: day04/day04.rs
	rustc -o target/$@ $?
day05rs: day05/day05.rs
	rustc -o target/$@ $?
day06rs: day06/day06.rs
	rustc -o target/$@ $?
day07rs: day07/day07.rs
	rustc -o target/$@ $?

day01hs: day01/day01.hs
	ghc -o target/$@ $?
day02hs: day02/day02.hs
	ghc -o target/$@ $?
day03hs: day03/day03.hs
	ghc -o target/$@ $?
day04hs: day04/day04.hs
	ghc -o target/$@ $?
day05hs: day05/day05.hs
	ghc -o target/$@ $?
day11hs: day11/day11.hs
	ghc -o target/$@ $?
day12hs: day12/day12.hs
	ghc -o target/$@ $?

day01c: day01/day01.c
	cc -o target/$@ $?
day02c: day02/day02.c
	cc -o target/$@ $?
day11c: day11/day11.c
	cc -o target/$@ $?
day12c: day12/day12.c
	cc -o target/$@ $?

.PHONY: setup
setup:
	mkdir -p target
