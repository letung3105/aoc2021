buildrs: setup day01rs day02rs day03rs day04rs day05rs day06rs day07rs

day01rs: day01.rs
	rustc -o target/$@ $?

day02rs: day02.rs
	rustc -o target/$@ $?

day03rs: day03.rs
	rustc -o target/$@ $?

day04rs: day04.rs
	rustc -o target/$@ $?

day05rs: day05.rs
	rustc -o target/$@ $?

day06rs: day06.rs
	rustc -o target/$@ $?

day07rs: day07.rs
	rustc -o target/$@ $?

day01hs: day01.hs
	ghc -o target/$@ $?

.PHONY: setup
setup:
	mkdir -p target