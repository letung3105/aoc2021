all: buildrs buildhs buildc buildzig

buildrs: setup day01rs day02rs day03rs day04rs day05rs day06rs day07rs
buildhs: setup day01hs day02hs day03hs day04hs day05hs day11hs day12hs
buildc: setup day01c day02c day11c day12c
buildzig: setup day11zig day12zig day13zig day14zig day15zig day16zig \
	day17zig day18zig day19zig
buildodin: setup day01odin day02odin day03odin day04odin day05odin \
	day06odin day07odin day08odin day09odin day10odin day11odin

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

day11zig: day11/day11.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day12zig: day12/day12.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day13zig: day13/day13.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day14zig: day14/day14.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day15zig: day15/day15.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day16zig: day16/day16.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day17zig: day17/day17.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day18zig: day18/day18.zig
	zig build-exe --name $@ $? && mv $@ target/$@
day19zig: day19/day19.zig
	zig build-exe --name $@ $? && mv $@ target/$@

day01odin: day01/day01.odin
	odin build $? -out:target/$@
day02odin: day02/day02.odin
	odin build $? -out:target/$@
day03odin: day03/day03.odin
	odin build $? -out:target/$@
day04odin: day04/day04.odin
	odin build $? -out:target/$@
day05odin: day05/day05.odin
	odin build $? -out:target/$@
day06odin: day06/day06.odin
	odin build $? -out:target/$@
day07odin: day07/day07.odin
	odin build $? -out:target/$@
day08odin: day08/day08.odin
	odin build $? -out:target/$@
day09odin: day09/day09.odin
	odin build $? -out:target/$@
day10odin: day10/day10.odin
	odin build $? -out:target/$@
day11odin: day11/day11.odin
	odin build $? -out:target/$@

.PHONY: setup
setup:
	mkdir -p target
