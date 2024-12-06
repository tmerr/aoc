all: out/part1 out/part2

out/part1: out/part1.o
	ld $< -o $@

out/part1.o: part1.nasm
	mkdir -p out
	nasm -felf64 $< -o $@

out/part2: out/part2.o
	ld $< -o $@

out/part2.o: part2.nasm
	mkdir -p out
	nasm -felf64 $< -o $@

clean:
	rm -rf out
