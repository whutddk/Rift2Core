


all:
	riscv64-unknown-elf-gcc -Os -ggdb -march=rv64imac -mabi=lp64 -Wall -mcmodel=medany -mexplicit-relocs \
	-c ./main.c \
	-o ./main.o

	riscv64-unknown-elf-gcc -Os -ggdb -march=rv64imc -mabi=lp64 -Wall -mcmodel=medany -mexplicit-relocs -mcmodel=medany -mexplicit-relocs \
	-c ./startup.S \
	-o ./startup.o

	riscv64-unknown-elf-gcc -Os -ggdb -march=rv64imc -mabi=lp64 -Wall -mcmodel=medany -mexplicit-relocs -nostdlib -nodefaultlibs -nostartfiles \
	-T linker.lds \
	./startup.o \
	./main.o \
	-o clint_plic.elf

	riscv64-unknown-elf-objcopy -O binary ./clint_plic.elf  ./clint_plic.bin

	riscv64-unknown-elf-objdump \
	--disassemble-all \
	--disassemble-zeroes \
	--section=.text \
	--section=.text.startup \
	--section=.text.init \
	--section=.data \
	--section=.bss \
	--section=.rodata \
	clint_plic.elf > clint_plic.dump


	riscv64-unknown-elf-objcopy -O verilog ./clint_plic.elf  ./clint_plic.verilog
	sed -i 's/@800/@000/g' ./clint_plic.verilog





