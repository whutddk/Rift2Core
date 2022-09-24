R2        ?= ${R2}


aluisa += rv64ui-p-add
aluisa += rv64ui-v-add
aluisa += rv64ui-p-addi
aluisa += rv64ui-v-addi
aluisa += rv64ui-p-addiw
aluisa += rv64ui-v-addiw
aluisa += rv64ui-p-addw
aluisa += rv64ui-v-addw
aluisa += rv64ui-p-and
aluisa += rv64ui-v-and
aluisa += rv64ui-p-andi
aluisa += rv64ui-v-andi
aluisa += rv64ui-p-auipc
aluisa += rv64ui-v-auipc
aluisa += rv64ui-p-or
aluisa += rv64ui-v-or
aluisa += rv64ui-p-ori
aluisa += rv64ui-v-ori
aluisa += rv64ui-p-sll
aluisa += rv64ui-v-sll
aluisa += rv64ui-p-slli
aluisa += rv64ui-v-slli
aluisa += rv64ui-p-slliw
aluisa += rv64ui-v-slliw
aluisa += rv64ui-p-sllw
aluisa += rv64ui-v-sllw
aluisa += rv64ui-p-slt
aluisa += rv64ui-v-slt
aluisa += rv64ui-p-slti
aluisa += rv64ui-v-slti
aluisa += rv64ui-p-sltiu
aluisa += rv64ui-v-sltiu
aluisa += rv64ui-p-sltu
aluisa += rv64ui-v-sltu
aluisa += rv64ui-p-sra
aluisa += rv64ui-v-sra
aluisa += rv64ui-p-srai
aluisa += rv64ui-v-srai
aluisa += rv64ui-p-sraiw
aluisa += rv64ui-v-sraiw
aluisa += rv64ui-p-sraw
aluisa += rv64ui-v-sraw
aluisa += rv64ui-p-srl
aluisa += rv64ui-v-srl
aluisa += rv64ui-p-srli
aluisa += rv64ui-v-srli
aluisa += rv64ui-p-srliw
aluisa += rv64ui-v-srliw
aluisa += rv64ui-p-srlw
aluisa += rv64ui-v-srlw
aluisa += rv64ui-p-sub
aluisa += rv64ui-v-sub
aluisa += rv64ui-p-subw
aluisa += rv64ui-v-subw
aluisa += rv64ui-p-xor
aluisa += rv64ui-v-xor
aluisa += rv64ui-p-xori
aluisa += rv64ui-v-xori
aluisa += rv64ui-p-simple
aluisa += rv64ui-v-simple

bruisa += rv64mi-p-ma_addr
bruisa += rv64mi-p-ma_fetch
bruisa += rv64si-p-ma_fetch
bruisa += rv64ui-p-beq
bruisa += rv64ui-v-beq
bruisa += rv64ui-p-bge
bruisa += rv64ui-v-bge
bruisa += rv64ui-p-bgeu
bruisa += rv64ui-v-bgeu
bruisa += rv64ui-p-blt
bruisa += rv64ui-v-blt
bruisa += rv64ui-p-bltu
bruisa += rv64ui-v-bltu
bruisa += rv64ui-p-bne
bruisa += rv64ui-v-bne
bruisa += rv64ui-p-jal
bruisa += rv64ui-v-jal
bruisa += rv64ui-p-jalr
bruisa += rv64ui-v-jalr

lsuisa += rv64ui-p-fence_i
lsuisa += rv64ui-v-fence_i
lsuisa += rv64ui-p-lb
lsuisa += rv64ui-v-lb
lsuisa += rv64ui-p-lbu
lsuisa += rv64ui-v-lbu
lsuisa += rv64ui-p-ld
lsuisa += rv64ui-v-ld
lsuisa += rv64ui-p-lh
lsuisa += rv64ui-v-lh
lsuisa += rv64ui-p-lhu
lsuisa += rv64ui-v-lhu
lsuisa += rv64ui-p-lui
lsuisa += rv64ui-v-lui
lsuisa += rv64ui-p-lw
lsuisa += rv64ui-v-lw
lsuisa += rv64ui-p-lwu
lsuisa += rv64ui-v-lwu
lsuisa += rv64ui-p-sb
lsuisa += rv64ui-v-sb
lsuisa += rv64ui-p-sd
lsuisa += rv64ui-v-sd
lsuisa += rv64ui-p-sh
lsuisa += rv64ui-v-sh
lsuisa += rv64ui-p-sw
lsuisa += rv64ui-v-sw
lsuisa += rv64ua-p-amoadd_d
lsuisa += rv64ua-v-amoadd_d
lsuisa += rv64ua-p-amoadd_w
lsuisa += rv64ua-v-amoadd_w
lsuisa += rv64ua-p-amoand_d
lsuisa += rv64ua-v-amoand_d
lsuisa += rv64ua-p-amoand_w
lsuisa += rv64ua-v-amoand_w
lsuisa += rv64ua-p-amomax_d
lsuisa += rv64ua-v-amomax_d
lsuisa += rv64ua-p-amomax_w
lsuisa += rv64ua-v-amomax_w
lsuisa += rv64ua-p-amomaxu_d
lsuisa += rv64ua-v-amomaxu_d
lsuisa += rv64ua-p-amomaxu_w
lsuisa += rv64ua-v-amomaxu_w
lsuisa += rv64ua-p-amomin_d
lsuisa += rv64ua-v-amomin_d
lsuisa += rv64ua-p-amomin_w
lsuisa += rv64ua-v-amomin_w
lsuisa += rv64ua-p-amominu_d
lsuisa += rv64ua-v-amominu_d
lsuisa += rv64ua-p-amominu_w
lsuisa += rv64ua-v-amominu_w
lsuisa += rv64ua-p-amoor_d
lsuisa += rv64ua-v-amoor_d
lsuisa += rv64ua-p-amoor_w
lsuisa += rv64ua-v-amoor_w
lsuisa += rv64ua-p-amoswap_d
lsuisa += rv64ua-v-amoswap_d
lsuisa += rv64ua-p-amoswap_w
lsuisa += rv64ua-v-amoswap_w
lsuisa += rv64ua-p-amoxor_d
lsuisa += rv64ua-v-amoxor_d
lsuisa += rv64ua-p-amoxor_w
lsuisa += rv64ua-v-amoxor_w
lsuisa += rv64ua-p-lrsc
lsuisa += rv64ua-v-lrsc
lsuisa += rv64uc-p-rvc
lsuisa += rv64uc-v-rvc



mulisa += rv64um-p-div
mulisa += rv64um-v-div
mulisa += rv64um-p-divu
mulisa += rv64um-v-divu
mulisa += rv64um-p-divuw
mulisa += rv64um-v-divuw
mulisa += rv64um-p-divw
mulisa += rv64um-v-divw
mulisa += rv64um-p-mul
mulisa += rv64um-v-mul
mulisa += rv64um-p-mulh
mulisa += rv64um-v-mulh
mulisa += rv64um-p-mulhsu
mulisa += rv64um-v-mulhsu
mulisa += rv64um-p-mulhu
mulisa += rv64um-v-mulhu
mulisa += rv64um-p-mulw
mulisa += rv64um-v-mulw
mulisa += rv64um-p-rem
mulisa += rv64um-v-rem
mulisa += rv64um-p-remu
mulisa += rv64um-v-remu
mulisa += rv64um-p-remuw
mulisa += rv64um-v-remuw
mulisa += rv64um-p-remw
mulisa += rv64um-v-remw

privisa += rv64mi-p-access
privisa += rv64mi-p-breakpoint
privisa += rv64mi-p-csr
privisa += rv64mi-p-illegal
privisa += rv64mi-p-mcsr
privisa += rv64mi-p-sbreak
privisa += rv64mi-p-scall
privisa += rv64si-p-csr
privisa += rv64si-p-dirty
privisa += rv64si-p-icache-alias
privisa += rv64si-p-sbreak
privisa += rv64si-p-scall
privisa += rv64si-p-wfi
    



# isa += rv64ssvnapot-p-napot

fpuisa += rv64ud-p-fadd
fpuisa += rv64ud-v-fadd
fpuisa += rv64ud-p-fclass
fpuisa += rv64ud-v-fclass
fpuisa += rv64ud-p-fcmp
fpuisa += rv64ud-v-fcmp
fpuisa += rv64ud-p-fcvt
fpuisa += rv64ud-v-fcvt
fpuisa += rv64ud-p-fcvt_w
fpuisa += rv64ud-v-fcvt_w
fpuisa += rv64ud-p-fdiv
fpuisa += rv64ud-v-fdiv
fpuisa += rv64ud-p-fmadd
fpuisa += rv64ud-v-fmadd
fpuisa += rv64ud-p-fmin
fpuisa += rv64ud-v-fmin
fpuisa += rv64ud-p-ldst
fpuisa += rv64ud-v-ldst
fpuisa += rv64ud-p-move
fpuisa += rv64ud-v-move
fpuisa += rv64ud-p-recoding
fpuisa += rv64ud-v-recoding
fpuisa += rv64ud-p-structural
fpuisa += rv64ud-v-structural
fpuisa += rv64uf-p-fadd
fpuisa += rv64uf-v-fadd
fpuisa += rv64uf-p-fclass
fpuisa += rv64uf-v-fclass
fpuisa += rv64uf-p-fcmp
fpuisa += rv64uf-v-fcmp
fpuisa += rv64uf-p-fcvt
fpuisa += rv64uf-v-fcvt
fpuisa += rv64uf-p-fcvt_w
fpuisa += rv64uf-v-fcvt_w
fpuisa += rv64uf-p-fdiv
fpuisa += rv64uf-v-fdiv
fpuisa += rv64uf-p-fmadd
fpuisa += rv64uf-v-fmadd
fpuisa += rv64uf-p-fmin
fpuisa += rv64uf-v-fmin
fpuisa += rv64uf-p-ldst
fpuisa += rv64uf-v-ldst
fpuisa += rv64uf-p-move
fpuisa += rv64uf-v-move
fpuisa += rv64uf-p-recoding
fpuisa += rv64uf-v-recoding

isa ?= $(aluisa) $(bruisa) $(lsuisa) $(privisa) $(mulisa) $(fpuisa) 
# isa ?= $(fpuisa)










.PHONY: compile clean VSimTop

module:
	sbt "test:runMain test.testModule --target-dir generated --show-registrations --full-stacktrace -E verilog"

compile:
	rm -rf ./generated/Main/
	sbt "test:runMain test.testMain \
	-e verilog"

#--gen-mem-verilog \
# --inline \

# --list-clocks \

line: 
	rm -rf generated/Debug/
	rm -rf generated/Release/
	sbt "test:runMain test.testAll"

CONFIG ?= /Main/


VSimTop: 
	rm -rf ./generated/build/$(CONFIG)
	mkdir -p ./generated/build/$(CONFIG)
	verilator -Wno-fatal  \
	--timescale "1 ns / 1 ps" \
	-y ${R2}/generated/$(CONFIG) \
	-y ${R2}/tb/ \
	-y ${R2}/tb/vtb/ \
	--top-module SimTop \
	--trace-fst \
	-LDFLAGS -ldromajo_cosim \
	--cc ${R2}/tb/verilator/SimTop.v  \
	+define+RANDOMIZE_GARBAGE_ASSIGN \
	+define+RANDOMIZE_INVALID_ASSIGN \
	+define+RANDOMIZE_REG_INIT \
	+define+RANDOMIZE_MEM_INIT \
	+define+RANDOMIZE_DELAY=0 \
	+define+USE_POWER_PINS \
	--exe --build \
	${R2}/tb/verilator/sim_main.cpp  \
	${R2}/tb/verilator/diff.cpp \
	-Mdir ./generated/build/$(CONFIG) \
	-j 30


isa: VSimTop
	$(foreach test, $(isa), ${R2}/generated/build/$(CONFIG)/VSimTop -l -f ./tb/ci/$(test) || exit; )
	echo "{\n  \"schemaVersion\": 1, \n  \"label\": \"\", \n  \"message\": \"Pass\", \n  \"color\": \"f6bf94\" \n}" >> isa.json
	mv isa.json ${R2}/generated/$(CONFIG)/isa.json

single: VSimTop
	${R2}/generated/build/$(CONFIG)/VSimTop -w -d -l -p -f ./tb/ci/$(TESTFILE)

torture:
	${R2}/generated/build/$(CONFIG)/VSimTop -d -l -p -f ./tb/torture/output/test

dhrystone5: VSimTop
	${R2}/generated/build/$(CONFIG)/VSimTop -w -p -f  ./tb/ci/dhrystone5.riscv

dhrystone500: VSimTop
	${R2}/generated/build/$(CONFIG)/VSimTop -p -f ./tb/ci/dhrystone500.riscv
	mv dhrystone.json ${R2}/generated/$(CONFIG)/dhrystone.json

coremark: VSimTop
	${R2}/generated/build/$(CONFIG)/VSimTop -p -f ./tb/ci/coremark1_bare
	mv coremark.json ${R2}/generated/$(CONFIG)/coremark.json

wave:
	gtkwave ${R2}/generated/build/wave.vcd &

test: VSimTop isa dhrystone500 coremark
	
yosys:
	rm -f $(R2)/generated/$(CONFIG)/area.json
	cd $(R2)/generated/$(CONFIG) \
	&& yosys ./Rift2Chip.v ./plusarg_reader.v $(R2)/src/yosys/area.ys

area: yosys
	echo "{\n  \"schemaVersion\": 1, \n  \"label\": \"\", \n  \"message\": \""$(basename $(filter %.000000, $(shell cat $(R2)/generated/$(CONFIG)/stat.log) ))"\", \n  \"color\": \"a6bf94\" \n}" >> $(R2)/generated/$(CONFIG)/area.json
# rm -f $(R2)/generated/$(CONFIG)/stat.log


# lineCfg += Rift2300
# lineCfg += Rift2310
# lineCfg += Rift2320
# lineCfg += Rift2330
# lineCfg += Rift2340
# lineCfg += Rift2350
# lineCfg += Rift2360
# lineCfg += Rift2370
# lineCfg += Rift2380
# lineCfg += Rift2390

# CONFIG ?= /Debug/Rift2330


# testAll:
# 	$(foreach cfg, $(lineCfg), make test CONFIG=/Debug/$(cfg); )

# yosysAll:
# 	$(foreach cfg, $(lineCfg), make yosys CONFIG=/Release/$(cfg); )
# CONFIG ?= Rift2330
# lineSim: 
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.cpp
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.o
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.d
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.h
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.mk
# 	rm -f ${R2}/generated/Debug/$(CONFIG)/build/*.a
# 	verilator -Wno-fatal  \
# 	--timescale "1 ns / 1 ps" \
# 	-y ${R2}/generated/Debug/$(CONFIG)/ \
# 	-y ${R2}/tb/ \
# 	-y ${R2}/tb/vtb/ \
# 	--top-module SimTop \
# 	--trace \
# 	--cc ${R2}/tb/verilator/SimTop.v  \
# 	+define+RANDOMIZE_GARBAGE_ASSIGN \
# 	+define+RANDOMIZE_INVALID_ASSIGN \
# 	+define+RANDOMIZE_REG_INIT \
# 	+define+RANDOMIZE_MEM_INIT \
# 	--exe --build \
# 	-LDFLAGS -ldromajo_cosim \
# 	${R2}/tb/verilator/sim_main.cpp  \
# 	${R2}/tb/verilator/diff.cpp \
# 	-Mdir  ${R2}/generated/Debug/$(CONFIG)/build \
# 	-j 128


# lineUnit:
# 	$(foreach test, $(isa), ${R2}/generated/Debug/$(CONFIG)/build/VSimTop -l -f ./tb/ci/$(test); )

# lineDhrystone500:
# 	${R2}/generated/Debug/$(CONFIG)/build/VSimTop -p -f ./tb/ci/dhrystone500.riscv

# lineCoremark:
# 	${R2}/generated/Debug/$(CONFIG)/build/VSimTop -p -f ./tb/ci/coremark1_bare





# sim:
# 	verilator --cc  -I./generated -I./tb  --exe  -o rift2tb -Mdir ./tb/build rift2core_tb.cpp Rift2Chip.v



VSimDebugger: 
	rm -rf ./generated/build/$(CONFIG)
	mkdir -p ./generated/build/$(CONFIG)
	verilator -Wno-fatal  \
	--timescale "1 ns / 1 ps" \
	-O3 \
	--x-assign fast \
	--x-initial fast \
	--threads 32 \
	-y ${R2}/generated/$(CONFIG) \
	-y ${R2}/tb/ \
	-y ${R2}/tb/vtb/ \
	--top-module SimTop \
	--trace-fst \
	--cc ${R2}/tb/debugger/SimTop.v  \
	--exe --build \
	+define+RANDOMIZE_GARBAGE_ASSIGN \
	+define+RANDOMIZE_INVALID_ASSIGN \
	+define+RANDOMIZE_REG_INIT \
	+define+RANDOMIZE_MEM_INIT \
	${R2}/tb/debugger/sim_main.cpp  \
	${R2}/tb/debugger/SimJTAG.cc \
	${R2}/tb/debugger/remote_bitbang.cc \
	-Mdir ./generated/build/$(CONFIG) \
	-j 128



jtag:
	${R2}/generated/build/$(CONFIG)/VSimTop -j -f ./tb/debugger/jtag

fst:
	gtkwave ${R2}/generated/build/wave.fst &


tape:
	rm -rf ./generated/Main/
	rm -rf ./generated/TapeMain/
	rm -rf ./generated/TapeSim/
	sbt "test:runMain test.tapeMain"

clean:
	rm -rf generated/*



