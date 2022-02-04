
test:
	sbt "test:runMain test.testModule --target-dir generated --show-registrations --full-stacktrace -e verilog"

compile:
	sbt "test:runMain test.testMain --target-dir generated --show-registrations --full-stacktrace -e verilog"


# sim:
# 	verilator --cc  -I./generated -I./tb  --exe  -o rift2tb -Mdir ./tb/build rift2core_tb.cpp Rift2Chip.v

clean:
	rm generated/*.v generated/*.json generated/*.fir generated/*.f
	make -f tb/Makefile clean



sim:
	make -f tb/Makefile sim

unit:
	make -f tb/Makefile unit

single: sim
	make -f tb/Makefile single

dromajo:
	make -f tb/Makefile dromajo

dhrystone5: sim
	make -f tb/Makefile dhrystone5

dhrystone500: sim
	make -f tb/Makefile dhrystone500

coremark: sim
	make -f tb/Makefile coremark

wave:
	make -f tb/Makefile wave



module:
	make -f tb/Makefile module
