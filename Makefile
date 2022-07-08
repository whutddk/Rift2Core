
.PHONY: test compile clean

test:
	sbt "test:runMain test.testModule --target-dir generated --show-registrations --full-stacktrace -e verilog"

compile: clean
	sbt "test:runMain test.testMain \
	--target-dir generated \
	--show-registrations \
	--full-stacktrace \
	-e verilog"
	
#--gen-mem-verilog \
# --inline \

# --list-clocks \

# sim:
# 	verilator --cc  -I./generated -I./tb  --exe  -o rift2tb -Mdir ./tb/build rift2core_tb.cpp Rift2Chip.v

clean:
	rm -f generated/*.v generated/*.json generated/*.fir generated/*.f



