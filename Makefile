
# test:
# 	sbt "testOnly test.WaveformSpec"

compile:
	sbt "test:runMain test.testMain --target-dir generated --split-modules --full-stacktrace"


# sim:
# 	verilator --cc  -I./generated -I./tb  --exe  -o rift2tb -Mdir ./tb/build rift2core_tb.cpp Rift2Chip.v

