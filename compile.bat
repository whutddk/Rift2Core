@REM @Author: Ruige Lee
@REM @Date:   2021-03-05 10:42:25
@REM @Last Modified by:   Ruige Lee
@REM Modified time: 2021-06-22 09:43:16




@REM sbt "testOnly test.WaveformSpec"

@REM sbt "test:runMain test.testMain --target-dir generated --split-modules --full-stacktrace"
@REM sbt "test:runMain test.testMain --help"

sbt "test:runMain test.testMain --target-dir generated --show-registrations --full-stacktrace -e verilog"

