



@REM sbt "testOnly test.WaveformSpec"

@REM sbt "test:runMain test.testMain --target-dir generated --split-modules --full-stacktrace"
@REM sbt "test:runMain test.testMain --help"

sbt "test:runMain test.testMain --target-dir generated --show-registrations --full-stacktrace -e verilog"

