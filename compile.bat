@REM @Author: Ruige Lee
@REM @Date:   2021-03-05 10:42:25
@REM @Last Modified by:   Ruige Lee
@REM Modified time: 2021-05-24 11:09:14




rem sbt "testOnly test.WaveformSpec"

sbt "test:runMain test.testMain --target-dir generated --split-modules --full-stacktrace"

