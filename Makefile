
test:
	sbt "testOnly test.WaveformSpec"

compile:
	sbt "test:runMain test.testMain --target-dir generated --split-modules --full-stacktrace"

