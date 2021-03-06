CLASSPATH=bin:ext/asm.jar:ext/asm-tree.jar:ext/asm-util.jar


build:
	@mkdir bin 2>/dev/null || true
	@fsc -deprecation src/main/scala/cro/*.scala src/test/scala/cro/*.scala -d bin -classpath ${CLASSPATH}

run-test:
	scala -classpath  ${CLASSPATH} cro.Cro

run-bench:
	scala -classpath  ${CLASSPATH} cro.test.TokenizerBenchmark :test

clean:
	rm -r bin/*
