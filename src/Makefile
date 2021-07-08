all: test

build:
	cd ../benchmark/benchmark$t; \
	ant clean soot

test:
	cd demos; \
	racket demo-$t.rkt

scala:
	python3 ../scripts/gen-bench-cases.py ../benchmark/benchmark6/src/org/projectfloodlight/openflow/types/OFPort.java ../benchmark/benchmark6-s/src/org/projectfloodlight/openflow/types/OFPort.java $n
	cd ../benchmark/benchmark6-s; \
	ant clean soot
	cd demos; \
	racket demo-scala.rkt

