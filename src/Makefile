all: test

build:
	cd ../benchmark/benchmark$t; \
	ant clean soot

test:
	racket ./demos/demo-$t.rkt

