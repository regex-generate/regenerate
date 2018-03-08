all:
	jbuilder build @install -j 4 --dev

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: all test clean
