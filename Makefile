web:
	rm web/regenerate_web.bc.js
	jbuilder build web/regenerate_web.bc.js -j 4 --verbose
	cp _build/default/web/regenerate_web.bc.js web/

all:
	jbuilder build @install -j 4 --dev

test:
	jbuilder runtest

clean:
	jbuilder clean

.PHONY: all test clean web
