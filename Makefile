default: all

prebuild:
	@rm -f web/regenerate_web.bc.js

web: prebuild
	jbuilder build web/regenerate_web.bc.js
	@cp _build/default/web/regenerate_web.bc.js web/

all: prebuild
	jbuilder build @install --dev

test: prebuild
	jbuilder runtest

clean: prebuild
	jbuilder clean

.PHONY: all test clean web
