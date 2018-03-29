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

NAME=regenerate
DOCDIR=.gh-pages

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages git@github.com:Drup/$(NAME).git . \
	)

gh-pages-index: $(DOCDIR)/.git web
	cp web/*.html web/*.js web/*.css "$(DOCDIR)/"

gh-pages: $(DOCDIR)/.git gh-pages-index
	git -C $(DOCDIR) add --all 
	git -C $(DOCDIR) commit -a -m "gh-page updates"
	git -C $(DOCDIR) push origin gh-pages


.PHONY: all test clean web prebuild gh-pages
