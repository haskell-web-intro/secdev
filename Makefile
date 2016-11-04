all: index.html

BLD = slide-build
SRC = $(BLD)/slides-src
CMN = examples/reflex-common/src


REVEAL-REFLEX=pandoc\
		--from=markdown \
		-t revealjs \
		--section-divs \
		--filter ./filter-pandoc \
		-H header.tmpl \
		--smart -s \
		--template=template.html

index.html: secdev.md $(BLD)/template.html $(BLD)/header.tmpl $(CMN)/MdlWidgets.hs $(SRC)/ReflexCommon.hs $(BLD)/filter-pandoc
	cd $(BLD) && $(REVEAL-REFLEX) ../secdev.md -o ../index.html && cd ..
	cp `stack --stack-yaml $(BLD)/stack.yaml path --local-install-root`/bin/slides.jsexe/all.js js/

$(BLD)/filter-pandoc: $(BLD)/filter-bld/filter.hs
	stack build --stack-yaml $(BLD)/filter-bld/stack.yaml
	cp `stack --stack-yaml $(BLD)/filter-bld/stack.yaml path --local-install-root`/bin/filter-pandoc $(BLD)

publish: index.html
	git commit -am"Commit before publish" || true
	git branch -D gh-pages || true
	git push origin :gh-pages || true
	git checkout --orphan gh-pages
	git rm -rf *.md *.txt ghcjs-docker examples bundle servant-docker slide-build .gitignore
	git add index.html js
	git commit -am"Publish new site version"
	git push -u origin gh-pages
	git checkout master

clean:
	rm -rf *~ $(SRC)/M* $(SRC)/main.* $(SRC)/*.js_* index.html
