all: dist                      \
     dist/man                  \
     dist/demo.gif             \
     dist/github-pandoc.css    \
     dist/index.html           \
     dist/man/index.html

dist:
	mkdir -p dist

dist/man:
	mkdir -p dist/man

dist/%.gif: %.gif
	cp $< $@

dist/%.css: %.css
	cp $< $@

dist/index.html: index.md github-pandoc.css template.html
	pandoc -s -t html --template=template.html --css github-pandoc.css $< -o $@

dist/man/index.html: sloane.md github-pandoc.css
	pandoc -s -t html --css ../github-pandoc.css $< -o $@
