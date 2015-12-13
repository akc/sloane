sloane.1: sloane.md
	pandoc -s -t man docs/sloane.md -o sloane.1

README.html: README.md
	pandoc -s -t html README.md -o README.html
