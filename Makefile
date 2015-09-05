sloane.1: sloane.md
	pandoc -s -t man sloane.md -o sloane.1

sloane.html: sloane.md
	pandoc -s -t html sloane.md -o sloane.html

README.html: README.md
	pandoc -s -t html README.md -o README.html
