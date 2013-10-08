sloane.1: README.md
	pandoc -s -t man README.md -o sloane.1

sloane.html: README.md
	pandoc -s -t html README.md -o sloane.html
