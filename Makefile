opn.1: README.md
	pandoc -s -t man README.md -o opn.1

%.html: %.md
	pandoc -s -t html $< -o $@
