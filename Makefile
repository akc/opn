opn.1: opn.md
	pandoc -s -t man opn.md -o opn.1

%.html: %.md
	pandoc -s -t html $< -o $@
