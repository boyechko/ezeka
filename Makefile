README.md: README.org
	pandoc -f org -t gfm -o $@ $<

.PHONY: readme
readme: README.md
