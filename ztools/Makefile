bindir=$(HOME)/bin

install:
	mkdir -p $(bindir)
	cd $(bindir)
	ln -s $(PWD)/z* "$(bindir)/"

uninstall:
	find $(bindir) -depth -type l -name "z*" -delete
