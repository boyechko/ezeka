bindir=$(HOME)/bin

install:
	cd $(bindir)
	ln -s $(PWD)/z* "$(bindir)/"

uninstall:
	find $(bindir) -depth -type l -name "z*" -delete
