.PHONY: build release install uninstall clean test reindent

build:
	jbuilder build @install --dev

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

test:
	jbuilder runtest

reindent:
	ocp-indent -i **/*.ml*
