.PHONY: build release test install uninstall clean reindent

build:
	jbuilder build @install

release:
	jbuilder build @install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

reindent:
	ocp-indent --inplace **/*.ml*
