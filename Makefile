.PHONY: build release install uninstall clean reindent

build:
	jbuilder build @install

release:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

reindent:
	ocp-indent --inplace **/*.ml*
