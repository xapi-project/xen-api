.PHONY: release build install uninstall clean test doc reindent

release:
	jbuilder build @install

build:
	jbuilder build @install --dev

install:
	install -D -m 0755 ./_build/default/src/squeezed.exe $(DESTDIR)/squeezed

uninstall:
	rm $(DESTDIR)/squeezed

clean:
	jbuilder clean

test:
	jbuilder runtest

# requires odoc
doc:
	jbuilder build @doc

reindent:
	git ls-files '*.ml*' | xargs ocp-indent --inplace
