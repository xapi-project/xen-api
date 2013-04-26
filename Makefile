PREFIX?=/usr/local

.PHONY: clean install uninstall

dist/build/rrddump/rrddump:
	obuild configure
	obuild build

clean:
	rm -rf dist

install: dist/build/rrddump/rrddump
	cp dist/build/rrddump/rrddump $(PREFIX)/bin/

uninstall:
	rm -f $(PREFIX)/bin/rrddump
