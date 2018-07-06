
.PHONY: build clean test

build:
	jbuilder build @install --dev

test:
	jbuilder build --dev test/client_test.exe
	jbuilder build --dev test/server_test.exe

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build
