.PHONY: build clean test

build:
	jbuilder build --dev

test:
	jbuilder runtest --dev

install:
	jbuilder install

clean:
	jbuilder clean
