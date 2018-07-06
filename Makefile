.PHONY: all clean

all:
	jbuilder build

test:
	jbuilder runtest

clean:
	rm -rf _build *.install
