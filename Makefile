.PHONY: all test clean

all:
	jbuilder build --dev

test:
	jbuilder runtest --dev

clean:
	jbuilder clean
