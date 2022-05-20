PROFILE=release

.PHONY: build clean

build: setup.py
	pip wheel -w dist --no-deps .
	python setup.py sdist

setup.py:
	dune build --profile=$(PROFILE) ./setuppy_gen.exe
	dune exec --profile=$(PROFILE) -- ./setuppy_gen.exe

clean:
	dune clean
	rm -rf setup.py dist/ build/ XenAPI.egg-info/
