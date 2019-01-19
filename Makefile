.PHONY: build build-deps watch install

build:
	stack build

build-deps:
	stack build --only-dependencies

watch:
	stack build --file-watch

install:
	stack build --copy-bins
