.PHONY: build build-watch run

build:
	stack build

build-watch:
	stack build --file-watch

run:
	stack exec
