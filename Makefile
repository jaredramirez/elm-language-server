.PHONY: build build-watch run

build:
	stack build

build-watch:
	stack build --file-watch

run: build
	stack exec elm-language-server-exe

install: build
	stack install
