.PHONY: build run install install-deps

build:
	cabal build

run: build
	./dist/build/elm-language-server-exe/elm-language-server-exe

install: build
	cp ./dist/build/elm-language-server-exe/elm-language-server-exe ~/.local/bin/elm-language-server-exe

install-deps:
	cabal install --only-dependencies
