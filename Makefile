.PHONY: build run install install-deps test test-install-deps clean

build:
	cabal build

run: build
	./dist/build/elm-language-server-exe/elm-language-server-exe

install: build
	cp ./dist/build/elm-language-server-exe/elm-language-server-exe ~/.local/bin/elm-language-server-exe

install-deps:
	cabal install --only-dependencies

test:
	cabal test

test-install-deps:
	cabal install --only-dependencies --enable-tests

clean:
	cabal clean
