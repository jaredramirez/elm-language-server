.PHONY: build run copy-exe install install-deps test test-install-deps clean link

build:
	cabal build

run: build
	./dist/build/elm-language-server-exe/elm-language-server-exe

copy-exe:
	rm ~/.local/bin/elm-language-server-exe && cp ./dist/build/elm-language-server-exe/elm-language-server-exe ~/.local/bin/elm-language-server-exe

install: | build copy-exe

install-deps:
	cabal install --only-dependencies

test:
	cabal test

test-install-deps:
	cabal install --only-dependencies --enable-tests

clean:
	cabal clean

link:
	cabal sandbox add-source ./elm-compiler-library
