.PHONY: build build-watch run

build:
	cabal build

run: build
	./dist/build/elm-language-server-exe/elm-language-server-exe

link: build


install: build
	cabal install
