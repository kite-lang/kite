.PHONY: clean generate build test

all: clean generate build test

clean:
	-rm src/Lexer.hs

generate:
	alex src/Lexer.x -o src/Lexer.hs

build: clean generate
	cabal configure
	cabal build

test:
	cd src; \
	runhaskell ../test/Test.hs
