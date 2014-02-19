# kite - the kite programming language

GEN = src/Kite/Lexer.hs src/Kite/Parser.hs

all: clean generate build test

clean:
	@echo Cleaning...
	@rm -f ${GEN}
	@echo

generate:
	@echo Generating...
	@alex src/Kite/Lexer.x -o src/Kite/Lexer.hs
	@happy src/Kite/Parser.y -o src/Kite/Parser.hs
	@echo

build: clean generate
	@echo Building...
	@cabal configure --enable-tests
	@cabal build
	@echo

test:
	@echo Testing...
	@cabal test
	@echo

.PHONY: all clean generate build test
