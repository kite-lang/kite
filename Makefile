# kite - the kite programming language

GEN = src/Lexer.hs src/Parser.hs

all: clean generate build test

clean:
	@echo Cleaning...
	@rm -f ${GEN}
	@echo

generate:
	@echo Generating...
	@alex src/Lexer.x -o src/Lexer.hs
	@happy src/Parser.y -o src/Parser.hs
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
