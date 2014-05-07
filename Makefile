# kite - the kite programming language

GEN = src/Kite/Lexer.hs src/Kite/Parser.hs

DESTDIR = /usr/local
BUILDDIR = dist/build

all: clean generate build

clean:
	@echo Cleaning...
	@cabal clean
	@rm -f ${GEN}
	@echo

generate:
	@echo Generating...
	@alex src/Kite/Lexer.x -o src/Kite/Lexer.hs
	@happy src/Kite/Parser.y -o src/Kite/Parser.hs
	@echo

build: clean generate
	@echo Building...
	@cabal configure
	@cabal build
	@echo

install:
	@echo Installing...
	@mkdir -p ${DESTDIR}/bin
	@cp -f ${BUILDDIR}/kite/kite ${DESTDIR}/bin
	@chmod 755 ${DESTDIR}/bin/kite

uninstall:
	@echo Uninstalling...
	@rm -f ${DESTDIR}/bin/kite

test:
	@echo Testing...
	@cabal configure --enable-tests
	@cabal build
	@cabal test
	@echo

.PHONY: all clean generate build test
