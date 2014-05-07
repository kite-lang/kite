# kite - the kite programming language

SHELL = /bin/sh

DESTDIR = /usr/local
BUILDDIR = dist/build

GENFLS = src/Kite/Lexer.hs src/Kite/Parser.hs
GENLEX = alex src/Kite/Lexer.x -o src/Kite/Lexer.hs
GENPAR = happy src/Kite/Parser.y -o src/Kite/Parser.hs

all: generate build

src/Kite/Lexer.hs:
	$(shell ${GENLEX})

src/Kite/Parser.hs:
	$(shell ${GENPAR})

clean:
	@echo Cleaning...
	@cabal clean
	@echo

generate: ${GENFLS}
	@echo Generating...
	@if (( $(shell stat -c %Y src/Kite/Lexer.x) > \
	$(shell stat -c %Y src/Kite/Lexer.hs) )); then \
		echo "Lexer: generated"; \
		${GENLEX}; \
	else \
		echo 'Lexer: nothing changed'; \
	fi
	@if (( $(shell stat -c %Y src/Kite/Parser.y) > \
	$(shell stat -c %Y src/Kite/Parser.hs) )); then \
		echo "Parser: generated"; \
		${GENPAR}; \
	else \
		echo 'Parser: nothing changed'; \
	fi
	@echo

build: generate
	@echo Building...
	@cabal configure
	@cabal build

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

.PHONY: all clean generate build install uninstall test
