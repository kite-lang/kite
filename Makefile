# kite - the kite programming language

SHELL = /bin/sh
CC = cabal

DESTDIR = /usr/local
BUILDDIR = dist/build

LEX = src/Kite/Lexer
PAR = src/Kite/Parser
GENLEX = alex $(LEX).x -o $(LEX).hs
GENPAR = happy $(PAR).y -o $(PAR).hs

all: build

$(LEX).hs: $(LEX).x
	@echo Generating lexer...
	$(shell $(GENLEX))

$(PAR).hs: $(PAR).y
	@echo Generating parser...
	$(shell $(GENPAR))
	@echo

clean:
	@echo Cleaning...
	@$(CC) clean
	@rm -f $(LEX).hs $(PAR).hs
	@echo

build: $(LEX).hs $(PAR).hs
	@echo Building...
	@$(CC) configure
	@$(CC) build

install:
	@echo Installing...
	@mkdir -p $(DESTDIR)/bin
	@cp -f $(BUILDDIR)/kite/kite $(DESTDIR)/bin
	@chmod 755 $(DESTDIR)/bin/kite

uninstall:
	@echo Uninstalling...
	@rm -f $(DESTDIR)/bin/kite

test:
	@echo Testing...
	@$(CC) configure --enable-tests
	@$(CC) build
	@dist/build/test/test
#@$(CC) test

.PHONY: all clean generate build install uninstall test
