
help:
	@echo "Usage: make <target>"
	@echo "       make help    - show this help"
	@echo "       make build   - build compiler"
	@echo "       make run     - run compiler"
	@echo "       make test    - run tests"
	@echo "       make clean   - clean temp files"

build:
	@echo build

run:
	@echo run
	@rm -f ./pl-compiler

test:
	@c3c test
	@rm -f ./testrun

clean:
	@rm -f ./pl-compiler
	@rm -f ./testrun
	@rm -f ./temp/*

.PHONY: help build run test clean

