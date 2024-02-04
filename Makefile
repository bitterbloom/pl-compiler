
help:
	@echo "Usage: make <target>"
	@echo "       make help    	- show this help"
	@echo "       make build   	- build compiler"
	@echo "       make run     	- run compiler"
	@echo "       make test    	- run tests"
	@echo "       make clean   	- clean temp files"
	@echo "       make cleanall	- also clean cache files"

build:
	@zig build

run:
	@zig build run

test:
	@zig build test

clean:
	@rm -rf ./zig-out/
	@rm -rf ./temp/
	@rm -f ./result

cleanall: clean
	@rm -rf ./zig-cache/

.PHONY: help build run test clean cleanall

