.PHONY: build install clean test

build:
	dune build

install: build
	dune install

clean:
	dune clean

test:
	dune test

# Build a static binary for deployment
release:
	dune build --force @install
	cp _build/default/bin/main.exe sq-store-bin 2>/dev/null || \
	cp _build/default/bin/main sq-store-bin
	@echo "Built: sq-store-bin"
