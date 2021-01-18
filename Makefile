.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune test
