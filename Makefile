stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp

install:
	$(stack) install

test:
	$(stack) test

dev-deps:
	stack install ghcid

.PHONY : build build-dirty install test dev-deps

