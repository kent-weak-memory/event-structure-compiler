.PHONY: all $(ALLOY_BIN) $(COMPILER_BIN)

ALLOY_BIN = alloystar/dist/alloy4.2.jar
COMPARATOR_BIN = ./comparator.native
COMPILER_BIN = ./src/compile.native

all: $(ALLOY_BIN) $(COMPILER_BIN)

$(ALLOY_BIN):
	opam install xml-light
	git submodule update --init --recursive
	cd alloystar && git checkout extra_features
	make -C alloystar

$(COMPILER_BIN):
	make -C src

test:
	make -C tests

clean:
	make -C src clean
	rm -f $(ALLOY_BIN)
