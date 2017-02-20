.PHONY: $(ALLOY_BIN) wickersonComparator all

ALLOY_BIN = alloystar/dist/alloy4.2.jar
COMPARATOR_BIN = ./comparator.native
COMPILER_BIN = ./src/compile.native

$(ALLOY_BIN):
	opam install xml-light
	git submodule update --init --recursive
	cd alloystar && git checkout extra_features
	make -C alloystar

$(COMPARATOR_BIN):
	make -C comparator

$(COMPILER_BIN): wickersonComparator
	make -C src

clean:
	make -C src clean
	make -C comparator clean
	make -C alloystar clean
