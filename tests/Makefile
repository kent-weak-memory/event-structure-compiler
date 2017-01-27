EVC = ../src/compile.byte

TESTS = ./test.als \
				./MP/mp1.als

%.als : %.jef
	$(EVC) $< $@

all: $(TESTS)

clean: $(TESTS)
	rm -rf $(TESTS)
