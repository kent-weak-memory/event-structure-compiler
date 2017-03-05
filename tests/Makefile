EVC = ../src/compile.byte
EVC_FLAGS =
EVC_ALLOY_FLAGS = --alloy-path ../../alloy
EVC_DOT_FLAGS = --long-names
RUN_ALLOY = ../runalloy.sh
SOLVER = glucose
OS = amd64-linux

TESTS = jctc1 \
	jctc2 \
	jctc3 \
	jctc4 \
	jctc5 \
	jctc6 \
	jctc7 \
	jctc8 \
	jctc9 \
	jctc10 \
	jctc11 \
	jctc12 \
	jctc13 \
	jctc14 \
	jctc15 \
	jctc16 \
	jctc17 \
	jctc18 \
	jctc19 \
	jctc20

SOURCE_FILES = $(addsuffix .jef,$(TESTS))
ALLOY_FILES = $(addsuffix .als,$(TESTS))
ISABELLE_FILES = $(addsuffix .thy,$(TESTS))
DOT_FILES = $(addsuffix .dot,$(TESTS))
PNG_FILES = $(addsuffix .png,$(TESTS))
ALLOY_TESTS = $(addsuffix .xml,$(TESTS))
OUTPUT_FILES = $(ISABELLE_FILES)
TIMEOUT = 3600 # 1 hour timeout

.PRECIOUS: $(ALLOY_FILES) $(ISABELLE_FILES)

.PHONY: all
all: $(OUTPUT_FILES)
isabelle: $(ISABELLE_FILES)
graphviz: $(PNG_FILES)
alloy: $(ALLOY_FILES)
alloy_test: $(ALLOY_TESTS)

%.als: %.jef
	mkdir -p ../xml/$(subst .als,,$@)/
	$(EVC) $(EVC_FLAGS) $(EVC_ALLOY_FLAGS) $< $@ || \
		echo "Failed to compile" > ../xml/$(subst .als,,$@)/error.out

%.dot: %.jef
	- $(EVC) $(EVC_FLAGS) $(EVC_DOT_FLAGS) $< $@

%.thy: %.jef
	- $(EVC) $(EVC_FLAGS) $< $@

%.png: %.dot
	- dot -Tpng $< -o $@

%.xml: %.als
	mkdir -p ../xml/$(subst .als,,$<)/
	@cd ../alloystar && SOLVER=$(SOLVER) timeout $(TIMEOUT) $(RUN_ALLOY) ../tests/$< 0 ../xml/$(subst .als,,$<) $(OS) || echo "Timeout" > ../xml/$(subst .als,,$<)/result


clean:
	rm -rf $(ALLOY_FILES) $(ISABELLE_FILES) $(DOT_FILES) $(PNG_FILES)
	rm -rf ../xml
