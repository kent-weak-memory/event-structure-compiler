EVC = ../src/compile.byte
EVC_FLAGS = --alloy-path ../../alloy
RUN_ALLOY = ../runalloy.sh
SOLVER = plingeling
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
	jctc20 \

SOURCE_FILES = $(addsuffix .jef,$(TESTS))
ALLOY_FILES = $(addsuffix .als,$(TESTS))
OUTPUT_FILES = $(addsuffix .xml,$(TESTS))

.PHONY: all
all: $(OUTPUT_FILES)

%.als: %.jef
	$(EVC) $(EVC_FLAGS) $< $@ || \
		echo "Failed to compile" > ../xml/$(subst .als,,$@)/error.out 

%.xml: %.als
	mkdir -p ../xml/$(subst .als,,$<)/
	@ cd ../alloystar && \
			SOLVER=$(SOLVER) $(RUN_ALLOY) ../tests/$< 0 ../xml/$(subst .als,,$<)/ $(OS) > ../xml/$(subst .als,,$<)/alloy.out || \
			echo "No instance found" > ../xml/$(subst .als,,$<)/result


clean:
	rm -rf $(ALLOY_FILES)
	rm -rf ../xml
