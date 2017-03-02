PACKAGES = ppx_deriving.show,ppx_deriving.ord,extlib,str

FILES = location virtualMap relation eventStructure tokeniser parser constraints outputAlloy outputIsabelle compile

FILES_ML = $(addsuffix .ml,$(FILES))
FILES_MLI = $(addsuffix .mli,$(FILES))
FILES_CMI = $(addsuffix .cmi,$(FILES))
FILES_CMT = $(addsuffix .cmt,$(FILES))
FILES_CMTI = $(addsuffix .cmti,$(FILES))
FILES_CMO = $(addsuffix .cmo,$(FILES))
FILES_CMX = $(addsuffix .cmx,$(FILES))
FILES_O = $(addsuffix .o,$(FILES))

OCC_FLAGS = -bin-annot

all: ocb-compile.byte ocb-compile.native

ocb-compile.byte :
	ocamlbuild -use-ocamlfind -package $(PACKAGES) compile.byte

ocb-compile.native :
	ocamlbuild -use-ocamlfind -package $(PACKAGES) compile.native

compile.byte : $(FILES_CMO)
	ocamlfind ocamlc $(OCC_FLAGS) -linkpkg -package $(PACKAGES) $(FILES_CMO) -o $@

compile.native : $(FILES_CMX)
	ocamlfind ocamlopt -linkpkg -package $(PACKAGES) $(FILES_CMX) $(OCC_FLAGS) -o $@

%.cmi : %.mli
	ocamlfind ocamlc -package $(PACKAGES) $(OCC_FLAGS) -o $@ -c $<

%.cmo : %.ml
	ocamlfind ocamlc -package $(PACKAGES) $(OCC_FLAGS) -o $@ -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -package $(PACKAGES) $(OCC_FLAGS) -o $@ -c $<

.PHONY : depend

clean :
	ocamlbuild -clean
	rm -f $(FILES_CMI) $(FILES_CMTI) $(FILES_CMT) $(FILES_CMO) $(FILES_CMX) $(FILES_O) compile.byte compile.native

depend :
	ocamlfind ocamldep $(FILES_ML) $(FILES_MLI) > .depend

include .depend
