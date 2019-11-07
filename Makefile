
OCB= ocamlbuild  -use-ocamlfind

all:    native byte

clean:
	$(OCB) -clean

run:    native
	./semantics.native

native:
	$(OCB) semantics.native

byte:
	$(OCB) semantics.byte




.PHONY: all clean byte native run 