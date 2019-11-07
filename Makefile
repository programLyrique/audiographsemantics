
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

test:
	$(OCB) tests.native
	./tests.native


.PHONY: all clean byte native run test