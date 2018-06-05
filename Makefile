OCB = ocamlbuild

all: native

clean:
	$(OCB) -clean

native: depend
	$(OCB) Main.native

depend:
	ocamldep *.ml > .depend

include .depend
