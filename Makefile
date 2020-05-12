.PHONY: clean

OCB = ocamlbuild

clean:
	$(OCB) -clean
	rm -rf *.out .*.un~

test1: 
	$(OCB) test1.native

test2:
	$(OCB) test2.native
