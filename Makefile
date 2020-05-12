.PHONY: clean

OCB = ocamlbuild

clean:
	$(OCB) -clean
	rm -rf *.out .*.un~
