.PHONY: clean

OCB = ocamlbuild
LLVM = -pkgs llvm

clean:
	$(OCB) -clean
	rm -rf *.out .*.un~

test1: 
	$(OCB) test1.native

test2:
	$(OCB) test2.native

pyni:
	$(OCB) $(LLVM) pyni.native
