.PHONY: clean

OCB = ocamlbuild
LLVM = -pkgs llvm

all: clean pyni

clean:
	$(OCB) -clean
	rm -rf testall.log *.diff *.out *.ll .*.un~

test1: 
	$(OCB) test1.native

test2:
	$(OCB) test2.native

pyni:
	$(OCB) $(LLVM) pyni.native
