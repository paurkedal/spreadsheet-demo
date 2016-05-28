.PHONY: doc test all install uninstall clean distclean

prefix = $(shell opam config var prefix)

all:
	ocaml pkg/pkg.ml build
clean:
	ocamlbuild -clean

doc: all
test: all

install:
	opam-installer --prefix $(prefix) spreadsheet-demo.install

uninstall:
	opam-installer --prefix $(prefix) -u spreadsheet-demo.install

distclean: clean
	rm -f spreadsheet-demo.install
