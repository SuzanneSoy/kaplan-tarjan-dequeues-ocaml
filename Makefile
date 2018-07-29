.PHONY: all
all: deques deques.mli Makefile

deques: deques.ml Makefile
	ocamlc $< -o $@

deques.mli: deques.ml Makefile
	ocamlc -i $< > $@

.PHONY: print-tool-versions
print-tool-versions:
	ocamlc -version
