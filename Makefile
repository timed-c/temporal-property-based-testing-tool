RESULT = generate-tfg
SOURCES = \
  src/tfg.mli src/tfg.ml \
  src/program.mli src/program.ml \
  src/main.ml
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
