RESULT = generate-tfg
SOURCES = \
  tfg.mli tfg.ml \
  program.mli program.ml \
  main.ml
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
