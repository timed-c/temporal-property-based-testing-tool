DIRS = src

.PHONY: all

# Init submodules if needed and make native version.
# The resulting executable can be found under /bin and /library (symlinks)
all: tfg program main native

# Compile native version.
tfg:
	@rm -f -r bin
	@mkdir bin
	@ocamlbuild -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) tfg.cmx
	@ocamlbuild   -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) tfg.cmxa
	@rm -f bytes.ml

program:
	@ocamlbuild -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) program.cmx
	@ocamlbuild   -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) program.cmxa
	@rm -f bytes.ml

main:
	@rm -f -r libs
	@mkdir libs
	@ocamlbuild -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) main.cmx
	@ocamlbuild   -no-hygiene -cflags '-w -a' -use-ocamlfind -pkgs 'cil,yojson,csv' -Is $(DIRS) main.cmxa
	@rm -f bytes.ml

native:
	@ocamlbuild -cflags '-w -a' -no-hygiene  -use-ocamlfind -pkgs 'cil,yojson,csv'  -Is $(DIRS) main.native
	@rm -f main.native
	@cp _build/src/main.native generate-tfg
	@cd bin;cp ../_build/src/main.native generate-tfg

clean:
	@rm -f -r libs
	@rm -f -r _build
	@rm -f -r bin 
	@rm -f -r generate-tfg
