SOURCES = src/helpers.ml src/server.ml src/program.ml src/guesser.ml src/tester.ml src/main.ml
RESULT = bleach

PACKS = netclient extlib yojson
#INCDIRS = ~/.opam/system/lib/ocamlnet

.SILENT:

#all: native-code
all: debug-native-code

-include OCamlMakefile


run:
	./bleach

prune:
	find cache -type f -mtime '-5' -print -delete
