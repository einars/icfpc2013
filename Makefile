SOURCES = src/program.ml src/tester.ml src/main.ml
RESULT = bleach

.SILENT:

-include OCamlMakefile


run:
	./bleach

