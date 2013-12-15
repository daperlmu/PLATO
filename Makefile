# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = Ast.mli Sast.mli JavaAst.mli Logger.ml Parser.mly Scanner.mll PlatoLibraryStrings.ml PLATO.ml

# the name of the resulting executable
RESULT = platoc

# paths to additional libraries
#LIBS = oUnit
#INCDIRS = /usr/local/lib/ocaml/site-lib/oUnit

# generate type information (.annot files)
ANNOTATE = yes

# additional files to delete
TRASH = *.log, *.java

# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile
