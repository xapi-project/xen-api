set -e
set -x

#obus-gen-interface ../vm.xml 
#obus-gen-server ../vm.xml

obus-gen-interface -o vm ../vm.xml 
obus-gen-interface -o resource ../resource.xml
obus-gen-interface -o controller ../controller.xml

PACKS=obus,lwt.syntax

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c vm.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c vm.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c resource.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c resource.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c controller.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c controller.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c simulator.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -o simulator -linkpkg vm.cmo resource.cmo controller.cmo simulator.cmo

