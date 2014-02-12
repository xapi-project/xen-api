set -e
set -x

#obus-gen-interface ../vm.xml 
#obus-gen-server ../vm.xml

obus-gen-interface -o vm ../org.xenserver.Vm.xml 
obus-gen-interface -o resource ../org.xenserver.Resource.xml
obus-gen-interface -o controller ../org.xenserver.Controller.xml
obus-gen-interface -o task_interfaces ../org.xenserver.Task.xml
obus-gen-client -o task ../org.xenserver.Task.xml
obus-gen-interface -o taskOwner ../org.xenserver.TaskOwner.xml

PACKS=obus,lwt.syntax,uri,re.str

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c vm.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c vm.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c resource.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c resource.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c controller.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c controller.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c task_interfaces.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c task_interfaces.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c task.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c task.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c taskOwner.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c taskOwner.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c simulator.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -o simulator -linkpkg vm.cmo resource.cmo controller.cmo task_interfaces.cmo task.cmo taskOwner.cmo simulator.cmo

