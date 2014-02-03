set -e
set -x

obus-gen-interface ../ping_pong.xml 
obus-gen-server ../ping_pong.xml

obus-gen-interface -o ping_pong ../ping_pong.xml 


PACKS=obus,lwt.syntax

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c ping_pong.mli
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c ping_pong.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -c ping.ml
ocamlfind ocamlc -syntax camlp4o -package $PACKS -c pong.ml

ocamlfind ocamlc -syntax camlp4o -package $PACKS -o ping -linkpkg ping_pong.cmo ping.cmo
ocamlfind ocamlc -syntax camlp4o -package $PACKS -o pong -linkpkg ping_pong.cmo pong.cmo

