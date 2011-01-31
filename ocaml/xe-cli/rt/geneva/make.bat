
ocamlc -c utils.ml
ocamlc -c cli_utils.ml
ocamlc -c cli_test.ml
ocamlc -o test.exe unix.cma utils.cmo cli_utils.cmo cli_test.cmo
