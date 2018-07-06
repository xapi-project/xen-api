re_scan: re_scan.ml ../re.cmxa
	ocamlopt -o re_scan -I .. re.cmxa re_scan.ml

pcre_scan: pcre_scan.ml
	ocamlopt -o pcre_scan -I +contrib pcre.cmxa pcre_scan.ml

re_match: re_match.ml ../re.cmxa
	ocamlopt -o re_match -I .. re.cmxa re_match.ml

pcre_match: pcre_match.ml
	ocamlopt -o pcre_match -I +contrib pcre.cmxa pcre_match.ml

unison3: unison3.ml
	ocamlopt -o unison3 unix.cmxa -I +contrib pcre.cmxa unison3.ml

unison: unison.ml ../re.cmxa
	ocamlopt -o unison unix.cmxa -I .. re.cmxa unison.ml

unison2: unison2.ml
	ocamlopt -o unison2 unix.cmxa -I ../../unison/src re.cmx unison2.ml

clean:
	rm -f re_scan pcre_scan re_match pcre_match unison unison2 unison3
	rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o
