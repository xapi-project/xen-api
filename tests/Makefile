include ../config.mk



all: copy_file close_all_fds_except unlink watch_bug dotdot

copy_file: copy_file.cmo
	$(OCAMLC) -linkpkg -custom -o $@ $<

close_all_fds_except: close_all_fds_except.cmx
	$(OCAMLOPT) -linkpkg -o $@ $<

unlink: unlink.cmo
	$(OCAMLC) -linkpkg -custom -o $@ $<

watch_bug: watch_bug.cmo
	$(OCAMLC) -linkpkg -custom -o $@ $<

dotdot: dotdot.cmo
	$(OCAMLC) -linkpkg -custom -o $@ $<

clean:
	rm -f *.o *.so *.a *.cmo *.cmi *.cma *.cmx *.cmxa *.annot $(SPOT) copy_file

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $<
%.cmx: %.ml
	$(OCAMLOPT) -c -o $@ $<

