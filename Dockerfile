FROM unikernel/mirage
COPY xen-gnt-unix.opam /src/xen-gnt-unix.opam
RUN opam install depext -y
RUN opam pin add xen-gnt-unix /src -n
RUN opam depext xen-gnt-unix -y
RUN opam install xen-gnt-unix --deps-only

