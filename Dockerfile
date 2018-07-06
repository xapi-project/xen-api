FROM unikernel/mirage
RUN opam remote add live git://github.com/ocaml/opam-repository
RUN opam update
RUN opam upgrade -y
COPY . /src
RUN opam pin add io-page . -y

