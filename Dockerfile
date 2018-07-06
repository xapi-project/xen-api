FROM ocaml/opam:alpine
ENV OPAMJOBS 8
WORKDIR /src
COPY . /src
RUN opam remote add mirage-dev git://github.com/mirage/mirage-dev
RUN opam pin add mirage-block-unix /src -n
RUN opam depext -u -i mirage-block-unix
RUN opam install mirage-block-unix -y
