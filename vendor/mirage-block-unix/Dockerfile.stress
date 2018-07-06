FROM ocaml/opam:alpine as build
ENV OPAMJOBS 8
RUN opam remote add mirage-dev git://github.com/mirage/mirage-dev
RUN opam pin add diet git://github.com/djs55/ocaml-diet -n
RUN opam depext -u -i mirage-block-unix
RUN opam install mirage-block-unix -y
RUN opam install ounit diet -y
WORKDIR /src
COPY . /src
RUN opam pin add mirage-block-unix /src -n
USER 0
RUN opam config --root /home/opam/.opam exec -- sh -c 'jbuilder build lib_test/stress.exe'

FROM alpine
COPY --from=build /src/_build/default/lib_test/stress.exe /

