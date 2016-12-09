There are two ways of building and installing the repository:

* Installing as an OPAM package using the
  [`opam-repo-dev`](https://github.com/xapi-project/opam-repo-dev) OPAM
  repository, possibly in combination with the `opam` file in this repository.

* Creating a Docker image with all the dependencies pre-installed using
  [`xs-shipyard`](https://github.com/lindig/xs-shipyard), and then running
  `make` to build the project, followed by `make install` to install it

Please see the READMEs of the above repositories for detailed build
instructions.
