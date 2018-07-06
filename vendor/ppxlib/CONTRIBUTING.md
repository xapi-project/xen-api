This repository contains core libraries and tools used to develop ppx
rewriters. The code was originally developed and is still maintained
and used by [Jane Street][js].

This repository is not the first piece of open source software
released by Jane Street, however it is the first to be entirely
developed on github. We are hoping that opening the development of
this repository will help collaboration with other open source users.

We welcome contributions and we will be happy to add contributors,
given that they are motivated to help maintain and grow the
project. However, given how important this code is to the functioning
of Jane Street, we do require that at least one Jane Street developer
reads every pull request that modifies the source code.

Additionally, all contributors must sign our [Contributor License
Agreement][CLA], except for very simple contributions.

### Developing patches

We ask that patches changing the code respect the overall coding
style. In particular, the code should be indented using
[ocp-indent][ocpi]. Additionally the test suite should pass on the
contributor's machine before a patch is submitted for review.

Note that in addition to the normal dependencies, you need to install
[cinaps][cinaps] in order to modify the code. This is because some
parts of the code are auto-generated and committed in the repository.

So before submitting a PR, make sure to check all the following
points:

- all the modified code is correctly indented according to ocp-indent
- `make` succeeds
- `make test` succeeds

### Submitting patches and code review

Once a patch is ready according to the criteria stated in the
previous section, it should be submitted via the github website. When
submitting a pull request, we prefer if you tick the `Allow edits from
maintainers` box as it is much simpler to fix typos or do simple
improvements directly rather than go back and forth through the web
interface.

[js]:     https://opensource.janestreet.com/
[CLA]:    https://janestreet.github.io/contributing.html
[ocpi]:   https://github.com/OCamlPro/ocp-indent
[cinaps]: https://github.com/janestreet/cinaps
