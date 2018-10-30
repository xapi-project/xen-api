# XenAPI Software Development Kit

[![Build Status](https://travis-ci.org/xapi-project/xen-api-sdk.svg?branch=master)](https://travis-ci.org/xapi-project/xen-api-sdk)
[![Lines of Code](https://tokei.rs/b1/github/xapi-project/xen-api-sdk)](https://github.com/xapi-project/xen-api-sdk)

This repository contains the generation code for the XenAPI Software Development
Kit (SDK) and its associated documentation and examples.

The SDK consists of five components, one for each of C, C#, Java, PowerShell,
and Python, exposing the individual XenAPI calls as first-class functions in the
target language.

The generation code is written in [OCaml][1].

## Build

### 1. Environment setup

Install [opam][2]. It is also recommended to install one of the listed external
solvers (e.g. aspcud).

Ensure you are using version 4.06.1 of the OCaml compiler:
`opam switch 4.06.1`

and you have the package depext: `opam install depext`

_Note: if you want to avoid setting up the environment by yourself, you can use
one of the ready containers from [opam-dockerfiles][3], which come with a
preinstalled OPAM environment and OCaml compiler._

### 2. Install dependencies

Add [xs-opam][4] as a remote Opam repository (follow the instructions in that
repo).

Then, to install the dependencies of the SDK, which include Xapi, run

```bash
   opam depext -y xen-api-sdk
   opam install -y --deps-only xen-api-sdk
```

This will install first the external dependencies of this package and of all
its dependencies, and then all the OPAM packages required for building it.

Finally, copy [this file][5] into the root of the xen-api-sdk repo.

### 3. SDK generation

Run `make <lang>`, where `<lang>` one or more of `c`, `java`, `csharp`,
`powershell`, to generate SDK source code for the specified programming
language(s), or simply `make` to generate SDK source code for all the supported
languages.

To compile the generated source code, follow the instructions in the corresponding
README files. The third party libraries required for the compilation of the C#
and PowerShell source code can be obtained from [dotnet-packages][6].

_Note: The Python module is not auto-generated. It can be found at
[XenAPI.py][7]._

## Contributions

The preferable way to contribute patches is to fork the repository on Github
and then submit a pull request. If for some reason you can't use Github to
submit a pull request, then you may send your patch for review to the
<xen-api@lists.xen.org> mailing list, with a link to a public git
repository for review. Please see the [CONTRIB.md][8] file for some general
guidelines on submitting changes.

## Maintainers

* Konstantina Chremmou <konstantina.chremmou@citrix.com>
* Mihaela Stoica <mihaela.stoica@citrix.com>

## License

This code is licensed under the BSD 2-Clause license. Please see the file
[LICENSE][9] for more information.

[1]: http://caml.inria.fr/ocaml/
[2]: https://opam.ocaml.org/
[3]: https://github.com/ocaml/opam-dockerfiles
[4]: https://github.com/xapi-project/xs-opam
[5]: https://github.com/xapi-project/sm/blob/master/drivers/XE_SR_ERRORCODES.xml
[6]: https://github.com/xenserver/dotnet-packages
[7]: https://github.com/xapi-project/xen-api/blob/master/scripts/examples/python/XenAPI.py
[8]: CONTRIB.md
[9]: LICENSE
