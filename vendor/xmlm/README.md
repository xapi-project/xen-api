Xmlm — Streaming XML codec for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Xmlm is a streaming codec to decode and encode the XML data format. It
can process XML documents without a complete in-memory representation of the
data.

Xmlm is made of a single independent module and distributed
under the ISC license.

Home page: http://erratique.ch/software/xmlm  

## Installation

Xmlm can be installed with `opam`:

    opam install xmlm

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated
from the source interfaces. It can be consulted [online][doc]
or via `odig doc xmlm`.

[doc]: http://erratique.ch/software/xmlm/doc/Xmlm

## Sample programs

If you installed xmlm with `opam` sample programs are located in
the directory `opam config var xmlm:doc`.


In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 

The `xmltrip` tool reads XML files with Xmlm and outputs them back in
various ways. It is useful to understand how Xmlm handles
documents. `xmltrip -help` has more information.

If you need to parse XHTML, the file [`xhtml.ml`](test/xhtml.ml) in
the `test` directory has an OCaml list coupling each XHTML character
entity with its corresponding UTF-8 encoded character string. You can
use it to program a suitable entity callback.
