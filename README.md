## mirage-fs — MirageOS signatures for filesystem devices

[![Build Status](https://travis-ci.org/mirage/mirage-fs.svg?branch=master)](https://travis-ci.org/mirage/mirage-fs)

mirage-fs provides the `[Mirage_fs.S][fs]` and `[Mirage_fs_lwt.S]` signatures
the MirageOS filesystem devices should implement.

[fs]: http://mirage.github.io/mirage-fs/Mirage_fs.html
[fslwt]: http://mirage.github.io/mirage-fs/Mirage_fs_lwt.html

## Installation

mirage-fs can be installed with `opam`:

    opam install mirage-fs

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
mirage-fs`.

[doc]: https://mirage.github.io/mirage-fs/

## License

mirage-fs is distributed under the ISC license, see the [LICENSE.md] file for its text.

[LICENSE.md]: ./LICENSE.md
