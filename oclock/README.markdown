Oclock: Precise POSIX clock for OCaml

Mickaël Delahaye

This module give access to the `clock_gettime (2)` family of functions to Ocaml
programs.

Requirements
============

Just the usual suspects: GNU Make, GCC, OCaml, and optionally Findlib (i.e.,
ocamlfind).

Installation
============

    $ make
Build the library

    # make install
Install the library in the standard ocaml directory

Usage
=====

Manually:
    ocamlc -I +oclock oclock.cma ...
    ocamlopt -I +oclock oclock.cmxa ...

Or with ocamlfind:
    ocamlfind ocamlc -package oclock ...
    ocamlfind ocamlopt -package oclock ...

Documentation
=============

To build the API documentation in HTML, use:
    $ make doc
Then, open `doc/index.html` with your favorite browser.

Two examples are also provided in the `examples` directory:

*   `getcputime` gets the CPU-time consumed by a process given by its PID.
*   `realtime` gets the real time since the Epoch, and evaluates the clock
    precision inside Ocaml.

License
=======
Copyright (c) 2011, Mickaël Delahaye <mickael.delahaye@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
