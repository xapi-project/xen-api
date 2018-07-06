v1.3.0 2017-03-15 La Forclaz (VS)
---------------------------------

- Add `Xmlm.pp_{dtd,name,attribute,tag,signal}`
- Safe-string support.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v1.2.0 2013-09-06 Cambridge (UK) 
--------------------------------

- `Xmlm.output`, illegal XML Unicode characters in Data signals or 
  attribute values are output as U+FFFD (thanks to David Sheets for
  insisting that something should be done about that).
- Deprecate the ability to IO multiple documents from the same
  IO abstraction.
- Deprecate the functorial interface.
- OPAM friendly workflow and drop OASIS support.
      

v1.1.1 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.


v1.1.0 2012-03-16 La Forclaz (VS) 
---------------------------------

- OASIS support.
- Fixes a bug in the UTF-16 decoder.
- Fixes a bug in `Xmlm.make_output` with a custom function. Thanks to
  Konstantinas Myalo for the report and the patch.
- New optional argument `decl` to `Xmlm.make_output` to control whether the 
  XML declaration should be output.
- New function `Xmlm.output_depth`, returns the current element nesting level.


v1.0.2 2009-11-11 大足县
-----------------------

- Replaced a (non tail-recursive) use of `List.map`.


v1.0.1 2008-08-01 Lausanne
----------------------------

- POSIX compliant build shell script (thanks to Michael D Ekstrand).
- Support for Debian packaging.


v1.0.0 2008-03-17 Lausanne
----------------------------

## New features:
- Streaming IO api with support to IO arborescent data structures.
- Proper XML namespace support, all names are expanded names.
- Whitespace stripping respects the xml:space attributes.
- Xmlm.Make functor to use other types for strings and internal buffers.  
- UTF-8 encoded documents can start with an UTF-8 encoded BOM.

## Incompatible changes:
- `Xmlm.encoding` becomes a polymorphic variant.
- `Xmlm.error` becomes a polymorphic variant and the "E_" prefix is dropped.
- Removed the callback api.
- Removed the tree and cursor api.

## Other:
- `test/xhtml.ml` has a mapping from XHTML entities to their UTF-8 sequence.
- Build system switched from make to ocamlbuild 


v0.9.0 2007-02-26 Lausanne
--------------------------

- First release.
