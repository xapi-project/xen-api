## v0.8 (13-Mar-2026)
* Set a license
* Provide a new with_backtraces that prevents printing invalid traces
* Fix losing backtraces when reraising
* Add regression tests

## v0.7 (18-Sep-2018)
* Remove dependency on full sexplib
* Simplify jbuild, quiet warnings, move to dune and update opam dependencies
* jbuild: remove ppx_deriving_rpc from libraries
* Move to dune and update opam dependencies

## v0.6 (16-May-2018)
* Add support for ppx_sexp_conv >= v0.11.0

## v0.5 (04-Aug-2017)
* port to jbuilder

## v0.3 (21-Aug-2015)
* correct ordering
* add rpc to opam
* add doc gen to _oasis

## v0.2 (20-Nov-2014)
* store backtraces as lists of records rather than strings
* change the API for "importing" backtraces from other languages
