v0.8.5 2017-12-27 La Forclaz (VS)
---------------------------------

* Fix `Fmt.{kstrf,strf_like}` when they are partially applied
  and repeatedly called. Thanks to Thomas Gazagnaire for the report.
* Add `Fmt.comma`.
* Relax the `Fmt.(invalid_arg, failwith)` type signature. Thanks to
  Hezekiah M. Carty for the patch.

v0.8.4 2017-07-08 Zagreb
------------------------

* Add `Fmt.{invalid_arg,failwith}`. Thanks to Hezekiah M. Carty for the patch.


v0.8.3 2017-04-13 La Forclaz (VS)
---------------------------------

* Fix `Fmt.exn_backtrace`. Thanks to Thomas Leonard for the report.

v0.8.2 2017-03-20 La Forclaz (VS)
---------------------------------

* Fix `META` file.

v0.8.1 2017-03-15 La Forclaz (VS)
---------------------------------

* `Fmt_tty.setup`, treat empty `TERM` env var as dumb.
* Add `Fmt.Dump.uchar` formatter for inspecting `Uchar.t` values.

v0.8.0 2016-05-23 La Forclaz (VS)
---------------------------------

* Build depend on topkg.
* Relicense from BSD3 to ISC.
* Tweak `Fmt.Dump.option` to indent like in sources.
* Add `Fmt.Dump.signal` formatter for `Sys` signal numbers.
* Add `Fmt[.Dump].result`, formatter for `result` values.
* Add `Fmt.{words,paragraphs}` formatters on US-ASCII strings.
* Add `Fmt.exn[_backtrace]`. Thanks to Edwin Török for suggesting.
* Add `Fmt.quote`.
* Rename `Fmt.text_range` to `Fmt.text_loc` and simplify output
  when range is a position.

v0.7.1 2015-12-03 Cambridge (UK)
--------------------------------

* Add optional cmdliner support. See the `Fmt_cli` module provided
  by the package `fmt.cli`.


v0.7.0 2015-09-17 Cambridge (UK)
--------------------------------

First Release.
