v2.3.5 2017-07-15
-----------------

* Remove unnecessary dependency on mirage-unix from the packages.

v2.3.4 2017-06-17
-----------------

* Depend on OCaml >= 4.03.0 and above
* Add dependency on io-page-xen

v2.3.3 2017-06-15
-----------------

* Remove `mli` only modules in favour of `ml` ones instead.
  This squelches some warnings from jbuilder and makes it safer
  against future changes (if for example an exception is defined
  in the mli file, it wont work).

* Depend on Cstruct 3 for the package naming of cstruct-unix.

v2.3.2 2017-06-11
-----------------

* Set `(wrapped false)` in mirage-console-xen

v2.3.1 2017-06-11
-----------------

* Add missing xenstore.client to mirage-console-xen-backend
* Add missing `Console_xen` to mirage-console-xen

v2.3.0 2017-06-01
-----------------

* Build with jbuilder and release with topkg
* Update to new shared-memory-ring-lwt dependency

v2.2.1 2017-05-24
-----------------

* Link to `cstruct.lwt` explicitly rather than implicitly for the unix package.

v2.2.0 2016-12-21
-----------------

* Import `V1.CONSOLE` from `mirage-types` into `Mirage_console.S` and create
  a new `mirage-console` opam package (@samoht
- Import `V1_LWT.CONSOLE` from `mirage-types-lwt` into `Mirage_console_lwt.S`
  and create a new `mirage-console-lwt` opam package (@samoht

v2.1.3 2015-08-03
-----------------

* Fix installation of Unix library defaults by splitting out the
  base Unix dependency and the Xenctrl ones.  This needs a new `--enable-xenctrl`
  flag that explicitly depends on the Xen libraries being installed. (#36
* Install the `mirage-console-cli` executable if it is built.

v2.1.2 2015-03-08
-----------------

* Add an explicit `connect` function to interface. (#34
* Modernise Travis scripts with central sourcing.
* Only build Unix executable if relevant `xenctrl` libraries are installed.

v2.1.1 2015-01-23
-----------------

* Add an `error_message` function to turn an `error` into a string.

v2.1.0 2014-12-23
-----------------

* [xen]: for secondary consoles, Console.connect blocks in a watch waiting for
  a hotplug. This allows stand-alone console servers time to connect (such as
  xentropyd

v2.0.0 2014-10-31
-----------------

* enable travis (for both xen and unix cases
* fix dependencies: drop mirage-{xen,unix}; keep dependencies on implementation
  libraries and mirage-types
* switch build to OASIS
* add command-line tool to attach consoles
* add experimental support for named consoles
* add support for reading from consoles (so we can do user interaction
* [xen] support connecting to additional (named) consoles
* [xen] don't zero the initial console ring
* install findlib packages as `mirage-console.[xen/unix]`

v1.0.2 2014-02-01
-----------------

* [xen] Fix console on resume by reattaching the ring.
* [xen] Switch to ocamlfind xen-{gnt,evtchn}.

v1.0.1 2013-12-08
-----------------

* Set the type of `id` in the Xen console to `string`.

v1.0.0 2013-12-08
-----------------

* Adapt to mirage-types-0.4.0 interface.

v0.9.9 2013-12-07
-----------------

* Install separate libraries for `mirage-console-unix` and `mirage-console-xen`.
* Update library dependencies for mirage-types-0.3.0
* Adapt to V1.CONSOLE interface.
* Initial public release, based on mirage/mirage-platform#0.9.8
