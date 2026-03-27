## v4.24.0 (17-Jan-2024)
- unix: really_read now retries reads on EINTR
- std: add Listext.List.find_minimum

## v4.23.0 (30-Oct-2023)
- unix: fix blkgetsize return type mismatch (CA-382014)
- unix: add function to recursively remove files

## v4.22.0 (24-May-2023)
- date, pervasive, std: remove deprecated code
- encodings: Optimize XML_UTF8.is_valid: avoid allocating an int32 for each unicode codepoint

## v4.21.0 (29-Nov-2022)
 - unix: add permissions to write_{bytes,string}_to_file
 - Use a dune version with fixed metadata generation 
 - threads, unix: avoid using C functions deprecated in OCaml 5
 - Avoid warnings and add the check to detect them to the CI
 - zerocheck: remove wrong, unused code. It was dangerous to leave it available

## v4.20.0 (17-Nov-2022)
 - date: consolidate the types into a single t
 - date: add conversion functions that have semantic meaning, the previous functions containing 'float' and 'string' will be deprecated in a future release.

## v4.19.0 (17-Jun-2022)
 - maintenance: give a name to the project
 - threads: Remove all the modules except Mutex
 - Add license to opam metadata, remove unused opam files

## v4.18.0 (15-Jun-2021)
 - CP-31119: Enable documentation upload
 - CP-31119: Prepare to generate documentation
 - CP-34643: Prepare doc comments for odoc
 - CP-34643: Reorder functions in listext interface
 - CP-34643: drop deprecated methods from listext
 - unix: remove unused stdext-std dependency

## v4.17.0 (01-Mar-2021)
 - listext: avoid traversing list twice on assoc_default
 - maintenance: format with ocamlformat
 - maintenance: prepare for ocamlformat
 - CP-34643: listext: add drop function, rework some functions
 - CP-34643: add unit tests for listext
 - CP-34643: Listext: deprecate functions in Stdlib.List
 - CP-34643: listext: remove implementations for functions in Stdlib.List

## v4.16.0 (29-Dec-2020)
 - ci: remove travis workflow
 - Create ocaml-ci.yml
 - date: allow timezones other than UTC for printing
 - XSI-894 date.iso8601.to_float should assume UTC

## v4.15.0 (14-Dec-2020)
 - XSI-894 handle iso8601's with no timezone
 - maintenance: format xstringext files with ocamlformat
 - xapi-stdext-std: Do not duplicate functions from Stdlib
 - CP-34643: add tests for xstringext
 - maintenance: reformat pervasivesext with ocamlformat
 - CP-34643: Deprecated non-idiomatic pervasivesext functions
 - unixext: remove Fdset module and stubs

## v4.14.0 (11-Aug-2020)
 - CP-33121: Move encodings test to the package directory
 - CP-33121: remove dependency of date in encodings tests

## v4.13.0 (11-Aug-2020)
 - CA-342171 allow clients to create an iso8601 from localtime

## v4.12.0 (24-Jul-2020)
 - CP-33121: run encodings tests as part of the encodings package
 - maintenance: update travis config
 - maintenance: prepare for ocamlformat
 - CP-33121: remove obsoleted modules and packages

## v4.11.0 (24-Apr-2020)
 - CA-338243 remove legacy variant in iso8601

## v4.09.0 (23-Apr-2020)
 - CA-338243 iso8601.to_string backwards compatibility

## v4.8.0 (15-Apr-2020)
 - CA-333908 accept YYYY-MM-DD date format
 - unixext: better description for write___to_file
 - fixup! CP-32686: Ensure durability with atomic_write_to_file
 - fixup! CP-32686: Ensure durability with atomic_write_to_file
 - maintenance: whitespace
 - CP-32686: Ensure durability with atomic_write_to_file
 - ci: use environment vars from xs-opam
 - ci: do do not pin base64, it doesn't exist

## v4.7.0 (04-Jun-2019)
 - CP-30756: Remove Base64

## v4.6.0 (02-Apr-2019)
- CA-314001: release runtime lock around long running system calls

## v4.5.0 (13-Mar-2019)
 - Update .travis.yml
 - CA-310525 fix C binding for statvfs

## 4.4.1 (21-Jan-2019)
 - Replaced jbuild files with dune.

## 4.4.0 (05-Jul-2018): -- xapi-stdext-pervasives only
* CA-292641: Use Logs to log cleanup exn instead of shadowing the original one with it

## 4.3.0 (30-May-2018):
* CP-28365: improve backtraces by using finally

## 4.2.0 (25-May-2018): -- xapi-stdext-unix only
* unixext: update interface to mimick the ocaml Unix one

## 4.1.0 (25-Apr-2018): -- xapi-stdext-unix only
* really_write:
	- use single_write_substring and avoid an unsafe coercion
	- remove deprecation and make robust against EINTR
* unixext_open_stubs: fix use of uninitialised variable

## 4.0.0 (15-Mar-2018):
* Make safe-string safe (xap-stdext-{bigbuffer, encodings, std, threads, unix} 1.1.0)
* Remove bigbuffer from the default stdext set of packages
* Use backward compatible naming for stdext xapi-stdext

## 3.0.0 (02-Aug-2017):
* Remove unused packages
* Refactor in a backward compatible wrapper and 12 new separate libraries (see https://github.com/xapi-project/stdext/pull/21)
* Port to jbuilder

## 2.1.0 (20-Oct-2016):
* New Semaphore module

## 2.0.0 (22-Jun-2016):
* Namespace everything under Stdext. This is a backwards incompatible change.

## 0.13.0 (20-Nov-2014):
* Depend on Backtrace from backtrace
* Add an opam file

## 0.12.0 (26-Sep-2014):
* Fix build errors on OS X

## 0.11.0 (30-May-2013):
* Change Stringext module to Xstringext to avoid conflict with other packages

## 0.9.1 (10-Sep-2013):
* Add Unixext.domain_of_addr
* Add String.sub_{before,after}

## 0.9.0 (3-Jun-2013):
* first public release
