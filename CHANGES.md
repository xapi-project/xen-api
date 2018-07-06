## v0.9.0 (2017-11-25)

- preliminary support for Cstruct.t-backed tar processing (#54 by @hcarty)
- fix build with OCaml 4.06.0 (and `-safe-string`)

## v0.8.0 (2017-05-09)

- split into 3 packages: `tar`, `tar-unix`, `tar-mirage`
- use jbuilder for building
- add support for reading @LongLink headers
- mark deprecated functions with @@ocaml.deprecated
- fix some warnings

## v0.7.1 (2017-02-03)

- convert build system to topkg (#43, @hannesm)

## v0.7.0 (2017-01-19)

- Build against MirageOS version 3, and drop support for earlier versions.
- Support only OCaml versions 4.03 and higher.

## v0.6.1 (2016-09-30)

- fix a bug in the key=value interface when the archive isn't a multiple
  of 4KiB in size

## v0.6.0 (2016-09-19)

- support for pax headers
- removed Tar.Archive.fold: please use HeaderReader instead

## v0.5.1 (2016-08-30)

- handle EINTR and short writes properly (@ivg)
- avoid a warning catching `Failure` exceptions from `int_of_string`

## v0.5.0 (2016-04-24)

- now requires cstruct >= 1.9.0 and OCaml 4.02+

## v0.4.2 (2016-04-22)

- test: only run tests if mirage-block-unix is present
- improve the opam file
- travis: simplify the configuration

## v0.4.1 (2015-07-21)

- fix Tar_mirage when using block devices with < 4096 byte
  sectors

## v0.4.0 (2015-07-19)

- add tar.mirage in ocamlfind, containing Tar_mirage which
  exposes a BLOCK device as a KV_RO

## v0.3.0 (2015-04-06)

- add Tar.Make functor which allows easier integration with
  camlzip
- always initialise tar header unused bytes to 0 (previously
  would use uninitialised data)
- modernise Travis CI scripts to use OPAM 1.2 workflow.

## v0.2.1 (2013-11-15)

- Re-add some old deprecated functions

## v0.2.0 (2013-10-13)

- Add 'Tar.Archive.fold' for folding over entries in an archive

## v0.1.1 (2013-10-03)

- Rename ocamlfind package from 'ocaml-tar' to simply 'tar'

## v0.1.0 (2013-10-03)

- Initial release
