v0.9.1 2017-10-16 Zagreb
------------------------

- Make `topkg build` return a non-zero exit code when the build
  fails. Thanks to Etienne Millon for the patch.
- Improve `topkg doc` for `jbuilder` users. Thanks to Thomas
  Gazagnaire for the patch.
- `ocamlbuild` users: default to parallel builds. This can
  be controlled via the `--jobs` command line argument. Thanks
  to Edwin Török for the patch.

v0.9.0 La Forclaz (VS)
----------------------

- Deprecate `--pinned` in favor of `--dev-pkg` in the `build` command.
  The semantics is the same and with opam < 2.0 it should still be set
  to `"%{pinned}%"`. With opam >= 2.0 it should be set to `"%{dev}%"`
  this allows to infer the correct build context for (non-pinned) VCS
  packages (#79).
- Improve `ocamlbuild` cross-compilation support. Adds the
  `Conf.toolchain` configuration key. If specified on the command line
  or via the `TOPKG_CONF_TOOLCHAIN` environment variable, its value is
  used with the `-toolchain` option introduced in `ocamlbuild` 0.11.0
  in default build command `Pkg.build_cmd`. If unspecified the default
  build command is left unchanged. Thanks to whitequark for the patch.
- Add the `--raw ARG` repeteable option to the `build` command. Allows
  to skip package build instructions and opam install file generation
  to simply invoke the package build command with the `ARG` argument.
- `topkg doc` (ocamlbuild specific). Build the documentation using
  the package `build` command and `--raw` arguments. Avoids problems
  encountered by packages that use ocamlbuild plugins (#80).
- Add the `cma`, `cmxa` and `cmxs` optional arguments to `Pkg.mllib`.
  These allow to precisely specify what you (don't) want to build. They
  all default to `true`. Thanks to Stephen Dolan for the suggestion.
- Add `Pkg.lib_root` and `Pkg.libexec_root` install fields. Warning
  these are opam 2.0 only fields.
- Change `test` command for multi-opam packages by mirroring the way
  the `build` command works. The `--pkg-name` or `-n` option specifies
  the package's test to run or list. If unspecified the default
  package is tested, before `pkg/pkg.ml test` would list and run
  the last built package. This means that if you have `pkg/pkg.ml
  build -n PKG && pkg/pkg test` invocations you need to turn them into
  `pkg/pkg.ml build -n PKG && pkg/pkg test -n PKG`.
- Fix `topkg run`, do not run `.so` and `.cmxs` files.
- Fix changelog parsing. Subsections of an entry were not being properly
  parsed (#103).
- Fix `topkg opam pkg`'s `url` file generation for github users which
  have `dev-repo:` with opam "version control bound" uris (#106).
- Depends at least on `cmdliner.1.0.0` and `opam-format` (`opam-lib`
  is out).

v0.8.1 2016-11-02 Zagreb
------------------------

- Add `Pkg.{nothing,flatten}`. Thanks to David Kaloper Meršinjak for the
  suggestion and the patch.


v0.8.0 2016-10-31 Zagreb
------------------------

- Add `Conf.debugger_support` a configuration key to inform to the
  build system it should build and install build artefacts for
  debuggers. Packages using `Pkg.{mlib,clib}` descriptions will handle
  this automatically. The key can be set globally in a switch via the
  `TOPKG_CONF_DEBUGGER_SUPPORT` environment variable (#77).
- Add `Pkg.{ocb_tag,ocb_bool_tag,ocb_bool_tags}` to easily extend
  `ocamlbuild` invocations according to the build configuration (#78). Thanks
  to David Kaloper Meršinjak for the idea and the patch.
- Add `Exts.interface` for installing `mli` only compilation units (#74).
- `Pkg.mllib` description. Correct support for mllib which have subpaths (#75).
- Documentation generation. Fix support in the presence of `ocamlbuild`
  plugins (#80). Thanks to David Kaloper Meršinjak for the report
  and the patch.
- Documentation generation. If there is no `doc/style.css` but `odig` is
  installed, use its `ocamldoc` stylesheet. This allows to see how it will
  be rendered by `odig` and avoids maintaining stylesheets in repos.

v0.7.9 2016-09-21 Zagreb
------------------------

- Better package parsing in `ocamlbuild` _tags files. Thanks
  to Thomas Gazagnaire for the report.
- Remove references to internal names in the API.

v0.7.8 2016-08-09 Zagreb
------------------------

- Add a `--profile` configuration key. Thanks to David Kaloper
  Meršinjak for the patch.
- Add `OS.File.write_subst`. Allows clients to substitute watermark
  like variables in hooks.
- Add `Pkg.{build,clean}_cmd`. Allows clients to extend the default
  build system invocation.
- Be more quiet on package builds (log the `.install` file
  written message at info level).
- Remove Topkg_care.Browser. Depend on the `webbrowser` package
  instead.

v0.7.7 2016-07-13 Cambridge (UK)
--------------------------------

- Test description, allow to specify a working directory
  for the test via the `?dir` optional argument. Thanks
  to Thomas Gazagnaire for suggesting.
- Fix behaviour of pinned distribution watermarking when
  git repo is not at the root directory of the package and
  `--vcs true` is forced.
- Fix pkg/pkg.ml's main's ignoring `TOPKG_VERBOSITY`'s value.
- Toy GitHub delegate: fix log verbosity propagation.
- Fix `Vcs.is_dirty` to detect untracked files. Thanks to Hannes Mehnert.
- Pager, do not try to discover if `TERM` variable is undefined.
- Add `topkg run` to easily run built executables.

v0.7.6 2016-07-01 Cambridge (UK)
--------------------------------

- Add `pkg/pkg.ml clean` command. Removes the opam install file
  and performs an effect that can be specified via `clean` in the
 `Pkg.build` description. The `topkg clean` command now simply forwards
  to this command.
- Change the signature of the build command `cmd` in the `Pkg.build`
  specification. This is an API breaking change but does not affect any
  published package. See #53 for details.
- Add `Conf.OCaml.word_size`. Reports the bit word size for
  the programs that are produced by a given compiler.
- Build configuration key parsing. Fail hard on any error instead
  of warn and continue (#56). Thanks to Thomas Gazagnaire for suggesting
  the previous idea was a terrible one.
- Add a `--debug` configuration key. Defaults to `true` or the value
  of the `TOPKG_CONF_DEBUG` environment variable. The default build
  system invocation is changed to enable save of debugging information
  in build artefacts if they key is `true`. The key is generally not
  meant to be specified by packagers so that the policy can changed in
  bulk over topkg packages (#54).
- `Pkg.files_to_watermark` default function. Make sure only files are
  returned (#58). Fixes problems with symlinks to directories in git
  checkout. Thanks to Thomas Gazagnaire for reporting.
- Improve error message of some `Topkg.OS` functions (#57).
- Remove deprecated `--installer` configuration key.
- `topkg lint` fix regression in error opam lint report.

v0.7.5 2016-06-22 Cambridge (UK)
--------------------------------

- `topkg doc` add short option `-d` for `--dev`.
- Fix `Pkg.mllib`, module list was lowercased rather than uncapitalized.

v0.7.4 2016-06-17 Cambridge (UK)
--------------------------------

- Add test description and run support. New `topkg test` command.
- Add distribution publication description support. Allows to define the
  set of default publication artefacts in the package description. The cli
  syntax of `topkg publish` for alternative artefacts changes from
  `alt KIND` to `alt-KIND`.
- Distributed (and thus installed) opam files are now properly
  versioned via the `version:` field.
- Improve tarball reproducibility across systems by not relying on the
  VCS checkout state for determining the read and write rights (#43).
- opam package submission: use the `opam-publish` submit message
  to append the release notes to the submission.
- Toy GitHub delegate: improve user authentication by trying to steal
  an existing opam-publish token.
- Toy GitHub delegate: improve package documentation publication. Thanks
  to Thomas Gazagnaire for the patches.
- Error message and IPC logging level propagation improvements. Thanks to
  Thomas Gazagnaire for the help.

v0.7.3 2016-06-12 Cambridge (UK)
--------------------------------

- Change pin build detection (#44). This changes opam build
  instruction for packages. Substitute `"--installer" "true"` by
  `"--pinned" "%{pinned}%"` in build instructions. The
  `--installer` option is deprecated and has no effect.

v0.7.2 2016-06-02 Cambridge (UK)
--------------------------------

- `Pkg.describe`, allow multiple readme, change log and license files.
  The optional arguments `readme`, `change_log` and `license` become
  `readmes`, `change_logs`, `licenses` with the same default. When
  topkg needs to act on a change log (e.g. `topkg log`) or readme
  (e.g. `topkg opam descr`), it acts on the first element of
  `change_log` and/or `readmes`.

- Fix `Conf.vcs` discovery to only look for a git
  directory in the build directory (#42).

v0.7.1 2016-05-26 Arbaz (VS)
----------------------------

- Improve Windows support. Thanks to Andreas Hauptmann for the help.

v0.7.0 2016-05-22 La Forclaz (VS)
---------------------------------

First release. 
