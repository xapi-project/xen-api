# OCaml-migrate-parsetree
Convert OCaml parsetrees between different major versions

This library converts between parsetrees of different OCaml versions.

Supported versions are 4.02, 4.03, 4.04, 4.05 and 4.06.
For each version, there is a snapshot of the parsetree and conversion functions
to the next and/or previous version.

## Asts

```ocaml
module Ast_402, Ast_403, Ast_404, Ast_405, Ast_406, Ast_407 : sig

  (* These two modules didn't change between compiler versions.
     Just share the ones from compiler-libs. *)
  module Location = Location
  module Longident = Longident

  (* Version specific copy of AST *)
  module Asttypes
  module Parsetree
  module Outcometree

  (* Other modules that are useful for implementing PPX.

     Docstrings and Ast_mapper only contain general definitions
     In particular, the internal state used by compiler-libs has been
     removed.
     Also equalities are lost for abstract types (Docstring.docstring).  *)
  module Docstrings
  module Ast_helper
  module Ast_mapper

  (* Magic numbers used for marshalling *)
  module Config : sig
    val ast_impl_magic_number : string
    val ast_intf_magic_number : string
  end
end
```

These embed copies of AST definitions for each supported OCaml major version.

The AST matching the version of the OCaml toolchain will contain equalities
relating the copy of types to the definitions from compiler-libs.  For
instance, when installed with OCaml 4.04.x, `Ast_404.Parsetree` looks
like.

## Migration modules

For each pair of versions `$(n)` and `$(n+1)`, the two modules
`Migrate_parsetree_$(n)_$(n+1)` and `Migrate_parsetree_$(n+1)_$(n)` convert the AST forward and backward.

The forward conversion is total while the backward conversion is partial: when
a feature is not available in a previous version of the parsetree, a
`Migrate_parsetree_def.Migration_error` exception is raised detailing the
failure case.

`Migrate_parsetree_versions` abstract versions of the compiler. Each version is
represented as a module with `OCaml_version` signature.  Instances are named
`OCaml_402`, `OCaml_403`, ... `OCaml_current` is an alias to the version of the
current compiler.
The `Convert` functor takes two versions of OCaml and produce conversion
functions.

Finally, the `Migrate_parsetree_ast_io` provides an easy interface for
marshalling/unmarshalling.

## Migrate_parsetree.Driver

The `Migrate_parsetree.Driver` provides an API for ppx rewriters to
register OCaml AST rewriters. Ppx rewriters using this API can be used
as standalone rewriter executable or as part of a _driver_ including
several rewriters.

Using a single driver for several rewritings has the advantage that it
is faster. Especially when using many ppx rewriters, it can speed up
compilation a lot.

If using [Jbuilder](https://github.com/janestreet/jbuilder), you can
consult the Jbuilder manual to see how to define and use ppx
rewriters. Jbuilder automatically creates drivers based on
ocaml-migrate-parsetree on demand.

The rest of this section describes how to do things manually or with
[ocamlbuild](https://github.com/ocaml/ocamlbuild).

## Building a custom driver using ocamlfind

To build a custom driver using ocamlfind, simply link all the ppx
rewriter libraries together with the
`ocaml-migrate-parsetree.driver-main` package at the end:

    ocamlfind ocamlopt -predicates ppx_driver -o ppx -linkpkg \
      -package ppx_sexp_conv -package ppx_bin_prot \
      -package ocaml-migrate-parsetree.driver-main

Normally, ocaml-migrate-parsetree based rewriters should be build with
the approriate `-linkall` option on individual libraries. If one is
missing this option, the rewriter might not get linked in. If this is
the case, a workaround is to pass `-linkall` when linking the custom
driver.

The resulting `ppx` program can be used as follow:

- `./ppx file.ml` to print the transformed code
- `ocamlc -pp './ppx --as-pp' ...` to use it as a pre-processor
- `ocamlc -ppx './ppx --as-ppx' ...` to use it as a `-ppx` rewriter

### Using the ocaml-migrate-parsetree driver with ocamlbuild

The ocaml-migrate-parsetree-ocamlbuild package provides an ocamlbuild
plugin to help building and using custom drivers on demand.

#### Setup

To use it you need to first tell ocamlbuild to use the plugin in
`myocamlbuild.ml`. If you are using oasis, add this to your `_oasis`
file:

```
AlphaFeatures:         ocamlbuild_more_args
XOCamlbuildPluginTags: package(ocaml-migrate-parsetree-ocamlbuild)
```

If you are calling ocamlbuild directly, you need to call it this way:

```
$ ocamlbuild -plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)" ...
```

Once you have done that, you need to enable it in your myocamlbuild.ml:

```ocaml
let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    Migrate_parsetree_ocamlbuild.dispatch hook;
    <other dispatch functions>
  )
```

#### Usage

The plugin provides a new parametric tag: `omp-driver`. The tag takes
as argument a `+` separated list of rewriters (as findlib package
names) followed by any command line arguments.

For instance to use `ppx_sexp_conv` and `ppx_bin_prot` put this in
your tags file:

```
<**/*>: predicate(custom_ppx)
<src/*.{ml,mli}>: omp-driver(ppx_sexp_conv+ppx_bin_prot)
```

The first line is to instruct ocamlfind not to automatically add
implicit `-ppx` argument. Without this, you might still get individual
`-ppx` for both `ppx_sexp_conv` and `ppx_bin_prot` in addition to the
main driver that already contains them both, meaning your code would
be transformed more than it should...

# Development

It started from the work of Alain Frisch in
[ppx\_tools](https://github.com/alainfrisch/ppx_tools).

The library is distributed under LGPL 2.1 and is copyright INRIA.

## Adding a new OCaml version

We use [Cinaps](https://github.com/janestreet/cinaps) to generate boilerplate.
Try `opam install cinaps`.  If it is not available, you might need to pin the
package:
`opam pin add jbuilder --dev-repo`
`opam pin add cinaps https://github.com/janestreet/cinaps.git`

Add the new version in
[src/cinaps.ml](https://github.com/let-def/ocaml-migrate-parsetree/blob/master/src/cinaps.ml)
`supported_versions`.

Snapshot the ast in file "asts/ast\_NEW.ml".
* Define the modules `Location` and `Longident` as aliases to corresponding
  modules from compiler-libs.
* Copy `Asttypes`, `Parsetree`, `Outcometree`, `Docstrings`, `Ast_helper` and
  `Ast_mapper` from the upstream files in `parsing/` directory.
* Global state and definitions referencing external values should be removed
  from `Docstrings` and `Ast_mapper`. Take a look at existing snapshots.
* Create a `Config` module containing `ast_impl_magic_number`
  `ast_impl_magic_number` from upstream `Config`
* Call `tools/add_special_comments.native` on the file

Add migration functions:
- Manually compile the ast (`ocamlc -c ast_NEW.ml`)
- Using `gencopy` from [ppx\_tools](https://github.com/alainfrisch/ppx_tools), generate copy code to and from previous version (assuming it is 404):
```
gencopy -I . -map Ast_404:Ast_NEW Ast_404.Parsetree.expression Ast_404.Parsetree.toplevel_phrase Ast_404.Outcometree.out_phrase > migrate_parsetree_404_NEW_migrate.ml
gencopy -I . -map Ast_NEW:Ast_404 Ast_NEW.Parsetree.expression Ast_NEW.Parsetree.toplevel_phrase Ast_NEW.Outcometree.out_phrase > migrate_parsetree_NEW_404_migrate.ml
```
- Fix the generated code by implementing new cases
- By default generated code use very long identifiers, simplify unambiguous ones (e.g. `copy_Ast_NEW_Parsetree_structure` -> `copy_structure`). The migration functor expects specific names, look at `Migrate_parsetree_versions` interface.

*TODO*: specialize and improve gencopy for these cases

Add mapper lifting functions in the files `migrate_parsetree_NEW_404.ml` and
`migrate_parsetree_404_NEW.ml`:
- include the corresponding `Migrate_parsetree_40x_40y_migrate` module
- define `copy_mapper` function, look at existing `Migrate_parsetree_40x_40y`
  for guidance.

At any time, you can expand boilerplate code by running `make cinaps`.

Update build system:
- in [Makefile](Makefile), add "src/ast\_NEW.ml" to `OCAML_ASTS` and migration modules to `OBJECTS`
- make sure `make cinaps` reaches a fixed point :)
- `make` should succeed
