0.3.0
-----

- Update the AST to 4.06 (#8, @xclerc)

- Deprecate old references to type_conv in argument and rewriter names
  and add new ones mentioning deriving instead (#7, #9 @xclerc)

- Fix compatibility with `-safe-string` (#10, @hhugo)

- Restore tests (#11, @xclerc)

- Allow to set the suffix of corrected files (#15, @diml)

- Restore compatibility with OCaml 4.04.x (#16, @xclerc)

0.2.0
-----

- Make sure to import command line arguments registered with
  ocaml-migrate-parsetree (#5, @diml)

- Fix an issue where cookies set from the command line sometimes
  disappeared (#6, @diml)

0.1.0
-----

Initial release.
