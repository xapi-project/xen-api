Package structure
-----------------

This repo is the base of two opam packages:

- `topkg`, with opam file [topkg.opam](topkg.opam)
- `topkg-care` with opam file [topkg-care.opam](topkg-care.opam)

Both share the same [pkg/pkg.ml](pkg/pkg.ml) file. `topkg` simply
builds the `Topkg` library and `topkg-care` builds the `Topkg_care`
library and the `topkg` command line tool. The distinction is made in
the `pkg.ml` file based on the package name passed to the build
instructions.

The reason for this structure is that while both `Topkg` and `Topkg_care`
could be distributed together and `Topkg_care` be simply build
depending on optional dependencies being present, this would have a
fatal flaw: `topkg` cannot be used for any of `Topkg_care`'s
dependencies. Since we want to use `topkg` for these dependencies
aswell, this structure allows to cut the dependency cycle that would
occur.

So if you want to develop `topkg` you should do a:

```
opam pin add -kgit topkg topkg#master
opam pin add -kgit topkg-care topkg#master
```

Changing the Topkg API
----------------------

Is a *very* delicate thing to do as it could break packages. Here's an
invocation that can be used to reinstall packages that build depend on
`topkg`:

```
opam reinstall $(opam list -s --installed --depends-on topkg)
```
