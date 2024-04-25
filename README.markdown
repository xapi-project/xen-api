Xapi Project's XenAPI Management Toolstack
==========================================

![Build](https://github.com/xapi-project/xen-api/actions/workflows/main.yml/badge.svg?branch=master)

Xen API (or xapi) is a management stack that configures and controls
Xen-enabled hosts and resource pools, and coordinates resources
within the pool. Xapi exposes the Xen API interface for many
languages and is a component of the XenServer project.
Xen API is written mostly in [OCaml](http://caml.inria.fr/ocaml/)
4.07.

Xapi is the main component produced by the Linux Foundation's
[Xapi Project](http://xenproject.org/developers/teams/xapi.html).

Build and Install
-----------------

To build xen-api from source, we recommend using [opam](https://opam.ocaml.org/doc/Manual.html) with the [xs-opam](https://github.com/xapi-project/xs-opam) repository (further explanation in its [readme](https://github.com/xapi-project/xs-opam/blob/master/README.md)).

1) Install `opam` and `git` with your package manager.

2) Clone this repo and work from its root.

    ```bash
    git clone https://github.com/xapi-project/xen-api && cd xen-api
    ```

3) Figure out which version of ocaml-base-compiler to use.

    - Go to [this xs-opam file](https://raw.githubusercontent.com/xapi-project/xs-opam/master/tools/xs-opam-ci.env), and look for `OCAML_VERSION_FULL`.
    - Run that line, e.g.:

        ```bash
        export OCAML_VERSION_FULL="4.14.1"
        ```

4) Setup opam with your environment (i.e. switch).

    ```bash
    opam init
    opam switch create xen-api ocaml-base-compiler.$OCAML_VERSION_FULL
    # This basically "jumps you into" the environment you just created:
    eval $(opam env --switch=xen-api --set-switch)
    ```

5) Get the Recommended Packages.

    ```bash
    # Add the xs-opam library as the main repo to check for versions at:
    opam repo add xs-opam https://github.com/xapi-project/xs-opam.git
    # Remove the default, because how it handles version conflicts is different:
    opam repo remove default
    # (NOT needed with opam>=2.1.0) Have opam now figure out what versions of each package to use:
    opam pin --yes add . --no-action
    ```

6) Install all the Packages.

    ```bash
    opam install xs-toolstack
    # Update the current switch. (You're already on the correct one, just refresh it).
    eval $(opam env)
    ```

7) Build `xen-api`.

    ```bash
    ./configure
    make
    make test
    ```

The binaries should now be in `./_build/install/default/bin`!

Working From a Fork
-------------------
If you are working from within a clone of a fork of this repository, you will
need tags from the upstream repository in order to produce a build with the
correct versioning string.

To fetch these tags, you must ensure that this repository is known to `git` (as a
remote, often called `upstream`) and then you can fetch the tags as follows:

```bash
git remote add upstream https://github.com/xapi-project/xen-api
git fetch upstream --tags
```

You can check if this has been successful by invoking `git describe`.

You can then push these tags to your remote repository to ensure they are cloned
in future:

```bash
git push origin --tags
```

Contributions
-------------

To contribute patches to xen-api, please fork the repository on
Github, and then submit a pull request. If for some reason you can't
use Github to submit a pull request, then you may send your patch for
review to the [xen-api@lists.xenproject.org mailing list](http://www.xenproject.org/help/mailing-list.html), with a link to a
public git repository for review. We much prefer Github pull requests,
however, and submitting a patch to the mailing list will take much
more time for review.

Maintainers
-----------

Maintainers can be contacted via this mailing list: `xen-api@lists.xenproject.org`

Licensing
---------

This software is licensed under the GNU LGPLv2 license. Please see the
LICENSE file for more information.
