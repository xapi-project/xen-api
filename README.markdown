Xapi Project's XenAPI Management Toolstack
==========================================

[![Build Status](https://travis-ci.org/xapi-project/xen-api.svg?branch=master)](https://travis-ci.org/xapi-project/xen-api)
[![Coverage Status](https://coveralls.io/repos/github/xapi-project/xen-api/badge.svg?branch=master)](https://coveralls.io/github/xapi-project/xen-api?branch=master)
[![Lines of Code](https://tokei.rs/b1/github/xapi-project/xen-api)](https://github.com/xapi-project/xen-api)

Xen API (or xapi) is a management stack that configures and controls
Xen-enabled hosts and resource pools, and co-ordinates resources
within the pool. Xapi exposes the Xen API interface for many
languages and is a component of the Citrix Hypervisor project.
Xen API is written mostly in [OCaml](http://caml.inria.fr/ocaml/)
4.07.

Xapi is the main component produced by the Linux Foundation's
[Xapi Project](http://xenproject.org/developers/teams/xapi.html).

Build and Install
-----------------

To build xen-api from source, we recommend using [opam](https://opam.ocaml.org/doc/Manual.html) with the [xs-opam](https://github.com/xapi-project/xs-opam) remote (farther explanation in it's [readme](https://github.com/xapi-project/xs-opam/blob/master/README.md)).

1) Install `opam` and `git` with your package manager.

2) Clone this repo, and work from it's root.

    ```bash
    git clone https://github.com/xapi-project/xen-api && cd xen-api
    ```

3) Figure out what version of ocaml-base-compiler to use.

    - Go to [this xs-opam file](https://raw.githubusercontent.com/xapi-project/xs-opam/master/tools/xs-opam-ci.env), and look for "OCAML_VERSION_FULL"
    - Run that line, i.e:

        ```bash
        export OCAML_VERSION_FULL="4.10.1"
        ```

4) Setup opam, with your Enviornment (i.e switch).

    ```bash
    opam init
    opam switch create xen-api ocaml-base-compiler.$OCAML_VERSION_FULL
    # This basically "jumps you into" the enviornment you just created:
    eval $(opam env --switch=xen-api --set-switch)
    ```

5) Get the Recommended Packages.

    ```bash
    # Add the xs-opam library as the main repo to check for versions at:
    opam repo add xs-opam https://github.com/xapi-project/xs-opam.git
    # Remove the default, because how it handles version conflicts is different:
    opam repo remove default
    # Have opam now figure out what versions of each package to use:
    opam pin --yes add . --no-action
    ```

6) Install all the Packages.

    ```bash
    PACKAGES="xapi-cli-protocol xapi-client xapi-consts xapi-database xapi-datamodel xapi-types xapi xe xen-api-sdk xen-api-client xen-api-client-lwt xen-api-client-async xapi-rrdd xapi-rrdd-plugin xapi-rrd-transport xapi-rrd-transport-utils rrd-transport rrdd-plugin rrdd-plugins rrddump gzip http-svr pciutil safe-resources sexpr stunnel uuid xapi-compression xml-light2 zstd vhd-tool"

    ## Install all the dependances (Including OS):
    opam --yes depext --yes -u $PACKAGES # The first '--yes' is to install depext itself
    ## Install the Packages finally:
    opam install $PACKAGES --yes --deps-only --with-test -v
    # update the current switch. (You're already on the correct one, just refresh it).
    eval $(opam env)
    ```

7) Build `xen-api`.

    ```bash
    ./configure
    make
    make test
    ```

The binaries should now be in `./_build/install/default/bin`!

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
