Xapi Project's XenAPI Management Toolstack
==========================================

[![Build Status](https://travis-ci.org/xapi-project/xen-api.svg?branch=master)](https://travis-ci.org/xapi-project/xen-api)
[![Coverage Status](https://coveralls.io/repos/github/xapi-project/xen-api/badge.svg?branch=master)](https://coveralls.io/github/xapi-project/xen-api?branch=master)
[![Lines of Code](https://tokei.rs/b1/github/xapi-project/xen-api)](https://github.com/xapi-project/xen-api)

Xen API (or xapi) is a management stack that configures and controls
Xen-enabled hosts and resource pools, and co-ordinates resources
within the pool. Xapi exposes the Xen API interface for many
languages and is a component of the XenServer project.
Xen API is written mostly in [OCaml](http://caml.inria.fr/ocaml/)
4.04.2

Xapi is the main component produced by the Linux Foundation's
[Xapi Project](http://xenproject.org/developers/teams/xapi.html).

Build and Install
-----------------

The build install instructions are currently being written. The Xapi
Project contains a large list of dependencies and sub-projects, which
are actually quite difficult to build independently. To build xen-api
from source, we recommend using [opam](https://opam.ocaml.org/doc/Manual.html) with the [xs-opam](https://github.com/xapi-project/xs-opam) remote (detailed explanation in [readme](https://github.com/xapi-project/xs-opam/blob/master/README.md).

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
