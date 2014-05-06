Xapi Project's XenAPI Management Toolstack
==========================================

Xen API (or xapi) is a management stack that configures and controls
Xen-enabled hosts and resource pools, and co-ordinates resources
within the pool. Xapi exposes the Xen API interface for many
languages and is a component of the XenServer project.
Xen API is written mostly in [OCaml](http://caml.inria.fr/ocaml/)
3.12.1.

Xapi is the main component produced by the Linux Foundation's
[Xapi Project](http://xenproject.org/developers/teams/xapi.html).

Build and Install
-----------------

The build install instructions are currently being written. The Xapi
Project contains a large list of dependencies and sub-projects, which
are actually quite difficult to build independently. To build xen-api
from source, we recommend using [xenserver-core](https://github.com/xenserver/xenserver-core).

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

This is a short list of people who currently maintain this project.

* Akshay Ramani <akshay.ramani@citrix.com>
* Dave Scott <dave.scott@citrix.com>
* Jerome Maloberti <jerome.maloberti@citrix.com>
* John Else <john.else@citrix.com>
* Jon Ludlam <jonathan.ludlam@citrix.com>
* Mike McClurg <mike.mcclurg@citrix.com>
* Rob Hoes <rob.hoes@citrix.com>
* Siddharth Vinoth Kumar <siddharth.vinothkumar@citrix.com>
* Thomas Sanders <thomas.sanders@citrix.com>
* Zheng Li <zheng.li@citrix.com>

You can usually find the developers hanging out in #xen-api on
freenode. We are also reachable on the xen-api@lists.xenproject.org mailing
list.

Licensing
---------

This software is licensed under the GNU LGPLv2 license. Please see the
LICENSE file for more information.
