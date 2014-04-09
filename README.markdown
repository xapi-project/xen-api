Xapi Project's XenAPI Management Toolstack
==========================================

Xen API (or xapi) is a management stack that configures and controls
Xen-enabled hosts and resource pools, and co-ordinates resources
within the pool. Xapi exposes the Xen API interface for many
languages and is a component of the Xen Cloud Platform (XCP) project.
Xen API is written mostly in [OCaml](http://caml.inria.fr/ocaml/)
3.12.1.

Xapi is the main component produced by the Linux Foundation's
[Xapi Project](http://xenproject.org/developers/teams/xapi.html).

Xen Cloud Platform
------------------

Xen Cloud Platform (XCP) is an open source enterprise-ready server
virtualization and cloud computing platform that is based on the Xapi
Project and the Xen Project Hypervisor. XCP provides network and
storage support, and management tools in a single, tested installable
image. It is essentially a custom build of a CentOS 5.7 environment.

* [XCP Home](http://www.xenproject.org/products/cloudxen.html)

* [XCP Downloads](http://www.xenproject.org/download/xcp/index.html)

* [XCP Community](http://www.xenproject.org/products/xcp/community_and_support.html)

XCP in Debian
-------------

It is also possible to install an older version of Xapi in Debian
Wheezy by doing 'apt-get install xcp-xapi'. Along with some
configuration, this will give you a system that is roughly
functionally equivalent to the Xen Cloud Platform distribution.

We are working on getting the Xapi Project dependencies into other
Linux distributions. Expect to soon be able to 'yum install xapi' on
CentOS 6.4...

Build and Install
-----------------

The build install instructions are currently being written. The Xapi
Project contains a large list of dependencies and sub-projects, which
are actually quite difficult to build independently. To build xen-api
from source, we recommend using opam.

```
opam remote add xapi git://github.com/xapi-project/opam-repo-dev.git
opam update
opam install xapi
```

This will use opam to build and install xapi and xapi's dependencies
into your home directory. You can also pin the xapi package to a local
git clone of xen-api and use opam to build this way. A full
description of how opam works is out of scope for this README, but
more information can be found on
[OCaml Pro's](http://www.ocamlpro.com/products/opam.html) website.

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
