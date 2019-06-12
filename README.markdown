rrdd: the XAPI project's performance monitoring daemon
------------------------------------------------------

`rrdd` runs in the dom0 of a Xen host, and periodically collects numerical
performance data. This data is archived to disk in the
[RRD](http://en.wikipedia.org/wiki/Round-Robin_Database) format, and can be
retrieved over HTTP.

`rrdd` also implements a plugin mechanism, by which other processes can
register additional datasources to be collected.


# Installation

The easiest way to install is via opam:

    opam switch create new-env
    opam repo remove default
    opam repo add xs-opam https://github.com/xapi-project/xs-opam.git
    opam install xcp-rrdd
