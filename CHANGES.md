## 2.0.0 (2017-12-06):
* Fix build with OCaml 4.04 (and `-safe-string`)
* Remove unnecessary dependency on `ocamlfind` and make `ounit` a test
  dependency
* Ensure the CI runs the unit tests

## 1.4.0 (2017-06-08):
* Add terminating replacements for transaction function
* Switch to jbuilder
* Release with topkg
* Use Docker in travis

## 1.3.0 (2016-03-14):
* Add EEXIST exception to the interface
* Allow Ocaml xenstore clients to receive oversized replies from xenstored
* Return the task for a wait asynchronously
* Increase maximum incoming watch limit from 1024 to 65536
* Don't leak watch strings in clients
* Add opam file
* Ensure errors from wait functions aren't lost
* Fix a non-tail call in the dispatcher
* Switch to ppx from camlp4

## 1.2.5 (2013-10-04):
* Add Travis continuous integration scripts
* fix a spurious EQUOTA failure when processing transactions

## 1.2.4 (2013-09-11):
* fix watches

## 1.2.3 (2013-08-27):
* export a signature, which can be satisfied by both Unix userspace
  and xen kernelspace clients

## 1.2.2 (2013-08-06):
* Generate documentation with make doc.
* Documentation improvements/uniformization.
* Xs_handle rewritten in a more functional style.
* Function names now are equal to mirage ones.
* Renamed Xs_client to Xs_client_lwt.

## 1.2.1 (2013-03-15):
* Drop packets which are too large

## 1.2.0 (2013-02-08):
* Use the latest cstruct >=0.6.0 API.

## 1.1.0 (2012-12-15):
* Add suspend/resume support to the Xenstore client

## 1.0.0 (2012-10-05):
* Initial public release.
