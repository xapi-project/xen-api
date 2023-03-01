0.4.3
* xenstore_transport.unix is deprecated
* Avoid failing tests in CI without access to xenstore

0.4.2
* Drop Xenctrl.interface_close

0.4.1
* CA-342986: Keep watches after soft reset of a domain
* Update Travis configuration from xs-opam

0.4.0
* Update travis
* Split package up into core and watch sub-packages

0.3.1
* Replaced jbuild with dune files.

0.3.0
* Use dune --profile=release for release target
* Add Travis support
* Update opam file
* Revert "CA-286115, ez_xenstore_watch: make add and remove domain thread safe"
* ez_xenstore_watch: improve as per @lindig comments
* CA-286115, ez_xenstore_watch: make add and remove domain thread safe

0.2.0
* ezxenstore: make safe-string safe

0.1.4
* CA-277850 update ezxenstore.opam (add xenctrl)
* CA-277850 make xenstore_watch a functor

0.1.3
* Integrate xenctrl_uuid, xenstore_watch
* Add xenctrl_uuid from xapi-xenopsd
* Add xenstore_watch.ml from xapi-xenopds

0.1.2
* Port to jbuilder

0.1.1
* Don't install xstest
* Add build-time oasis dependency to opam file

0.1.0
* Initial release

