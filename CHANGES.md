## 2.8.0 (2017-06-01)

* Port to Jbuilder (#65 @vbmithr @avsm).
  There should be no observable changes, except that `Ipaddr_unix` is now
  in a separate subdirectory. This means that packages that implicitly
  depended on the module without including the ocamlfind `ipaddr.unix`
  package may now fail. Just adding the ocamlfind dependency will fix it,
  and is backwards compatible with older Ipaddr releases.
* Minimum version of OCaml required is now 4.03.0 (formerly was 4.02.2),
  due to the use of recent `ppx_sexp_conv` with Jbuilder also having that
  as the minimum supported compiler version.

## 2.7.2 (2017-02-16)

* Fix a missing findlib toploop package (#61 from Vincent Bernardoff)

## 2.7.1 (2016-11-16)

* Use topkg for build (#60 from Jochen Bartl)

## 2.7.0 (2016-02-14)

* Remove `sexplib.syntax`, `type_conv` deps and camlp4 transitive dependency
* Add `ppx_sexp_conv` dependency
* Require OCaml 4.02.2+
* Add `Ipaddr.Prefix.subset`, `Ipaddr.V4.Prefix.subset` and `Ipaddr.V6.subset`
  predicates to test containment of subnets (#52 from @seliopou)

## 2.6.1 (2015-02-20)

* Fix findlib requires in oasis to restore pre-4.02.1 compatibility

## 2.6.0 (2015-02-19)

* Change IPv6 link-local address prefix from fe80::/10 to fe80::/64. (#39)
* Remove type bytes = string alias (potentially breaking)
* Turn on -safe-string (#41)
* {V4,V6}.`to_bytes_raw` now uses Bytes.t rather than string (potentially breaking)
* Add multicast MAC conversions from RFC 1112 and RFC 2464
* Add `to_domain_name` conversions to DNS label lists (in-addr.arpa and ip6.arpa)
* Add `V6.interface_routers`, `V6.site_routers`, and `V6.Prefix.solicited_node`
* Add `V6.link_address_of_mac` to convert a MAC into a link local IP address

## 2.5.0 (2014-05-27)

* Add `with sexp` (de)serializers to all of the Ipaddr and Macaddr types. (#31)

## 2.4.0 (2014-02-11)

* Add `Ipaddr.V6.Prefix.of_netmask` for conversion from an IPv6
  address/netmask to prefix (useful for some binary interfaces). See #27.
* Add `Ipaddr.V6.Prefix.netmask` to generate a netmask address from a
  prefix (useful for some binary interfaces). See #27.
* Add `Ipaddr.Prefix.network` for generic prefix -> address conversion
* Add `Ipaddr.Prefix.netmask` for generic prefix -> netmask conversion

## 2.3.0 (2014-02-05)

* Add `Ipaddr.V4.Prefix.of_netmask` for conversion from an
  address/netmask to prefix
* Add `Ipaddr.V4.Prefix.netmask` to generate a netmask address from a prefix

## 2.2.0 (2014-01-27)

* Add an [Ipaddr_unix] module to convert to-and-from the standard library.
* Add a toplevel pretty printer in the `ipaddr.top` package.

## 2.1.0 (2014-01-20)

* Add `of_string_raw` to `Ipaddr.V4.Prefix` and `Ipaddr.V6.Prefix`
* Add `of_addr` to `Ipaddr.V4.Prefix` and `Ipaddr.V6.Prefix`
* Add type `('v4,'v6) v4v6` to `Ipaddr` to represent version disjuncts
* Add `Ipaddr.Prefix` module for generic prefix manipulation

## 2.0.0 (2014-01-17)

* Change `Ipaddr.V4.make` to accept `int` rather than `int32` (breaking)
* Add IPv6 support
* Add generic IP address support
* Add type `scope` for classifying address scopes
* Add `Ipaddr.V4.of_string_raw` for parsing inside of larger strings
* Add `Ipaddr.V4.to_int16` and `Ipaddr.V4.of_int16`
* Add `unspecified`, `nodes`, and `routers` constants to `Ipaddr.V4`
* Add `Ipaddr.V4.Prefix.network_address` to put an address into a subnet
* Add `of_address_string_exn`, `of_address_string`, `to_address_string`,
  `to_address_buffer` to `Ipaddr.V4.Prefix` to parse/print combined addr/prefix
* Add `multicast_org`, `multicast_admin`, `multicast_link` subnet constants to
  `Ipaddr.V4.Prefix`
* Add `Ipaddr.V4.scope` to classify IPv4 addresses
* Add `Ipaddr.V4.is_global` and `Ipaddr.V4.is_multicast` predicates
* Add optional `sep:char` argument to `Macaddr.to_string`
* Remove internal use of Scanf.scanf

## 1.0.0 (2013-10-16)

* Add Travis-CI testing scripts.
* Include debug symbols and annot files by default.

## 0.2.3 (2013-09-20)

* Add `Ipaddr.V4.Prefix.bits` function to produce bits of prefix from prefix.

## 0.2.2 (2013-08-07)

* Add `Macaddr.make_local` function to create local unicast MAC
  addresses from an octet generation function.
* Add `Macaddr.get_oui` accessor to extract the Organizationally Unique
  Identifier as an integer.
* Add `Macaddr.is_local` predicate to test for a locally administered address.
* Add `Macaddr.is_unicast` predicate to test for a unicast MAC address.

## 0.2.1 (2013-08-01)
* Add `Ipaddr.V4.any`, `Ipaddr.V4.broadcast`, `Ipaddr.V4.localhost`
  special constant addresses.
* Add `Ipaddr.V4.Prefix.global` (0.0.0.0/0) subnet constant.
* Add `Ipaddr.V4.Prefix.network` function to produce subnet address from prefix.

## 0.2.0 (2013-08-01)
* Add `Macaddr` module for handling MAC-48 (Ethernet) addresses.
* `Ipaddr.Parse_error` now contains both the error condition and the
  failing input.
* Add ocamldoc-compatible comments on all interfaces.

## 0.1.1 (2013-07-31)
* Add loopback and link local addresses to the private blocks.
* Fix build system so Makefile is generated by OASIS.

## 0.1.0 (2013-07-24)
* Initial public release.
* Includes IPv4 and IPv4 CIDR prefix support.
