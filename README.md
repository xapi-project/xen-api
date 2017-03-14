A slightly easier interface to Xenstore than that provided by the ocaml-xenstore and
ocaml-xenstore-clients libraries. This library is good if you want a single persistent
connection to xenstored for your process.

Example
-------

```ocaml
# #require "ezxenstore";;
# #require "ezxenstore";;
# Xs_transport.xenstored_socket := "/home/jludlam/devel/ocaml-xenstore-server/mysocket";;
# with_xs (fun xs -> xs.write "/bar" "baz");;
```
