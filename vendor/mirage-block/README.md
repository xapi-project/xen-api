Block implementations for mirage
================================

This repo contains generic operations over Mirage `BLOCK` devices.

Please consult [the API documentation](https://mirage.github.io/mirage-block/index.html).

Example usage
-------------

In a top-level like utop:
```ocaml
# #require "io-page.unix";;
# #require "mirage-block";;
# #require "mirage-block-ramdisk";;
# #require "lwt.syntax";;

# lwt t_or_error = Ramdisk.create ~name:"hello" ~size_sectors:1024L ~sector_size:512;;
val t_or_error : [ `Error of Ramdisk.error | `Ok of Ramdisk.t ] = `Ok <abstr>

# let t = Mirage_block.Error.ok_exn t_or_error;;
val t : Ramdisk.t = <abstr>

# let page = Io_page.(to_cstruct (get 1));;
val page : Ramdisk.page_aligned_buffer =
  {Cstruct.buffer = <abstr>; off = 0; len = 4096}

# lwt result_or_error = Ramdisk.read t 0L [ page ];;
val result_or_error : [ `Error of Ramdisk.error | `Ok of unit ] = `Ok ()

# lwt ok_or_error = Mirage_block.sparse_copy (module Ramdisk) t (module Ramdisk) t;;
val ok_or_error :
  [ `Error of [> `Different_sizes | `Is_read_only | `Msg of bytes ]
  | `Ok of unit ] = `Ok ()
```
