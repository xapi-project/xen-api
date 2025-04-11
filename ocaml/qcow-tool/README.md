Ocaml support for Qcow2 images
==============================

[![Build Status](https://travis-ci.org/mirage/ocaml-qcow.png?branch=master)](https://travis-ci.org/mirage/ocaml-qcow) [![Coverage Status](https://coveralls.io/repos/mirage/ocaml-qcow/badge.png?branch=master)](https://coveralls.io/r/mirage/ocaml-qcow?branch=master)

Please read [the API documentation](https://mirage.github.io/ocaml-qcow/).

Features
--------

- supports `resize`
- exposes sparseness information
- produces files which can be understood by qemu (although not in
  reverse since we don't support many features)

Example
-------

In a top-level like utop:
```ocaml
# #require "io-page.unix";;
# #require "mirage-block";;
# #require "mirage-block-ramdisk";;
# #require "qcow";;
# #require "lwt.syntax";;

# lwt t_or_error = Ramdisk.create ~name:"hello" ~size_sectors:1024L ~sector_size:512;;
val t_or_error : [ `Error of Ramdisk.error | `Ok of Ramdisk.t ] = `Ok <abstr>

# let t = Mirage_block.Error.ok_exn t_or_error;;
val t : Ramdisk.t = <abstr>

# module Qcow_on_ramdisk = Qcow.Make(Ramdisk);;
module Qcow_on_ramdisk :                                                          sig                                                                               type page_aligned_buffer = Ramdisk.page_aligned_buffer
    type error =
        [ `Disconnected | `Is_read_only | `Unimplemented | `Unknown of bytes ]
    type 'a io = 'a Ramdisk.io
    type t = Qcow.Make(Ramdisk).t
    type id = Qcow.Make(Ramdisk).id
    val disconnect : t -> unit io
    type info =
      Qcow.Make(Ramdisk).info = {
      read_write : bool;
      sector_size : int;
      size_sectors : int64;
    }
    val get_info : t -> info io
    val read :
      t ->
      int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
    val write :
      t ->
      int64 -> page_aligned_buffer list -> [ `Error of error | `Ok of unit ] io
    val create : Ramdisk.t -> int64 -> [ `Error of error | `Ok of t ] io
    val connect : Ramdisk.t -> t io
    val resize : t -> int64 -> [ `Error of error | `Ok of unit ] io
    val seek_unmapped : t -> int64 -> [ `Error of error | `Ok of int64 ] io
    val seek_mapped : t -> int64 -> [ `Error of error | `Ok of int64 ] io
    val rebuild_refcount_table : t -> [ `Error of error | `Ok of unit ] io
    val header : t -> Qcow.Header.t
    module Debug :
      sig
        type t = Qcow.Make(Ramdisk).t
        type error = error
        val check_no_overlaps : t -> [ `Error of error | `Ok of unit ] io
        val set_next_cluster : t -> int64 -> unit
      end
  end

# lwt t_or_error = Qcow_on_ramdisk.create t 1048576L;;
val t_or_error : [ `Error of Qcow_on_ramdisk.error | `Ok of Qcow_on_ramdisk.t ]
  = `Ok <abstr>

# let t = Mirage_block.Error.ok_exn t_or_error;;
val t : Qcow_on_ramdisk.t = <abstr>

# let page = Io_page.(to_cstruct (get 1));;
val page : Ramdisk.page_aligned_buffer =
  {Cstruct.buffer = <abstr>; off = 0; len = 4096}

# lwt result_or_error = Qcow_on_ramdisk.read t 0L [ page ];;
val result_or_error : [ `Error of Ramdisk.error | `Ok of unit ] = `Ok ()

# lwt ok_or_error = Mirage_block.sparse_copy (module Ramdisk) t (module Ramdisk) t;;
val ok_or_error :
  [ `Error of [> `Different_sizes | `Is_read_only | `Msg of bytes ]
  | `Ok of unit ] = `Ok ()
```

Limitations
-----------

- cluster size is fixed at 64-bits
- no support for snapshots
