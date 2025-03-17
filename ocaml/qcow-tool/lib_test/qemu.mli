(*
 * Copyright (C) 2016 Unikernel Systems
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

(** Wrappers for qemu-img to allow us to compare the contents of
   ocaml-qcow images and qemu-produced images. *)

module Img: sig

  val create: string -> int64 -> unit
  (** [create path size] creates a qcow2 format image at [path] with size [size] *)

  val check: string -> unit
  (** [check path] runs "qemu-img check" on the given qcow2 image. *)

  type info = {
    virtual_size: int64;
    filename: string;
    cluster_size: int;
    actual_size: int;
    compat: string;
    lazy_refcounts: bool option;
    refcount_bits: int option;
    corrupt: bool option;
    dirty_flag: bool;
  }

  val info: string -> info
  (** [info path] returns metadata associated with the given qcow2 image. *)
end
