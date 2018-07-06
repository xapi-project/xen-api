(*
 * Copyright (C) 2011-2013 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

val sector_size: int
(** Sector size of the vhd virtual disk, in bytes *)

val sector_shift: int
(** [1 lsl sector_shift = sector_size] *)

val cstruct_equal: Cstruct.t -> Cstruct.t -> bool

exception Invalid_sector of int64 (* request *) * int64 (* maximum *)
(** An attempt to access (read/write) an invalid sector *)

val max_disk_size: int64
(** Maximum size of a dynamic disk in bytes *)

module UTF16: sig
  type t = int array

  val to_utf8_exn: t -> string
end

module Feature: sig
  type t = 
    | Temporary (** A temporary disk is a candidate for deletion on shutdown *)

  val to_string : t -> string
end

module Disk_type: sig
  type t = 
    | Fixed_hard_disk
    (** A flat constant-space image *)
    | Dynamic_hard_disk
    (** An image which can grow and shrink as data is added and removed *)
    | Differencing_hard_disk
    (** An image which stores only differences from a base "parent" disk *)

  val to_string : t -> string
end

module Host_OS: sig
  type t =
    | Windows
    | Macintosh
    | Other of int32

  val to_string : t -> string
end

module Geometry: sig
  type t = {
    cylinders : int;
    heads : int;
    sectors : int;
  }

  val to_string : t -> string

  val of_sectors : int64 -> t
end

module Footer: sig
  type t = {
    features : Feature.t list;
    data_offset : int64;
    (** For dynamic and differencing disks, this is the absolute byte offset
        from the beginning of the file to the next structure. For fixed disks
        the value is undefined. *)
    time_stamp : int32;
    (** Creation time in seconds since midnight on January 1, 2000 *)
    creator_application : string;
    (** Name of the application which created the image *)
    creator_version : int32;
    (** Version number of the application which created the image *)
    creator_host_os : Host_OS.t;
    original_size : int64;
    (** size of the virtual disk in bytes at creation time *)
    current_size : int64;
    (** size of the virtual disk in bytes now *)
    geometry : Geometry.t;
    disk_type : Disk_type.t;
    checksum : int32;
    (** one's complement checksum of the footer with the checksum set to 0l *)
    uid : Uuidm.t;
    (** 128-bit UUID *)
    saved_state : bool
    (** true if the virtual machine is in a saved (suspended) state *)
  }

  val create : ?features:Feature.t list ->
    data_offset:int64 ->
    ?time_stamp:int32 ->
    ?creator_application:string ->
    ?creator_version:int32 ->
    ?creator_host_os:Host_OS.t ->
    current_size:int64 ->
    ?original_size:int64 ->
    disk_type:Disk_type.t ->
    ?uid: Uuidm.t ->
    ?saved_state:bool ->
    unit -> t

  val compute_checksum: t -> int32
  (** compute the expected checksum value *)

  val default_creator_application: string
  val default_creator_version: int32

  val sizeof : int
  val marshal : Cstruct.t -> t -> t
  val unmarshal : Cstruct.t -> (t, exn) result

  val to_string: t -> string
end

module Platform_code : sig
  type t = 
    | None
    | Wi2r (** deprecated *) 
    | Wi2k (** deprecated *)
    | W2ru (** UTF-16 relative windows path *)
    | W2ku (** UTF-16 absolute windows path *)
    | Mac  (** Mac OS alias *)
    | MacX (** RFC2396 file URL *)

  val to_string : t -> string
end

module Parent_locator : sig
  type t = {
    platform_code : Platform_code.t;
    platform_data_space : int32;
    (** The number of 512-byte sectors needed to store the platform_data *)
    platform_data_space_original : int32;
    (** The original platform_data_space before automatic correction *)
    platform_data_length : int32;
    (** The length of the platform_data *)
    platform_data_offset : int64;
    (** The absolute offset of the platform_data *)
    platform_data : Cstruct.t;
  }
  val null : t
  (** No parent locator *)

  val equal: t -> t -> bool

  val to_string : t -> string
  val to_filename : t -> string option
  (** Attempt to read a filename from the platform_data *)

  val sizeof : int
  val marshal : Cstruct.t -> t -> unit
  val unmarshal : Cstruct.t -> (t, exn) result
end

module Header : sig
  type t = {
    table_offset : int64;
    (** absolute byte offset of the BAT *)
    max_table_entries : int;
    (** the maximum number of blocks *)
    block_size_sectors_shift : int;
    (** each block is 2 ** block_size_sectors_shift sectors in size *)
    checksum : int32;
    (** ones-complement checksum of the header *)
    parent_unique_id : Uuidm.t;
    (** if a differencing disk, this is the 128-bit UUID of the parent *)
    parent_time_stamp : int32;
    (** modification time stamp of the parent disk, as seconds since midnight Jan 1 2000 *)
    parent_unicode_name : int array;
    (** parent disk filename *)
    parent_locators : Parent_locator.t array;
    (** up to 8 different pointers to the parent disk image *)
  }

  val create : table_offset:int64 ->
    current_size:int64 ->
    ?block_size_sectors_shift:int ->
    ?checksum:int32 ->
    ?parent_unique_id:Uuidm.t ->
    ?parent_time_stamp:int32 ->
    ?parent_unicode_name:int array ->
    ?parent_locators:Parent_locator.t array ->
    unit -> t

  val equal: t -> t -> bool

  val to_string: t -> string

  val set_parent: t -> string -> t
  (** [set_parent t new_parent] updates the parent locators and
      unicode name *)

  val compute_checksum: t -> int32
  (** compute the expected checksum value *)

  val sizeof_bitmap : t -> int
  val default_block_size: int
  val default_block_size_sectors_shift: int

  val sizeof : int

  val marshal : Cstruct.t -> t -> t
  val unmarshal : Cstruct.t -> (t, exn) result
end

module BAT : sig
  type t
  (** Absolute sector offset of a data block, where a data block contains
      a sector bitmap and then data *)

  val equal: t -> t -> bool
  (** [equal t1 t2] is true if [t1] and [t2] represent the same data *)

  val get: t -> int -> int32
  (** [get t i] returns the [i]th entry *)

  val set: t -> int -> int32 -> unit
  (** [set t i j] sets the [i]th entry to [j] *)

  val fold: (int -> int32 -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t initial] folds [f] across all valid entries *)

  val length: t -> int
  (** [length t] the number of entries in the table *)

  val to_string : t -> string
  (** [to_string t] creates a debug-printable string *)
end

module Batmap_header : sig
  type t = {
    offset: int64;
    size_in_sectors: int;
    major_version: int;
    minor_version: int;
    checksum: int32;
    marker: int
  }
end

module Batmap : sig
  type t
end

module Bitmap : sig
  type t 

  val get : t -> int64 -> bool
  (** [get t sector] is true if [sector] is present in the block *)

  val set : t -> int64 -> (int64 * Cstruct.t) option
  (** [set t sector] asserts the bit for [sector], returning a
      (relative offset, data to be written to disk) pair. *)

end

module Bitmap_cache : sig
  type t = {
    cache: (int * Bitmap.t) option ref; (* effective only for streaming *)
    all_zeroes: Cstruct.t;
    all_ones: Cstruct.t;
  }
end

module Sector : sig
  type t = Cstruct.t
  val dump : Cstruct.t -> unit
end

module Vhd : sig
  type 'a t = {
    filename : string;
    rw: bool;
    handle : 'a;
    header : Header.t;
    footer : Footer.t;
    parent : 'a t option;
    bat : BAT.t;
    batmap : (Batmap_header.t * Batmap.t) option;
    bitmap_cache : Bitmap_cache.t
  }

  val resize: 'a t -> int64 -> 'a t
  (** [resize t new_size] changes the current virtual size of [t] to [new_size].
      Note that [new_size] must be less than or equal to the original size of
      the vhd -- otherwise there wouldn't be enough room for the on-disk
      structures. *)

  val check_overlapping_blocks : 'a t -> unit

  module Field : sig
    val list: string list
    val get: 'a t -> string -> string option
  end
end

module Raw : sig
  type 'a t = {
    filename: string;
    handle: 'a;
  }
end

type size = {
  total: int64;
  (** size of the final disk, in sectors *)

  metadata: int64;
  (** number of metadata sectors *)

  empty: int64;
  (** number of sectors of empty space *)

  copy: int64;
  (** number of copied sectors *)
}
(** The amount of data contained in a stream, broken down by type *)

val empty: size

module Stream : functor(A: S.ASYNC) -> sig
  open A

  type 'a ll =
    | Cons of 'a * (unit -> 'a ll t)
    | End
  (** a lazy list *)

  val iter: ('a -> unit t) -> 'a ll -> unit t
  (** [iter f stream] applies each element from [stream] to [f] in order. *)

  val fold_left: ('a -> 'b -> 'a t) -> 'a -> 'b ll -> 'a t
  (** [fold_left f initial stream] folds [f] across all the elements in
      the [stream] with neutral element [initial] *)

  type 'a stream = {
    elements: 'a Element.t ll;
    size: size;
  }
  (** an image of a disk represented as a stream *)

end

module Fragment : sig
  type t =
    | Header of Header.t
    | Footer of Footer.t
    | BAT of BAT.t
    | Batmap of Batmap.t
    | Block of int64 * Cstruct.t  (** sector offset * data block *)

  (** a fragment of a vhd-formatted stream/file *)

end

module From_input : functor (I: S.INPUT) -> sig
  open I

  type 'a ll =
    | Cons of 'a * (unit -> 'a ll t)
    | End
  (** a lazy list *)

  val openstream : fd -> Fragment.t ll t
  (** produce a stream of Fragment.ts from a vhd stream, using constant space *)
end


module From_file : functor (F : S.FILE) -> sig
  open F

  module Vhd_IO : sig
    val openchain : ?path:string list -> string -> bool -> fd Vhd.t t
    (** [openchain ?path filename] reads the vhd metadata from [filename] (and other
        files on the path from [filename] to the root of the tree). If [filename]
        or any of the parent locators have relative paths, then they will be
        searched for on the ?path. *)

    val openfile : string -> bool -> fd Vhd.t t
    (** [openfile filename] reads the vhd metadata from [filename], but not any
        other files on the path to the root of the tree. *)

    val close : fd Vhd.t -> unit t
    (** [close t] frees all resources associated with [t] *)

    val create_dynamic: filename:string -> size:int64
      -> ?uuid:Uuidm.t
      -> ?saved_state:bool
      -> ?features:Feature.t list
      -> unit -> fd Vhd.t t
    (** [create_dynamic ~filename ~size] creates an empty dynamic vhd with
        virtual size [size] bytes and filename [filename]. *)

    val create_difference: filename:string -> parent:fd Vhd.t
      -> ?relative_path:bool
      -> ?uuid:Uuidm.t
      -> ?saved_state:bool
      -> ?features:Feature.t list
      -> unit -> fd Vhd.t t
    (** [create_difference ~filename ~parent] creates an empty differencing vhd
        with filename [filename] backed by parent [parent]. *)

    val get_sector_location : fd Vhd.t -> int64 -> (fd Vhd.t * int64) option t
    (** [get_sector_location t sector] returns [Some (t', sector')] if the
        [sector] in the virtual disk resides in physical [sector'] in
        the vhd [t'] (where [t'] may be any vhd on the path from [t] to the
        root of the tree. If no sector is present, this returns [None]. *)

    val read_sector : fd Vhd.t -> int64 -> Cstruct.t -> bool t
    (** [read_sector t sector buffer]: if any data exists at [sector] in the
        virtual disk, reads it into [buffer] and returns true. If no data exists
        (i.e. the data should be interpreted as zeros) the function returns false
        but does not write into [buffer]. *)

    val write : fd Vhd.t -> int64 -> Cstruct.t list -> unit t
    (** [write t sector data] writes [data] at [sector] in [t] and
        updates all file metadata to preserve consistency. *)
  end

  module Raw_IO : sig
    val openfile : string -> bool -> fd Raw.t t
    (** [openfile filename] opens a raw-format file [filename] *)

    val close : fd Raw.t -> unit t
    (** [close t] frees all resources associated with [t] *)

    val create: filename:string -> size:int64
      -> unit -> fd Raw.t t
    (** [create ~filename ~size] creates an empty raw file with
        virtual size [size] bytes and filename [filename]. *)
  end

  type 'a ll =
    | Cons of 'a * (unit -> 'a ll t)
    | End
  (** a lazy list *)

  val iter: ('a -> unit t) -> 'a ll -> unit t
  (** [iter f stream] applies each element from [stream] to [f] in order. *)

  val fold_left: ('a -> 'b -> 'a t) -> 'a -> 'b ll -> 'a t
  (** [fold_left f initial stream] folds [f] across all the elements in
      the [stream] with neutral element [initial] *)

  type 'a stream = {
    elements: 'a Element.t ll;
    size: size;
  }
  (** an image of a disk represented as a stream *)

  val expand_empty: 'a stream -> 'a stream t
  (** replaces 'Empty' elements with explicit writes of zeroes *)

  val expand_copy: fd stream -> fd stream t
  (** replaces 'Copy' elements with explicit writes of data *)

  module Vhd_input : sig
    val raw: ?from: fd Vhd.t -> fd Vhd.t -> fd stream t
    (** [raw t] creates a raw-formatted stream representing the consolidated
        data present in the virtual disk [t]. If [from] is provided then
        the stream will contain the vhd differencing disk needed to transform
        [from] into [t]. *)

    val vhd: ?from: fd Vhd.t -> ?emit_batmap:bool -> fd Vhd.t -> fd stream t
    (** [vhd t] creates a vhd-formatted stream representing the consolidated
        data present in the virtual disk [t]. If [from] is provided then
        the stream will contain the vhd differencing disk needed to transform
        [from] into [t]. If ?emit_batmap is set then the resulting vhd will have
        the non-standard 'BATmap' metadata included. *)

  end

  module Hybrid_input : sig

    val raw: ?from: fd Vhd.t -> fd -> fd Vhd.t -> fd stream t
    (** [raw ?from raw vhd] creates a raw-formatted stream representing
        the consolidated data present in the virtual disk [t], in terms of
        copies from the virtual disk [raw]. If [from] is provided then the
        stream will contain only the virtual updates required to transform
        [from] into [t] *)

    val vhd: ?from: fd Vhd.t -> fd -> fd Vhd.t -> fd stream t
    (** [vhd ?from raw vhd] creates a vhd-formatted stream representing
        the consolidated data present in the virtual disk [t], in terms of
        copies from the virtual disk [raw]. If [from] is provided then the
        stream will contain only the virtual updates required to transform
        [from] into [t] *)
  end

  module Raw_input : sig
    val raw : fd Raw.t -> fd stream t

    val vhd : fd Raw.t -> fd stream t
  end

end

