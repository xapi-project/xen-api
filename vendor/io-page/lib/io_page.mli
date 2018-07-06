(*
 * Copyright (c) 2011-2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013      Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Memory allocation. *)

(** Memory allocation interface. *)

type buf = Cstruct.t
(** Type of a C buffer (in this case, a Cstruct) *)

type t = private (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Type of memory blocks. *)

val page_size : int
(** Size of one page of memory in bytes. *)

val get_addr : t -> nativeint
(** [get_addr t] returns the address of the underlying bigarray of [t].
    This is useful for debugging *)

val get_page : t -> nativeint
(** [get_page t] returns the page offset (get_addr t) mod page_size, starting at 0 .*)

val get : int -> t
(** [get n] allocates and returns a memory block of [n] pages. If
    there is not enough memory, an [Out_of_memory] exception is
    raised.  Note that this may be a recoverable situation, since
    this function allocates memory from a page-aligned pool, whereas
    the OCaml garbage collector will be running in its own heap that
    may have spare memory despite the [Out_of_memory] being raised
    from this function call. *)

val get_buf : ?n:int -> unit -> buf
(** [get_buf n] allocates and returns a memory block of [n] pages,
    represented as a {!Cstruct.t}. If there is not enough memory,
    an [Out_of_memory] exception is raised. *)

val get_order : int -> t
(** [get_order i] is [get (1 lsl i)]. *)

val pages : int -> t list
(** [pages n] allocates a memory block of [n] pages and return the
    list of pages allocated. *)

val pages_order : int -> t list
(** [pages_order i] is [pages (1 lsl i)]. *)

val length : t -> int
(** [length t] is the size of [t], in bytes. *)

val to_cstruct : t -> buf
(** [to_cstruct t] generates a {!Cstruct.t} that covers the entire Io_page. *)

exception Buffer_is_not_page_aligned
exception Buffer_not_multiple_of_page_size

val of_cstruct_exn : buf -> t
(** [of_cstruct t] converts a page-aligned buffer back to an Io_page.
  It raises {!Buffer_is_not_page_aligned} if [t] is not page aligned or 
  {!Buffer_not_multiple_of_page_size} if [t] is not a whole number
  of pages in length.
  TODO: currently assumes the underlying Bigarray is page aligned. *)

val to_string : t -> string
(** [to_string t] will allocate a fresh {!string} and copy the contents of [t]
    into the string. *)

val to_pages : t -> t list
(** [to_pages t] is a list of [size] memory blocks of one page each,
    where [size] is the size of [t] in pages. *)

val string_blit : string -> int -> t -> int -> int -> unit
(** [string_blit src srcoff dst dstoff len] copies [len] bytes from
    string [src], starting at byte number [srcoff], to memory block
    [dst], starting at byte number dstoff. *)

val blit : t -> t -> unit
(** [blit t1 t2] is the same as {!Bigarray.Array1.blit}. *)

val round_to_page_size : int -> int
(** [round_to_page_size n] returns the number of bytes that will be
    allocated for storing [n] bytes in memory *)

