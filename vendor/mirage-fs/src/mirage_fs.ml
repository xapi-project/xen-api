(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

type error = [
  | `Is_a_directory
  | `No_directory_entry
  | `Not_a_directory
]

type write_error = [
  | error
  | `File_already_exists
  | `No_directory_entry
  | `No_space
]

let pp_error ppf = function
  | `Is_a_directory      -> Fmt.string ppf "is a directory"
  | `Not_a_directory     -> Fmt.string ppf "is not a directory"
  | `No_directory_entry  ->
    Fmt.string ppf "a directory in the path does not exist"

let pp_write_error ppf = function
  | #error as e          -> pp_error ppf e
  | `Directory_not_empty -> Fmt.string ppf "directory is not empty"
  | `File_already_exists -> Fmt.string ppf "file already exists"
  | `No_space            -> Fmt.string ppf "device has no more free space"

type stat = {
  filename: string;
  read_only: bool;
  directory: bool;
  size: int64;
}

type fs_error = error
type fs_write_error = write_error

module type S = sig
  type error = private [> fs_error]
  val pp_error: error Fmt.t
  type write_error = private [> fs_write_error]
  val pp_write_error: write_error Fmt.t
  include Mirage_device.S
  type page_aligned_buffer
  val read: t -> string -> int -> int ->
    (page_aligned_buffer list, error) result io
  val size: t -> string -> (int64, error) result io
  val create: t -> string -> (unit, write_error) result io
  val mkdir: t -> string -> (unit, write_error) result io
  val destroy: t -> string -> (unit, write_error) result io
  val stat: t -> string -> (stat, error) result io
  val listdir: t -> string -> (string list, error) result io
  val write: t -> string -> int -> page_aligned_buffer ->
    (unit, write_error) result io
end
