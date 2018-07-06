(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Archive creation.

    See {!Topkg_care.Archive}. *)

open Bos_setup

(** {1 Ustar archives} *)

(** ustar encoder.

    {b References}.
    {{:http://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_06}ustar Interchange Format} in POSIX 1003.1, 2013. *)
module Tar : sig

  (** {1 Ustar encoder} *)

  type ptime = int
  (** The type for POSIX times in seconds since the epoch. *)

  type t
  (** The type for ustar archives. *)

  val empty : t
  (** [empty] is the empty ustar archive. *)

  val add :
    t -> Fpath.t -> mode:int -> mtime:ptime -> [`Dir | `File of string ] ->
    (t, R.msg) result
  (** [add a f mode mtime kind] adds to archive [a] an element of
      type [kind] with file path [f], permission mode [mode] and modificaton
      time [mtime]. *)

  val to_string : t -> string
  (** [to_string a] is the byte serialization of the archive [a]. *)
end

val tar :
  Fpath.t -> exclude_paths:Fpath.set -> root:Fpath.t -> mtime:int ->
  (string, R.msg) result

(** {1 Bzip2 compression and unarchiving} *)

val ensure_bzip2 : unit -> (unit, R.msg) result
val bzip2 : string -> dst:Fpath.t -> (unit, R.msg) result
val ensure_tar : unit -> (unit, R.msg) result
val untbz : ?clean:bool -> Fpath.t -> (Fpath.t, R.msg) result

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
