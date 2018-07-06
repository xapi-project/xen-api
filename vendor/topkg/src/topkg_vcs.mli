(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** VCS repositories.

    See {!Topkg.Vcs} for documentation. *)

(** {1 VCS} *)

open Topkg_result

type kind = [ `Git | `Hg ]
val pp_kind : Format.formatter -> kind -> unit

type commit_ish = string

type t

val kind : t -> kind
val dir : t -> Topkg_fpath.t
val find : ?dir:Topkg_fpath.t -> unit -> t option result
val get : ?dir:Topkg_fpath.t -> unit -> t result
val cmd : t -> Topkg_cmd.t
val pp : Format.formatter -> t -> unit

val is_dirty : t -> bool result
val not_dirty : t -> unit result
val file_is_dirty : t -> Topkg_fpath.t -> bool result
val head : ?dirty:bool -> t -> string result
val commit_id : ?dirty:bool -> ?commit_ish:string -> t -> string result
val commit_ptime_s : ?commit_ish:commit_ish -> t -> int result
val describe : ?dirty:bool -> ?commit_ish:string -> t -> string result
val tags : t -> string list result
val changes :
  ?until:string -> t -> after:string -> (string * string) list result

val tracked_files : ?tree_ish:string -> t -> Topkg_fpath.t list result

val clone : t -> dir:Topkg_fpath.t -> unit result
val checkout : ?branch:string -> t -> commit_ish:string -> unit result
val commit_files : ?msg:string -> t -> Topkg_fpath.t list -> unit result

val delete_tag : t -> string -> unit result
val tag :
  ?force:bool -> ?sign:bool -> ?msg:string -> ?commit_ish:string -> t ->
  string -> unit result

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
