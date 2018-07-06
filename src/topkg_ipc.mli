(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg interprocess communication.

    See {!Topkg.Private.Ipc} for documentation. *)

open Topkg_result

(** {1 Interprocess communication} *)

type 'a t

val v : ?answer:Topkg_fpath.t -> Topkg_cmd.t -> 'a Topkg_codec.t -> 'a t
val cmd : 'a t -> Topkg_cmd.t
val codec : 'a t -> 'a Topkg_codec.t
val answer : 'a t -> Topkg_fpath.t

val pkg : unit -> Topkg_pkg.t t
val lint_custom : unit -> Topkg_result.R.msg Topkg_result.result list option t
val distrib_prepare :
  dist_build_dir:string -> name:string -> version:string -> opam:string ->
  Topkg_fpath.t list result t

val write_answer : Topkg_cmd.t -> Topkg_pkg.t -> unit Topkg_result.result

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
