(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package build description. *)

open Topkg_result

(** {1 Build} *)

type t

val build_cmd : Topkg_conf.t -> Topkg_conf.os -> Topkg_cmd.t
val clean_cmd : Topkg_conf.os -> build_dir:Topkg_fpath.t -> Topkg_cmd.t

val v :
  ?prepare_on_pin:bool ->
  ?dir:Topkg_fpath.t ->
  ?pre:(Topkg_conf.t -> unit result) ->
  ?cmd:(Topkg_conf.t -> Topkg_conf.os -> Topkg_fpath.t list -> unit result) ->
  ?post:(Topkg_conf.t -> unit result) ->
  ?clean:(Topkg_conf.os -> build_dir:Topkg_fpath.t -> unit result) ->
  unit -> t

val with_dir : t -> Topkg_fpath.t -> t

val prepare_on_pin : t -> bool
val dir : t -> Topkg_fpath.t
val pre : t -> (Topkg_conf.t -> unit result)
val cmd :
  t -> (Topkg_conf.t -> Topkg_conf.os -> Topkg_fpath.t list -> unit result)

val post : t -> (Topkg_conf.t -> unit result)
val clean : t -> (Topkg_conf.os -> build_dir:Topkg_fpath.t -> unit result)

val codec : t Topkg_codec.t

val ocb_tag : Topkg_conf.t -> 'a Topkg_conf.key -> string -> Topkg_cmd.t
val ocb_bool_tag : Topkg_conf.t -> bool Topkg_conf.key -> string -> Topkg_cmd.t
val ocb_bool_tags :
  Topkg_conf.t -> (bool Topkg_conf.key * string) list -> Topkg_cmd.t

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
