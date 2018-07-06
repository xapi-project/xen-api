(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package delegate.

    See {!Topkg_care.Delegate} for documentation. *)

open Bos_setup

(** {1 Publish} *)

val publish_distrib :
  Topkg_care_pkg.t -> msg:string -> archive:Fpath.t ->
  (unit, R.msg) Result.result

val publish_doc :
  Topkg_care_pkg.t -> msg:string -> docdir:Fpath.t ->
  (unit, R.msg) Result.result

val publish_alt :
  Topkg_care_pkg.t -> kind:string -> msg:string -> archive:Fpath.t ->
  (unit, R.msg) Result.result

val publish_in_git_branch :
  remote:string -> branch:string ->
  name:string -> version:string -> docdir:Fpath.t ->
  dir:Fpath.t -> (unit, R.msg) result

(** {1 Delegate} *)

val issue_list : Topkg_care_pkg.t -> (unit, R.msg) result
val issue_show : Topkg_care_pkg.t -> id:string -> (unit, R.msg) result

val issue_open :
  Topkg_care_pkg.t -> title:string -> body:string -> (unit, R.msg) result

val issue_close :
  Topkg_care_pkg.t -> id:string -> msg:string -> (unit, R.msg) result

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
