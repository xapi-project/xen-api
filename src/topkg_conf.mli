(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Build configuration

    See {!Topkg.Conf}. *)

open Topkg_result

(** {1 Configuration key value converters} *)

type 'a conv

val conv :
  ?docv:string -> (string -> 'a result) -> (Format.formatter -> 'a -> unit) ->
  'a conv

val conv_with_docv : 'a conv -> docv:string -> 'a conv
val conv_parser : 'a conv -> (string -> 'a result)
val conv_printer : 'a conv -> (Format.formatter -> 'a -> unit)
val conv_docv : 'a conv -> string

val bool : bool conv
val int : int conv
val string : string conv
val fpath : Topkg_fpath.t conv
val some : ?none:string -> 'a conv -> 'a option conv

(** {1 Configuration keys} *)

type 'a key

val key :
  ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
  absent:'a -> 'a key

val discovered_key :
  ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
  absent:(unit -> 'a result) -> 'a key

val with_pkg : ?default:bool -> string -> bool key

val pp_keys_cli_opts : Format.formatter -> unit -> unit

(** {1 Build configuration} *)

type t
val empty : t
val value : t -> 'a key -> 'a
val pp_value : t -> Format.formatter -> 'a key -> unit
val dump : Format.formatter -> t -> unit
val of_cli_args :
  pkg_name:string -> build_dir:Topkg_fpath.t -> string list -> t result

val pkg_name : t -> string
val build_dir : t -> Topkg_fpath.t
val vcs : t -> bool
val pinned : t -> bool
val dev_pkg : t -> bool
val jobs : t -> int

type build_context = [`Dev | `Distrib | `Pin ]
val build_context : t -> [`Dev | `Distrib | `Pin ]
val build_tests : t -> bool

val debug : t -> bool
val debugger_support : t -> bool
val profile : t -> bool
val toolchain : t -> string option

(** {1 Tool lookup} *)

type os = [ `Build_os | `Host_os ]
val tool : ?conf:t -> string -> os -> Topkg_cmd.t

(** {1 OCaml configuration} *)

module OCaml : sig
  type conf = t
  type t
  val v : conf -> os -> t
  val find : string -> t -> string option
  val version : t -> int * int * int * string option
  val ext_obj : t -> string
  val ext_asm : t -> string
  val ext_lib : t -> string
  val ext_dll : t -> string
  val ext_exe : t -> string
  val native : t -> bool
  val native_dynlink : t -> bool
  val word_size : t -> int
  val dump : Format.formatter -> t -> unit
end

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
