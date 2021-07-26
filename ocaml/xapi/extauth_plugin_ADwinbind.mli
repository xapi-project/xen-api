(*
 * Copyright (C) Citrix Systems Inc.
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

(**
 * @winbind group Access Control
*)

module Winbind : sig
  (* start winbind service *)
  val start : timeout:float -> wait_until_success:bool -> unit

  (* stop winbind service *)
  val stop : timeout:float -> wait_until_success:bool -> unit

  (* init winbind service *)
  val init_service : __context:Context.t -> unit
end

module AuthADWinbind : sig
  val methods : Auth_signature.t
end

(* Expose function to make compiler happy for unittest *)
val extract_ou_config :
  config_params:(string * string) list -> (string * string) list * string list

val domainify_uname : domain:string -> string -> string

module Wbinfo : sig
  type uid_info = {user_name: string; uid: int; gid: int; gecos: string}

  val string_of_uid_info : uid_info -> string

  val parse_uid_info : string -> (uid_info, unit) result

  val exception_of_stderr : string -> exn option
end

module Ldap : sig
  type user = {
      name: string
    ; display_name: string
    ; upn: string
    ; account_disabled: bool
    ; account_expired: bool
    ; account_locked: bool
    ; password_expired: bool
  }

  val string_of_user : user -> string

  val parse_user : string -> (user, string) result

  val escape : string -> string
end

module Migrate_from_pbis : sig
  val range : int -> int -> int -> int list

  val parse_value_from_pbis : string -> string
end
