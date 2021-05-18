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
