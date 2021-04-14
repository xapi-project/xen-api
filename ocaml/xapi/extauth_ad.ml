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
 * @AD group Access Control
*)

open Db_actions

module D = Debug.Make (struct let name = "extauth_ad" end)

open D

module AD_type = struct
  exception Unknown_AD_type of string

  type t = Pbis | Winbind

  let of_string = function
    | "pbis" ->
        Pbis
    | "winbind" ->
        Winbind
    | _ as at ->
        error "Unknown AD type '%s'" at ;
        raise (Unknown_AD_type at)
end

module AD = struct
  let start_backend_daemon ~wait_until_success = function
    | AD_type.Pbis ->
        Extauth_plugin_ADpbis.Lwsmd.start ~wait_until_success ~timeout:5.
    | AD_type.Winbind ->
        Extauth_plugin_ADwinbind.Winbind.start ~wait_until_success ~timeout:5.

  let stop_backend_daemon ~wait_until_success = function
    | AD_type.Pbis ->
        Extauth_plugin_ADpbis.Lwsmd.stop ~wait_until_success ~timeout:3.
    | AD_type.Winbind ->
        Extauth_plugin_ADwinbind.Winbind.stop ~wait_until_success ~timeout:3.

  let init_service ~__context = function
    | AD_type.Pbis ->
        Extauth_plugin_ADpbis.Lwsmd.init_service ~__context
    | AD_type.Winbind ->
        Extauth_plugin_ADwinbind.Winbind.init_service ~__context
end

let start_backend_daemon ~wait_until_success =
  !Xapi_globs.extauth_ad_backend
  |> AD_type.of_string
  |> AD.start_backend_daemon ~wait_until_success

let stop_backend_daemon ~wait_until_success =
  !Xapi_globs.extauth_ad_backend
  |> AD_type.of_string
  |> AD.stop_backend_daemon ~wait_until_success

let init_service ~__context =
  !Xapi_globs.extauth_ad_backend
  |> AD_type.of_string
  |> AD.init_service ~__context

let methods =
  match !Xapi_globs.extauth_ad_backend |> AD_type.of_string with
  | Pbis ->
      Extauth_plugin_ADpbis.AuthADlw.methods
  | Winbind ->
      Extauth_plugin_ADwinbind.AuthADWinbind.methods
