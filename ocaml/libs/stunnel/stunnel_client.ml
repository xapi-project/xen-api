(*
 * Copyright (C) Systems Inc.
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

module D = Debug.Make (struct let name = "Stunnel_client" end)

let verify = ref false

let get_verify_by_default () = !verify

let set_verify_by_default = function
  | false ->
      D.info "disabling default tls verification" ;
      verify := false
  | true ->
      D.info "enabling default tls verification" ;
      verify := true

let pool () = match !verify with true -> Some Stunnel.pool | false -> None

let appliance () =
  match !verify with true -> Some Stunnel.appliance | false -> None
