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

module Stubs = struct
    external getpwd :   string -> string          = "caml_unixpwd_getpwd"
    external getspw :   string -> string          = "caml_unixpwd_getspw"
    external get    :   string -> string          = "caml_unixpwd_get"
    external setpwd :   string -> string -> unit  = "caml_unixpwd_setpwd"
    external setspw :   string -> string -> unit  = "caml_unixpwd_setspw"
    external unshadow : unit   -> string          = "caml_unixpwd_unshadow"
end

exception Error of string

let wrap f = try f () with Failure msg -> raise (Error msg)

let getpwd user     = wrap (fun () -> Stubs.getpwd user)
let getspw user     = wrap (fun () -> Stubs.getspw user)
let get    user     = wrap (fun () -> Stubs.get    user)

let setpwd user pwd = wrap (fun () -> Stubs.setpwd user pwd)
let setspw user pwd = wrap (fun () -> Stubs.setspw user pwd)

let unshadow ()     = wrap Stubs.unshadow
