(*
 * Copyright (C) 2018 Citrix Systems Inc.
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
open Idl

module D = Debug.Make (struct
  let name = "varstored-guard rpc"
end)

let wrap_rpc error f =
  let on_error e =
    Debug.log_backtrace e Backtrace.empty;
    D.warn "Got RPC exception %s" (Printexc.to_string e);
    Lwt.return
    @@
    match error.Error.matcher e with
    | Some r -> r |> Rpcmarshal.marshal error.Error.def.Rpc.Types.ty |> Rpc.failure
    | None -> Rpc.failure Rpc.Null
  in
  Lwt.catch f on_error
