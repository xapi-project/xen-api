(*
 * Copyright (C) Cloud Software Group
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

type dict = (string * string) list [@@deriving rpcty]

let option ?name ?(description = []) d =
  let open Rpc.Types in
  let name = Option.value ~default:(Printf.sprintf "%s option" d.name) name in
  {name; description; ty= Option d.ty}

let list ?name ?(description = []) d =
  let open Rpc.Types in
  let name = Option.value ~default:(Printf.sprintf "list of %ss" d.name) name in
  {name; description; ty= List d.ty}

let pair ?name ?(description = []) (p0, p2) =
  let open Rpc.Types in
  let name =
    Option.value
      ~default:(Printf.sprintf "pair of %s and %s" p0.name p2.name)
      name
  in
  {name; description; ty= Tuple (p0.ty, p2.ty)}

let triple ?name ?(description = []) (p1, p2, p3) =
  let open Rpc.Types in
  let name =
    Option.value
      ~default:(Printf.sprintf "triple of %s, %s and %s" p1.name p2.name p3.name)
      name
  in
  {name; description; ty= Tuple3 (p1.ty, p2.ty, p3.ty)}
