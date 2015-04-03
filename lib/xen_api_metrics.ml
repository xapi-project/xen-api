(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

type cf = [ `Average | `Min | `Max ]

let string_of_cf = function
  | `Average -> "AVERAGE"
  | `Min -> "MIN"
  | `Max -> "MAX"

module Updates = struct

  let uri ~host ~authentication ~start ?(include_host=false) ?interval ?cf () =
    let ssl, scheme = match Uri.scheme host with
    | Some "https" -> true, "https"
    | Some "http" -> false, "http"
    | x -> failwith (Printf.sprintf "Unknown scheme: %s" (match x with None -> "None" | Some x -> x)) in
    let port = match Uri.port host with
    | Some x -> x
    | None -> if ssl then 443 else 80 in
    let query = [
      "start", [ string_of_int start ];
      "host", [ string_of_bool include_host ]
    ] @ (match interval with None -> [] | Some x -> [ "interval", [ string_of_int x ] ])
      @ (match cf with None -> [] | Some x -> [ "cf", [ string_of_cf x ] ]) in
    let userinfo = match authentication with
    | `UserPassword (user, pass) -> Some (user ^ ":" ^ pass)
    | `Session_id _ -> None in
    let query = match authentication with
    | `UserPassword (_, _) -> query
    | `Session_id s -> ("session_id", [ s ]) :: query in
    Uri.make ~scheme ?userinfo ?host:(Uri.host host) ~port ~path:"/rrd_updates" ~query ()

  let parse x = x
end
