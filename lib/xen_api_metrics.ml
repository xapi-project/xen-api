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

let cf_of_string = function
  | "AVERAGE" -> `Ok `Average
  | "MIN" -> `Ok `Min
  | "MAX" -> `Ok `Max
  | x -> `Error (`Msg (Printf.sprintf "Unknown consolidation function: %s" x))

module Legend = struct
  type cls = [ `VM | `Host | `Other of string ]

  type t = string * cf * cls * Uuidm.t

  let colon = Re_str.regexp_string ":"
  let of_string x = match Re_str.split_delim colon x with
    | cf :: cls :: uuid :: name :: [] ->
      begin match cf_of_string cf with
      | `Error x -> `Error x
      | `Ok cf ->
        let cls = match cls with
        | "host" -> `Host
        | "vm" -> `VM
        | x -> `Other x in
        begin match Uuidm.of_string uuid with
        | None -> `Error (`Msg (Printf.sprintf "Failed to parse uuid: %s" uuid))
        | Some uuid ->
          `Ok (name, cf, cls, uuid)
        end
      end
    | _ -> `Error (`Msg (Printf.sprintf "Failed to parse legend: %s" x))
  (* Example:
    AVERAGE:host:05843b4a-be19-4ea2-9940-be3f31b5e4bb:memory_total_kib
  *)

  let find_data_source dsl (name, _, _, _) =
    try
      Some (List.find (fun ds -> ds.API.data_source_name_label = name) dsl)
    with Not_found ->
      None
end

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

  let parse x =
    let input = Xmlm.make_input (`String (0, x)) in
    Rrd_updates.of_xml input
end
