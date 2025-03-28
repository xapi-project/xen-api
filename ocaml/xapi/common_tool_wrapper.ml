(*
 * Copyright (C) 2025 Vates.
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

open Xapi_stdext_std.Xstringext

(** [find_backend_device path] returns [Some path'] where [path'] is the backend path in
    the driver domain corresponding to the frontend device [path] in this domain. *)
let find_backend_device path =
  try
    let open Ezxenstore_core.Xenstore in
    (* If we're looking at a xen frontend device, see if the backend
       is in the same domain. If so check if it looks like a .vhd *)
    let rdev = (Unix.stat path).Unix.st_rdev in
    let major = rdev / 256 and minor = rdev mod 256 in
    let link =
      Unix.readlink (Printf.sprintf "/sys/dev/block/%d:%d/device" major minor)
    in
    match List.rev (String.split '/' link) with
    | id :: "xen" :: "devices" :: _
      when Astring.String.is_prefix ~affix:"vbd-" id ->
        let id = int_of_string (String.sub id 4 (String.length id - 4)) in
        with_xs (fun xs ->
            let self = xs.Xs.read "domid" in
            let backend =
              xs.Xs.read (Printf.sprintf "device/vbd/%d/backend" id)
            in
            let params = xs.Xs.read (Printf.sprintf "%s/params" backend) in
            match String.split '/' backend with
            | "local" :: "domain" :: bedomid :: _ ->
                if not (self = bedomid) then
                  raise
                    Api_errors.(
                      Server_error
                        ( internal_error
                        , [
                            Printf.sprintf
                              "find_backend_device: Got domid %s but expected \
                               %s"
                              bedomid self
                          ]
                        )
                    ) ;
                Some params
            | _ ->
                raise Not_found
        )
    | _ ->
        raise Not_found
  with _ -> None
