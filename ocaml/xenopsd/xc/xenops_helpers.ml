(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
open Xenstore

(** {2 XC, XS and XAL interface helpers.} *)

let with_xc f = Xenctrl.with_intf f

let with_xc_and_xs f = Xenctrl.with_intf (fun xc -> with_xs (fun xs -> f xc xs))

let with_xc_and_xs_final f cf =
  with_xc_and_xs (fun xc xs ->
      Xapi_stdext_pervasives.Pervasiveext.finally (fun () -> f xc xs) cf
  )

exception Domain_not_found

let uuid_of_domid ~xs domid =
  try
    let vm = xs.Xs.getdomainpath domid ^ "/vm" in
    let vm_dir = xs.Xs.read vm in
    match Uuid.of_string (xs.Xs.read (vm_dir ^ "/uuid")) with
    | Some uuid ->
        uuid
    | None ->
        raise Domain_not_found
  with _ -> raise Domain_not_found

let domains_of_uuid ~xc uuid =
  let string_of_domain_handle handle =
    Array.to_list handle |> List.map string_of_int |> String.concat "; "
  in

  List.filter
    (fun x ->
      match Uuid.of_int_array x.Xenctrl.handle with
      | Some x ->
          x = uuid
      | None ->
          failwith
            (Printf.sprintf
               "Invalid VM handle for domid %d returned by domain_getinfolist \
                at %s: %a"
               x.Xenctrl.domid __FUNCTION__
               (fun () -> string_of_domain_handle)
               x.Xenctrl.handle
            )
    )
    (Xenctrl.domain_getinfolist xc 0)
