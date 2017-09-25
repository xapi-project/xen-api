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

open Stdext
open Listext
open Threadext
module D = Debug.Make(struct let name="xapi" end)
open D

let mutex = Mutex.create ()
let create ~__context ~uSB_group ~host ~other_config ~path
      ~vendor_id ~vendor_desc ~product_id ~product_desc ~serial ~version ~description =
  let pusb = Ref.make () and uuid = Uuid.make_uuid () in
  let host = Helpers.get_localhost ~__context in
  Db.PUSB.create ~__context ~ref:pusb ~uuid:(Uuid.to_string uuid)
    ~uSB_group ~host ~attached:Ref.null ~other_config ~path ~vendor_id ~vendor_desc ~product_id
    ~product_desc ~serial ~version ~description ~passthrough_enabled:false;
  debug "PUSB ref='%s' created" (Ref.string_of pusb);
  pusb


let scan ~__context =
  debug "pusb scan to do"
