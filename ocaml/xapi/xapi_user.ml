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
(** Module that defines API functions for User objects
 * @group XenAPI functions
*)

let create ~__context ~short_name ~fullname ~other_config =
  let uuid = Uuid.make_uuid () in
  let ref = Ref.make () in
  Db.User.create ~__context ~ref ~uuid:(Uuid.to_string uuid)
    ~short_name ~fullname ~other_config;
  ref

let destroy ~__context ~self =
  Db.User.destroy ~__context ~self
