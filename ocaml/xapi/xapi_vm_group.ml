(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = "xapi_vm_group" end)

let create ~__context ~name_label ~name_description ~placement =
  Pool_features.assert_enabled ~__context ~f:Features.VM_group ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  Db.VM_group.create ~__context ~ref ~uuid:(Uuidx.to_string uuid) ~name_label
    ~name_description ~placement ;
  ref

let destroy ~__context ~self =
  List.iter
    (fun vm -> Db.VM.remove_groups ~__context ~self:vm ~value:self)
    (Db.VM_group.get_VMs ~__context ~self) ;
  Db.VM_group.destroy ~__context ~self
