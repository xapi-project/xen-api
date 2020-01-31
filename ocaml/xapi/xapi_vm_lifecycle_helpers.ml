(*
 * Copyright (C) 2017 Citrix Systems Inc.
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
 
 module D = Debug.Make(struct let name="xapi" end)
 open D
 
 (** VM is considered as "live" when it's either Running or Paused, i.e. with a live domain *)
let is_live ~__context ~self =
  let power_state = Db.VM.get_power_state ~__context ~self in
  power_state = `Running || power_state = `Paused