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

 open Stdext
 open Listext
 
 module D = Debug.Make(struct let name="xapi" end)
 open D
 
 (** Assert that VM is in a certain set of states before starting an operation *)
 let assert_initial_power_state_in ~__context ~self ~allowed =
   let actual = Db.VM.get_power_state ~__context ~self in
   if not (List.mem actual allowed)
   then raise (Api_errors.Server_error(Api_errors.vm_bad_power_state, [
       Ref.string_of self;
       List.map Record_util.power_to_string allowed |> String.concat ";";
       Record_util.power_to_string actual ]))
 
 (** Assert that VM is in a certain state before starting an operation *)
 let assert_initial_power_state_is ~expected = assert_initial_power_state_in ~allowed:[expected]