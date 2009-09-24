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
(* Xen PV guest installer *)


open Distros

(* testing *)
let _ =
  let distro = rhel44 in
  let params = 
    {
      name="test";
      desc="test rhel44 install";
      version=1L;
      memory_static_max=268435456L;
      memory_static_min=268435456L;
      memory_dynamic_max=268435456L;
      memory_dynamic_min=268435456L;
      vcpus=1L;
      disk_size=268435456L
    } in
  let s = Operations.init_session "root" "xenroot" in
  let vm= Operations.vm_construct s distro params in
  ignore( Operations.boot_vm s vm );
  ignore( Operations.reset_bootloader s vm );
  
    
