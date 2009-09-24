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
(* Commandline utility to test PCI id parser *)

let _ = 
  let vendor = ref "" and device = ref "" in

  Arg.parse [ "-vendor", Arg.Set_string vendor, Printf.sprintf "Vendor id (default: %s)" !vendor;
	      "-device", Arg.Set_string device, Printf.sprintf "Device id (default: %s)" !device ]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s\n" x)
    "Test the PCI id parser";

  let v, d = Pciutil.parse !vendor !device in
  Printf.printf "vendor: %s\n" (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) v));
  Printf.printf "device: %s\n" (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) d))
