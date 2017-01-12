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

open OUnit
open Xapi_pci_helpers

(* This test generates a lot of print --- set skip to false to enable *)
let skip = true

let print_host_pcis () =
  skip_if skip "Generates lots of text...";
  try
    print_string "===== Host PCIs =====\n\n";
    let pcis = get_host_pcis () in
    List.iter
      (fun p ->
         let x_to_str = Printf.sprintf "%04x" in
         Printf.printf "%s " (String.concat " "
                                [
                                  p.address;
                                  x_to_str p.vendor.id;
                                  p.vendor.name;
                                  x_to_str p.device.id;
                                  p.device.name;
                                  x_to_str p.pci_class.id;
                                  p.pci_class.name
                                ]);
         List.iter (fun s -> print_string (s ^ ", ")) p.related;
         print_newline ())
      pcis
  with e ->
    print_string (Printexc.to_string e);
    assert_equal 0 1

let test =
  "test_pci_helpers" >:::
  [
    "print_host_pcis" >:: print_host_pcis;
  ]
