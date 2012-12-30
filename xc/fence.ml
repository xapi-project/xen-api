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
(* The world's simplest program which attempts to reboot domain 0 *)

let _ = 
  if Array.length Sys.argv <> 2 || Sys.argv.(1) <> "yesreally" then begin
    Printf.fprintf stderr "Immediately fence this host - use with extreme caution\n";
    Printf.fprintf stderr "Usage: %s yesreally\n" Sys.argv.(0);
    exit 1
  end;

  let xc = Xenctrl.interface_open () in
  (* Clear both watchdog slots *)
  (try ignore(Xenctrl.watchdog xc 1 0l) with _ -> ());
  (try ignore(Xenctrl.watchdog xc 2 0l) with _ -> ());
  (* set a very short timeout *)
  Xenctrl.watchdog xc 0 0l
  (* boom? *)

