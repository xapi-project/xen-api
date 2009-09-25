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



let _ = 
  let path = "/test" in

  let xs = Xs.daemon_open () in

  xs.Xs.rm path;

  let counter = ref 0 in
  let callback (path, _) = 
    let condition = try ignore (xs.Xs.read path); true with _ -> false in
    Printf.printf "watch: fired on %s; condition is %b\n" path condition; flush stdout;
    if !counter = 0 then begin
      Printf.printf "got the initial watch. Writing to watched path in callback\n"; flush stdout;
      xs.Xs.write path "gotcha";
      (* To make sure we trigger the bug, do lots of other xenstore operations --
	 these cause any incoming watch event to be queued where we never read it again. *)
      Printf.printf "now doing lots of xenstore reads in callback\n";
      for i = 0 to 1000 do
	ignore (xs.Xs.read path);
      done;
      Printf.printf "callback existing. The condition is now true, we should not block\n"; flush stdout;
    end;
    incr counter;
    condition in

  try
    Xs.monitor_paths xs [ path, "X" ] 10. callback;
    Printf.printf "test passed.\n";
  with Xs.Timeout ->
    Printf.printf "test failed.\n";
    exit 1
  
