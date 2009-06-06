(* The world's simplest program which attempts to reboot domain 0 *)

let _ = 
  if Array.length Sys.argv <> 2 || Sys.argv.(1) <> "yesreally" then begin
    Printf.fprintf stderr "Immediately fence this host - use with extreme caution\n";
    Printf.fprintf stderr "Usage: %s yesreally\n" Sys.argv.(0);
    exit 1
  end;

  let xc = Xc.interface_open () in
  (* Clear both watchdog slots *)
  (try ignore(Xc.watchdog xc 1 0l) with _ -> ());
  (try ignore(Xc.watchdog xc 2 0l) with _ -> ());
  (* set a very short timeout *)
  Xc.watchdog xc 0 0l
  (* boom? *)

