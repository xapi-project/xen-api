(* Simple example program which recursively copies a xenstore subtree to another path. *)

open Xenstore_dump


let _ = 

  let src = ref "" and dest = ref "" in
  Arg.parse 
    [ "-src", Arg.Set_string src, "source path";
      "-dest", Arg.Set_string dest, "destination path" ]
    (fun x -> Printf.fprintf stderr "Ignoring unknown parameter: %s\n" x)
    "Copy a xenstore subtree to another path";
  if !src = "" || !dest = "" then begin
    Printf.fprintf stderr "Usage:\n";
    Printf.fprintf stderr "  %s <source path> <destination path>\n" Sys.argv.(0);
    exit 1
  end;
  let xs = Xs.domain_open () in
  restore ~xs !dest (dump ~xs !src)

