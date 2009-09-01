open Squeezed_rpc

let _ = 
  print_debug := true;

  if Array.length Sys.argv < 2 then begin
    Printf.fprintf stderr "%s <fn name> [key=val]\n" Sys.argv.(0);
    Printf.fprintf stderr "  -- call function <fn name> with optional key=value arguments\n";
    exit 1
  end;
  let fn = Sys.argv.(1) in
  let args = List.tl (List.tl (Array.to_list Sys.argv)) in
  let args = List.map (fun x -> match Stringext.String.split ~limit:2 '=' x with
		       | [ key; v ] -> [ key, v ]
		       | _ -> debug "Skipping argument: %s" x; []) args in
  let args = List.concat args in
  with_xc_and_xs
    (fun _ xs ->
       let results = Rpc.client ~xs ~service:_service ~fn ~args in
       List.iter (fun (k, v) -> Printf.printf "%s=%s\n" k v) results
    )
