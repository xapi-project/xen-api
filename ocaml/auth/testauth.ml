
let usage() = 
  print_endline "Usage:";
  Printf.printf "%s auth <username> <password>\n" Sys.argv.(0);
  Printf.printf "%s chpasswd <username> <new password>\n" Sys.argv.(0);
  exit 1

let _ = 
  if Array.length Sys.argv <> 4 then usage ();
  let username = Sys.argv.(2)
  and password = Sys.argv.(3) in
  match Sys.argv.(1) with
  | "auth" ->
      Auth.authenticate username password
  | "chpasswd" ->
      Auth.change_password username password      
  | _ -> usage()
