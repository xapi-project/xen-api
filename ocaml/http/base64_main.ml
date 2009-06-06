open Base64

let usage () = 
  output_string stderr (Printf.sprintf "Usage: %s (encode|decode) string\n" Sys.argv.(0));
  exit 1

let _ = 
  if Array.length Sys.argv <> 3 then usage ();
  match Sys.argv.(1) with
  | "encode" ->
      print_string (encode Sys.argv.(2))
  | "decode" ->
      print_string (decode Sys.argv.(2))
  | _ -> 
      usage ()
