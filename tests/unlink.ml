
(** Test the Unixext.safe_unlink function *)

open Unixext

let _ = 

  let src = ref None in

  Arg.parse 
    [ ]
    (fun x -> match !src with
     | None -> src := Some x
     | _ -> Printf.printf "Ignoring unknown argument: %s" x)
    "Unlink a file which may not exist, suppressing the ENOENT error";

  match !src with
  | Some a -> Unixext.unlink_safe a
  | None -> failwith "Missing filename"


	      
