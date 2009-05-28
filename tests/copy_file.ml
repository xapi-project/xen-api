
(** Test the copy_file function *)

let with_openfile filename mode perms f = 
  let fd = Unix.openfile filename mode perms in
  Pervasiveext.finally (fun () -> f fd) (fun () -> Unix.close fd)

let _ = 

  let limit = ref None
  and src = ref None
  and dest = ref None in

  Arg.parse 
    [ "-limit", Arg.String (fun x -> limit := Some (Int64.of_string x)), 
      "copy the first N bytes" ]
    (fun x -> match !src, !dest with
     | None, None -> src := Some x
     | Some _, None -> dest := Some x
     | _, _ -> Printf.printf "Ignoring unknown argument: %s" x)
    "Copy a file from A to B";

  let limit = !limit in
  match !src, !dest with
  | Some a, Some b -> 
      with_openfile a [ Unix.O_RDONLY ] 0o0
	(fun ifd ->
	   with_openfile b [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600
	     (fun ofd ->
		Unixext.copy_file ?limit ifd ofd
	     )
	) 
  | _, _ -> failwith "Missing source and destination file arguments"


	      
