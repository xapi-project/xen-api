
(** Makes a new file in the same directory as 'otherfile' *)
let temp_file_in_dir otherfile =
  let base_dir = Filename.dirname otherfile in
  let rec keep_trying () = 
    try 
      let uuid = Uuid.to_string (Uuid.make_uuid ()) in
      let newfile = base_dir ^ "/" ^ uuid in
      Unix.close (Unix.openfile newfile [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_EXCL] 0o600);
      newfile
    with
      Unix.Unix_error (Unix.EEXIST, _, _)  -> keep_trying ()
  in
  keep_trying ()

	  
	
