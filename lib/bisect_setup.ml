(** set up the BISECT_FILE env var to point to a temp directory where
 *  the log files go
 *)

module D = Debug.Make(struct let name = "bisect_setup" end)

let init name =
  let (//) = Filename.concat in
  let tmpdir =
    let getenv n   = try Sys.getenv n with Not_found -> "" in
    let dirs    = 
      [ getenv "TMP"
      ; getenv "TEMP"
      ; "/tmp"
      ; "/usr/tmp"
      ; "/var/tmp"
      ] in
    let is_dir  = function 
    | ""    -> false
    | path  -> try Sys.is_directory path with Sys_error _ -> false
    in try
      List.find is_dir dirs
    with
      Not_found -> D.error "can't find temp directory %s" __LOC__; exit 1
  in try 
    ignore (Sys.getenv "BISECT_FILE") 
  with Not_found ->
    Unix.putenv "BISECT_FILE" (tmpdir // Printf.sprintf "bisect-%s" name)
 
