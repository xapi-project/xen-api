
(** This module sets up the env variable for bisect_ppx which describes
 *  where log files are written.
*)


(** [init name] sets up coverage profiling for binary [name]. You could 
 *  use [Sys.argv.(0)] for [name].
*)

let init name =
  let (//)    = Filename.concat in
  let tmpdir  = Filename.get_temp_dir_name () in
  try 
    ignore (Sys.getenv "BISECT_FILE") 
  with Not_found ->
    Unix.putenv "BISECT_FILE" (tmpdir // Printf.sprintf "bisect-%s-" name)

