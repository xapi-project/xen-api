
(** This module sets up the env variable for bisect_ppx which describes
 * where log files are writte
 *)

let (//) = Filename.concat 

let is_dir  = function 
  | ""    -> false
  | path  -> try Sys.is_directory path with Sys_error _ -> false

(* [tmpdir] points to a directory for temporary files *)
let tmpdir =
  let getenv n = try Sys.getenv n with Not_found -> "" in
  let dirs = 
    [ getenv "TMP"
    ; getenv "TEMP"
    ; "/tmp"
    ; "/usr/tmp"
    ; "/var/tmp"
    ] in
  try
    List.find is_dir dirs
  with
    Not_found -> failwith "can't find temp directory "^__LOC__

(** [init name] sets up coverage profiling for binary [name]. You could 
 *  use [Sys.argv.(0) for name
 *)
let init name =
  try 
    ignore (Sys.getenv "BISECT_FILE") 
  with Not_found ->
    Unix.putenv "BISECT_FILE" (tmpdir // Printf.sprintf "bisect-%s-" name)

