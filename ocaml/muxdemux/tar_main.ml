(* ------------------------------------------------------------------

   Copyright (c) 2006 Xensource Inc

   Contacts: Dave Scott    <dscott@xensource.com>

   Simple 'tar'-like program to test the Tar module

   ------------------------------------------------------------------- *)

open Debug
open Tar


let _ = 
  let tar = ref "" in
  let mode = ref `List in

  Arg.parse [
    "-list", Arg.Unit (fun _ -> mode := `List), "list the contents of a tar file";
    "-extract", Arg.String (fun dest -> mode := `Extract dest), "extract the contents of a tar file to a directory";
    "-create", Arg.String (fun dir -> mode := `Create dir), "create a tar file from the contents of a directory";
  ] (fun x -> if !tar = "" then tar := x else Printf.printf "Warning, ignoring unknown argument: %s" x)
    "Multiplex a set of files and streams together in a stylised tar";
  let tar = !tar in
  if tar = "" then failwith "Must supply a tar file as an argument";

  let finally after before = 
    try
      before ();
      after ()
    with e ->
      after ();
      raise e in

  let with_in_channel f filename = 
    let ifd = Unix.openfile filename [Unix.O_RDONLY] 0644 in
    finally (fun _ -> Unix.close ifd) (fun _ -> f ifd) in
	
  let with_out_channel f filename = 
    let ofd = Unix.openfile filename [Unix.O_WRONLY] 0644 in
    finally (fun _ -> Unix.close ofd) (fun _ -> f ofd) in

  let action = match !mode with
    | `List -> with_in_channel Archive.list
    | `Extract _ -> with_in_channel (Archive.extract "/tmp/foo")
    | `Create dir ->
	let files = Array.map (fun x -> dir ^ "/" ^ x) (Sys.readdir dir) in
	with_out_channel (Archive.create (Array.to_list files))
    | _ -> fun _ -> print_endline "Unimplemented" in

  action tar
