let _ =
  if Array.length Sys.argv <> 4 then (
    Printf.eprintf "Usage: %s cp/mv <origin> <destination>\n" Sys.argv.(0) ;
    exit 1
  ) ;
  let action = Sys.argv.(1) in
  let origin = Sys.argv.(2) in
  let url = Sys.argv.(3) in
  let rec export_file orig =
    if Sys.is_directory orig then
      let files = Sys.readdir orig in
      let file_paths = Array.map (fun file -> orig ^ "/" ^ file) files in
      Array.iter export_file file_paths
    else
      let span_json = Xapi_stdext_unix.Unixext.string_of_file orig in
      let result = Tracing.Export.Destination.Http.export ~span_json ~url in
      match result with
      | Ok _ ->
          ()
      | Error err ->
          Printf.eprintf "Error: %s" (Printexc.to_string err) ;
          exit 1
  in
  export_file origin ;
  let rec remove_all orig =
    if Sys.is_directory orig then (
      let files = Sys.readdir orig in
      let file_paths = Array.map (Filename.concat orig) files in
      Array.iter remove_all file_paths ;
      Sys.rmdir orig
    ) else
      Sys.remove orig
  in
  if action = "mv" then
    remove_all origin
