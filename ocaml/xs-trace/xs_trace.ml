let _ =
  if Array.length Sys.argv <> 4 then (
    Printf.eprintf "Usage: %s cp/mv <origin> <destination>\n" Sys.argv.(0) ;
    exit 1
  ) ;
  let action = Sys.argv.(1) in
  let origin = Sys.argv.(2) in
  let url = Uri.of_string Sys.argv.(3) in
  let submit_json_line line =
    let result = Tracing.Export.Destination.Http.export ~url line in
    match result with
    | Ok _ ->
        ()
    | Error err ->
        Printf.eprintf "Error: %s" (Printexc.to_string err) ;
        exit 1
  in
  let rec export_file orig =
    if Sys.is_directory orig then
      let files = Sys.readdir orig in
      let file_paths = Array.map (Filename.concat orig) files in
      Array.iter export_file file_paths
    else if Filename.check_suffix orig ".zst" then
      Xapi_stdext_unix.Unixext.with_file orig [O_RDONLY] 0o000
      @@ fun compressed_file ->
      Zstd.Fast.decompress_passive compressed_file @@ fun decompressed ->
      let ic = Unix.in_channel_of_descr decompressed in
      try
        while true do
          let line = input_line ic in
          submit_json_line line
        done
      with End_of_file -> ()
    else
      Xapi_stdext_unix.Unixext.readfile_line
        (fun line -> submit_json_line line)
        orig
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
