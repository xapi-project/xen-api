(* Only compiled when building with coverage is enabled *)
let init name =
  let dir = Filename.get_temp_dir_name () in
  Unix.putenv "BISECT_FILE"
    Filename.(Printf.sprintf "bisect-%s-" name |> concat dir)

let dump () =
  let tmp, ch = Filename.open_temp_file (Unix.getenv "BISECT_FILE" |>
  Filename.basename) ".out" in
  try
    Bisect.Runtime.dump_counters_exn ch;
    Bisect.Runtime.reset_counters ();
    close_out_noerr ch;
    [tmp]
  with e ->
    Sys.remove tmp;
    raise e
