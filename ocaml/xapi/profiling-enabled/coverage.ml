(* Only compiled when building with coverage is enabled *)
module D=Debug.Make(struct let name="coverage" end)
let queue_name = "org.xen.xapi.coverage"

let dump () =
  let tmp, ch = Filename.open_temp_file (Unix.getenv "BISECT_FILE" |>
  Filename.basename) ".out" in
  try
    Bisect.Runtime.dump_counters_exn ch;
    D.debug "Saved coverage data to %s" tmp;
    close_out_noerr ch;
    [tmp]
  with e ->
    Sys.remove tmp;
    raise e

let process = function
  | "dump" ->
    dump () |> List.hd
  | "reset" ->
    Bisect.Runtime.reset_counters ();
    D.debug "Coverage counters reset"; ""
  | api -> failwith api

let init name =
  let dir = Filename.get_temp_dir_name () in
  Unix.putenv "BISECT_FILE"
    Filename.(Printf.sprintf "bisect-%s-" name |> concat dir);
  let (_:Thread.t) = Thread.create (Protocol_unix.Server.listen ~process ~switch:!Xcp_client.switch_path ~queue:queue_name) () in
  ()
