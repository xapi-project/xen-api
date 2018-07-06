open Radix_tree

let test_strings = [
  "/import_vdi";
  "/import_raw_vdi";
  "/export";
  "/export_metadata";
  "/import";
  "/import_metadata";
  "/migrate";
  "/console";
  "/host_backup";
  "/host_restore";
  "/host_logs_download";
  "/pool_patch_upload";
  "/oem_patch_stream";
  "/pool_patch_download";
  "/sync_config_files";
  "/pool/xmldbdump";
  "http";
  "/vncsnapshot";
  "/system-status";
  "/remote_db_access";
  "/remote_db_access_v2";
  "/remote_stats";
  "/json";
  "/cli";
  "/vm_rrd";
  "/rrd";
  "/host_rrd";
  "/rrd_updates";
  "/blob";
  "/remotecmd";
  "/rss";
  "/wlb_report";
  "/wlb_diagnostics";
  "/audit_log";
  "/"
]

let t = List.fold_left (fun t x -> insert x x t) empty test_strings

(* Check that each string can be found in the structure and maps to
   the right key *)
let check1 () = List.iter
    (fun x ->
       if longest_prefix x t <> Some x
       then failwith (Printf.sprintf "x = %s" x)) test_strings

let check2 () =
  let all = fold (fun k v acc -> k :: acc) [] t in
  if List.length all <> (List.length test_strings)
  then failwith "fold"

let previous_longest_prefix x =
  let uris = List.sort (fun a b -> compare (String.length b) (String.length a)) test_strings in
  try Some (List.find (fun uri -> is_prefix uri x) uris) with _ -> None

let _ =
  check1 ();
  check2 ();
  let time n f =
    let start = Unix.gettimeofday () in
    for i = 0 to n do
      f ()
    done;
    let t = Unix.gettimeofday () -. start in
    float_of_int n /. t in (* ops per sec *)
  let before = time 1000000 (fun () -> previous_longest_prefix "/") in
  let after = time 1000000 (fun () -> ignore(longest_prefix "/" t)) in
  Printf.printf "Before: %.1f lookups/sec\n" before;
  Printf.printf "After: %.1f lookups/sec\n" after
