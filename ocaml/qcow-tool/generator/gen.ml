let output_file = ref "lib/qcow_word_size.ml"

let _ =
  Arg.parse [
    "-o", Arg.Set_string output_file, "output filename"
  ] (fun x ->
    Printf.fprintf stderr "Unexpected argument: %s\n%!" x;
    exit 1;
  ) "Auto-detect the host word size";

  let oc = open_out !output_file in
  begin match Sys.word_size with
  | 64 ->
    Printf.fprintf stderr "On a 64-bit machine so using 'int' for clusters\n";
    output_string oc "module Cluster = Qcow_int\n"
  | _ ->
    Printf.fprintf stderr "Not on a 64-bit machine to using 'int64' for clusters\n";
    output_string oc "module Cluster = Qcow_int64\n"
  end;
  close_out oc
