(* Read in an RRD, write it out again *)
(* This is to test input/output code *)

let _ =
  let rrd = Rrd_unix.of_file Sys.argv.(1) in
  Rrd_unix.to_file rrd (Printf.sprintf "%s.new" Sys.argv.(1))

