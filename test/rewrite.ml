(* Read in an RRD, write it out again *)
(* This is to test input/output code *)

let _ =
  let rrd = Rrd.of_file Sys.argv.(1) in
  Rrd.to_file rrd (Printf.sprintf "%s.new" Sys.argv.(1))

