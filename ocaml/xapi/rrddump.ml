let _ = 
  let body = Unixext.read_whole_file_to_string Sys.argv.(1) in
  let input = Xmlm.input_of_string body in
  let rrd = Rrd.from_xml input in
  Rrd.text_export rrd []
