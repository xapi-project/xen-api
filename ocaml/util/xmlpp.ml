
open Xml

let _ = 
  let xml = Xml.parse_in stdin in
  print_endline (Xml.to_string_fmt xml)
