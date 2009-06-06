let xml = Xml.parse_file "sql_msg_example.txt"
let str = Xml.to_string_fmt xml

let parse() = ignore (Xml.parse_string str)
let to_string() = ignore (Xml.to_string_fmt xml)

let rec repeat f i =
  if i = 0 then ()
  else (f(); repeat f (i-1))

let _ = repeat parse 1000
(* let _ = print_string str *)
