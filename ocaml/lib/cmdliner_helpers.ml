
let comma = Re_str.regexp_string ","
let equals = Re_str.regexp_string "="

let string_string_list =
  let read x =
    try
      let kvpairs = Re_str.split_delim comma x in
      let pair x =
        let i = String.index x '=' in
        String.sub x 0 i, String.sub x (i + 1) (String.length x - i - 1) in
      `Ok (List.map pair kvpairs)
    with _ ->
      `Error (Printf.sprintf "I expected a list of the form: k1=v1,k2=v2,...,kn=vn but I got %s" x) in
  let write f x =
    Format.fprintf f "%s" (String.concat ", " (List.map (fun (k, v) -> k ^ " = " ^ v) x)) in
  read, write
