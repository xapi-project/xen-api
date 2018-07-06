let apply_mustache json_data template_data =
  let env = Ezjsonm.from_string json_data
  and tmpl = Mustache.of_string template_data
  in
  Mustache.render tmpl env |> print_endline

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let run json_filename template_filename =
  let j = load_file json_filename
  and t = load_file template_filename
  in
  print_endline j;
  apply_mustache j t

let usage () =
  print_endline "Usage: $0 json_filename template_filename"

let () =
  match Sys.argv with
  | [| _ ; json_filename ; template_filename |]
    -> run json_filename template_filename
  | _ -> usage ()
