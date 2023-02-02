(*
 * Copyright (c) Cloud Software Group, Inc.
 *)

let rec formatted_wrap formatter s =
  let split_in_2 c s =
    match Astring.String.cut ~sep:c s with
    | Some (x, y) ->
        (x, y)
    | None ->
        (s, "")
  in
  let prespace, postspace = split_in_2 " " s in
  let preeol, posteol = split_in_2 "\n" s in

  if String.length prespace < String.length preeol then (
    Format.fprintf formatter "%s@ " prespace ;
    if String.length postspace > 0 then
      formatted_wrap formatter postspace
  ) else if String.length posteol > 0 then (
    Format.fprintf formatter "%s@\n" preeol ;
    formatted_wrap formatter posteol
  ) else
    Format.fprintf formatter "%s@ " preeol

let comment doc ?(indent = 0) s =
  let indent_str = String.make indent ' ' in
  let buf = Buffer.create 16 in
  let formatter = Format.formatter_of_buffer buf in
  let open Format in
  let out, flush, newline, spaces =
    let funcs = Format.pp_get_formatter_out_functions formatter () in
    (funcs.out_string, funcs.out_flush, funcs.out_newline, funcs.out_spaces)
  in

  let funcs =
    {
      out_string= out
    ; out_flush= flush
    ; out_newline=
        (fun () -> out (Printf.sprintf "\n%s * " indent_str) 0 (indent + 4))
    ; out_spaces= spaces
    ; out_indent= spaces
    }
  in
  Format.pp_set_formatter_out_functions formatter funcs ;

  Format.pp_open_hvbox formatter 0 ;
  Format.pp_set_margin formatter 76 ;
  Format.fprintf formatter "%s" indent_str ;
  Format.fprintf formatter "/*" ;
  if doc then
    Format.fprintf formatter "*" ;
  Format.fprintf formatter "\n" ;
  Format.fprintf formatter "%s" indent_str ;
  Format.fprintf formatter " * " ;

  formatted_wrap formatter s ;
  Format.pp_close_box formatter () ;

  Format.fprintf formatter "%!" ;

  Format.pp_set_formatter_out_functions formatter
    {funcs with out_newline= newline} ;

  let result = Buffer.contents buf in
  let n = String.length result in
  String.sub result 0 (n - 1) ^ "/"
