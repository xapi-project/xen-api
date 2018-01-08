(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(*
 * This is a replacement interface for xml-light that use the superior xmlm
 * engine to parse stuff. Also the output functions SKIP characters that are
 * not allowed in XML.
 *)

(* tree representation *)
type xml =
  | Element of (string * (string * string) list * xml list)
  | PCData of string

type error_pos = { eline: int; eline_start: int; emin: int; emax: int }
type error = string * error_pos

exception Error of error

let error (msg,pos) =
  Printf.sprintf "%s line %d" msg pos.eline

(* internal parse function *)
let is_empty xml =
  let is_empty_string s = 
    let is_empty = ref true in
    for i = 0 to (String.length s - 1)
    do
      if s.[i] <> '\n' && s.[i] <> ' ' && s.[i] <> '\t' then
        is_empty := false
    done;
    !is_empty in
  match xml with
  | PCData data when is_empty_string data -> true
  | _ -> false

let _parse i =
  let el (tag: Xmlm.tag) (children: xml list) : xml =
    let name_local = snd (fst tag) in
    let attrs' = List.map (fun (nameattr, str) -> (snd nameattr, str)) (snd tag) in
    Element (name_local, attrs', List.filter (fun xml -> not (is_empty xml)) children)
  in
  let data s = PCData s in
  match Xmlm.peek i with
  | `Dtd _ -> snd (Xmlm.input_doc_tree ~el ~data i)
  | _      -> Xmlm.input_tree ~el ~data i

let parse i =
  try _parse i
  with
  | Xmlm.Error ((line, col), msg) ->
    let pos = {
      eline = line; eline_start = line;
      emin = col; emax = col
    } in
    let err = Xmlm.error_message msg in
    raise (Error (err, pos))

(* common parse function *)
let parse_file file =
  let chan = open_in file in
  try
    let i = Xmlm.make_input (`Channel chan) in
    let ret = parse i in
    close_in chan;
    ret
  with exn ->
    close_in_noerr chan; raise exn

let parse_in chan =
  let i = Xmlm.make_input (`Channel chan) in
  parse i

let parse_string s =
  let i = Xmlm.make_input (`String (0, s)) in
  parse i

let parse_bigbuffer b =
  let n = ref Int64.zero in
  let aux () =
    try 
      let c = Xapi_stdext_bigbuffer.Bigbuffer.get b !n in
      n := Int64.add !n Int64.one;
      int_of_char c
    with _ -> raise End_of_file in
  let i = Xmlm.make_input (`Fun aux) in
  parse i

let esc_pcdata data =
  let buf = Buffer.create (String.length data + 10) in
  String.iter (fun c ->
      let s = match c with
        | '>'    -> "&gt;;"
        | '<'    -> "&lt;"
        | '&'    -> "&amp;"
        | '"'    -> "&quot;"
        | c when (c >= '\x20' && c <= '\xff')
              || c = '\x09' || c = '\x0a' || c = '\x0d'
          -> String.make 1 c
        | _      -> ""
      in
      Buffer.add_string buf s) data;
  Buffer.contents buf

let str_of_attrs attrs =
  let fmt s = Printf.sprintf s in
  if List.length attrs > 0 then
    " " ^ (String.concat " " (List.map (fun (k, v) -> fmt "%s=\"%s\"" k (esc_pcdata v)) attrs))
  else
    ""

let to_fct xml f =
  let fmt s = Printf.sprintf s in
  let rec print xml =
    match xml with
    | Element (name, attrs, []) ->
      let astr = str_of_attrs attrs in
      let on = fmt "<%s%s/>" name astr in
      f on;
    | Element (name, attrs, children) ->
      let astr = str_of_attrs attrs in
      let on = fmt "<%s%s>" name astr in
      let off = fmt "</%s>" name in
      f on;
      List.iter (fun child -> print child) children;
      f off
    | PCData data ->
      f (esc_pcdata data)
  in
  print xml

let to_fct_fmt xml f =
  let fmt s = Printf.sprintf s in
  let rec print newl indent xml =
    match xml with
    | Element (name, attrs, [ PCData data ]) ->
      let astr = str_of_attrs attrs in
      let on = fmt "%s<%s%s>" indent name astr in
      let off = fmt "</%s>%s" name (if newl then "\n" else "") in
      f on;
      f (esc_pcdata data);
      f off;
    | Element (name, attrs, []) ->
      let astr = str_of_attrs attrs in
      let on = fmt "%s<%s%s/>%s" indent name astr
          (if newl then "\n" else "") in
      f on;
    | Element (name, attrs, children) ->
      let astr = str_of_attrs attrs in
      let on = fmt "%s<%s%s>\n" indent name astr in
      let off = fmt "%s</%s>%s" indent name
          (if newl then "\n" else "") in
      f on;
      List.iter (fun child -> print true
                    (indent ^ "  ") child) children;
      f off
    | PCData data ->
      f ((esc_pcdata data) ^ (if newl then "\n" else ""))
  in
  print false "" xml

let to_string xml =
  let buffer = Buffer.create 1024 in
  to_fct xml (fun s -> Buffer.add_string buffer s);
  let s = Buffer.contents buffer in Buffer.reset buffer; s

let to_string_fmt xml =
  let buffer = Buffer.create 1024 in
  to_fct_fmt xml (fun s -> Buffer.add_string buffer s);
  let s = Buffer.contents buffer in Buffer.reset buffer; s

let to_bigbuffer xml =
  let open Xapi_stdext_bigbuffer in 
  let buffer = Bigbuffer.make () in
  to_fct xml (fun s -> Bigbuffer.append_substring buffer s 0 (String.length s));
  buffer

(* helpers functions *)
exception Not_pcdata of string
exception Not_element of string

let pcdata = function
  | PCData x -> x
  | e -> raise (Not_pcdata (to_string e))

let children = function
  | Element (_,_,c) -> c
  | e -> raise (Not_element (to_string e))

let tag = function
  | Element (x,_,_) -> x
  | e -> raise (Not_element (to_string e))
