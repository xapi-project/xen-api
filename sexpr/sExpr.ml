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
type t =
    Node of t list
  | Symbol of string
  | String of string
  | WeirdString of string * string

let unescape_buf buf s =
  let aux esc = function
      '\\' when not esc -> true
    | c -> Buffer.add_char buf c; false in
  if Astring.String.fold_left aux false s then
    Buffer.add_char buf '\\'

(* XXX: This escapes "'c'" and "\'c\'" to "\\'c\\'".
 * They are both unescaped as "'c'". They have been ported
 * to make sure that this corner case is left unchanged.
 * It is worth investigating the use of 
 * - Astring.String.Ascii.escape_string
 * - Astring.String.Ascii.unescape
 * that have guaranteed invariants and optimised performances *)
let escape s =
  let open Astring in
  String.fold_left
    (fun acc c ->
       acc ^
       match c with
       | '\\' -> "\\\\"
       | '"'  -> "\\\""
       | '\'' -> "\\\'"
       |  _   -> Astring.String.of_char c) "" s

let unescape s =
  let buf = Buffer.create (String.length s) in
  unescape_buf buf s;
  Buffer.contents buf

let mkstring x = String (unescape x)

let string_of sexpr =
  let buf = Buffer.create 64 in
  let rec __string_of_rec x =
    match x with
    | Node l ->
      Buffer.add_char buf '(';
      begin match l with
        | []     -> ()
        | [ a ]  -> __string_of_rec a
        | [ a ; b ] -> __string_of_rec a; Buffer.add_char buf ' '; __string_of_rec b
        | a :: l ->
          __string_of_rec a;
          List.iter (fun i -> Buffer.add_char buf ' '; __string_of_rec i) l;
      end;
      Buffer.add_char buf ')';
    | Symbol s | String s | WeirdString(_, s) ->
      Buffer.add_string buf "\'";
      Buffer.add_string buf (escape s);
      Buffer.add_string buf "\'";
  in
  __string_of_rec sexpr;
  Buffer.contents buf

let weird_of_string x = 
  let random_chars = "abcdefghijklmnopqrstuvwxyz" in
  let randchar () = String.sub random_chars (Random.int (String.length random_chars)) 1 in
  (* true if the parent string contains child as a substring, starting the 
     search forward from offset *)
  let rec has_substring parent offset child = 
    (String.length parent - offset >= (String.length child)) &&
    ((String.sub parent offset (String.length child) = child)
     || has_substring parent (offset + 1) child) in
  let rec find delim = 
    if has_substring x 0 delim then (find (delim ^ (randchar ()))) else delim in
  WeirdString(find "xxx", x)

let rec output_fmt ff = function
  | Node list ->
    let rec aux ?(first=true) = function
        [] -> ()
      | h::t when first ->
        output_fmt ff h;
        aux ~first:false t
      | h::t ->
        Format.fprintf ff "@;<1 2>%a" output_fmt h;
        aux ~first t in
    Format.fprintf ff "@[(";
    aux list;
    Format.fprintf ff ")@]"
  | Symbol s
  | String s
  | WeirdString(_, s) ->
    Format.fprintf ff "\"%s\"" (escape s)
(*
  | Symbol s ->
      Format.fprintf ff "%s" s
  | WeirdString(tag, s) ->
      Format.fprintf ff "<<%s<%s<%s<" tag s tag
*)
