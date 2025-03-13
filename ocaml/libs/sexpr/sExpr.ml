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
type t = Node of t list | Symbol of string | String of string

let unescape_buf buf s =
  let aux esc = function
    | '\\' when not esc ->
        true
    | c ->
        Buffer.add_char buf c ; false
  in
  if Astring.String.fold_left aux false s then
    Buffer.add_char buf '\\'

let is_escape_char = function '\\' | '"' | '\'' -> true | _ -> false

(* XXX: This escapes "'c'" and "\'c\'" to "\\'c\\'".
 * They are both unescaped as "'c'". They have been ported
 * to make sure that this corner case is left unchanged.
 * It is worth investigating the use of 
 * - Astring.String.Ascii.escape_string
 * - Astring.String.Ascii.unescape
 * that have guaranteed invariants and optimised performances *)
let escape_buf escaped s =
  let open Astring in
  if String.exists is_escape_char s then
    String.iter
      (fun c ->
        match c with
        | '\\' ->
            Buffer.add_string escaped "\\\\"
        | '"' ->
            Buffer.add_string escaped "\\\""
        | '\'' ->
            Buffer.add_string escaped "\\\'"
        | _ ->
            Buffer.add_char escaped c
      )
      s
  else
    Buffer.add_string escaped s

let unescape s =
  if String.contains s '\\' then (
    let buf = Buffer.create (String.length s) in
    unescape_buf buf s ; Buffer.contents buf
  ) else
    s

let mkstring x = String (unescape x)

let string_of sexpr =
  let buf = Buffer.create 64 in
  let rec __string_of_rec x =
    match x with
    | Node l ->
        Buffer.add_char buf '(' ;
        ( match l with
        | [] ->
            ()
        | [a] ->
            __string_of_rec a
        | [a; b] ->
            __string_of_rec a ; Buffer.add_char buf ' ' ; __string_of_rec b
        | a :: l ->
            __string_of_rec a ;
            List.iter (fun i -> Buffer.add_char buf ' ' ; __string_of_rec i) l
        ) ;
        Buffer.add_char buf ')'
    | Symbol s | String s ->
        Buffer.add_string buf "\'" ;
        escape_buf buf s ;
        Buffer.add_string buf "\'"
  in
  __string_of_rec sexpr ; Buffer.contents buf
