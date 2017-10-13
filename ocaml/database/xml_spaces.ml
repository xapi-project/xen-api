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
(* we pick here a valid XML character which is not used too often in state.db (ie. not '\') *)
let protect_char = '%'

type change =
  | No_change
  | Replace of char * char (* replace 2 chars by 2 other chars                         *)
  | Compress of char       (* replace 2 chars by 1 char                                *)
  | Expand of char * char  (* replace the first char of 2 chars by 2 chars             *)

(* remark: every real operation makes the map2 function to create a new buffer (even the for the replace operation). *)
(* Indeed, we  want to keep the string immutable in xapi, in order to avoid nasty string modifications.              *)
let map2_unlikely f s =
  let changed = ref false in
  let m = ref 0 in
  let i = ref 0 in
  let buf = Buffer.create 0 in
  let length_s = String.length s in
  let aux c d =
    match f (c, d) with
    | No_change -> ()
    | Replace (c,d) ->
      changed := true;
      Buffer.add_substring buf s !m (!i - !m);
      Buffer.add_char buf c;
      Buffer.add_char buf d;
      incr i;
      m := !i + 1
    | Compress char ->
      changed := true;
      Buffer.add_substring buf s !m (!i - !m);
      Buffer.add_char buf char;
      incr i;
      m := !i + 1
    | Expand (c,d) ->
      changed := true;
      Buffer.add_substring buf s !m (!i - !m);
      Buffer.add_char buf c;
      Buffer.add_char buf d;
      m := !i + 1
  in
  (* main loop *)
  while !i <= length_s - 2 do
    aux s.[!i] (Some s.[!i+1]);
    incr i;
  done;
  (* process the last character *)
  if !i = length_s - 1
  then aux s.[!i] None;
  if !changed then begin
    Buffer.add_substring buf s !m (String.length s - !m);
    Buffer.contents buf
  end else
    s

let protect_fn = function
  |  ' ',       _  -> Expand  (protect_char, '.' )
  | '\t',       _  -> Expand  (protect_char, 't' )
  | '\n',       _  -> Expand  (protect_char, 'n' )
  | '\r',       _  -> Expand  (protect_char, 'r' )
  |   c,        _ when c = protect_char ->
    Expand (protect_char, protect_char)
  |   _ ,       _  -> No_change

let unprotect_fn = function
  (* CA-268761: this is only for backward compatibility *)
  | c, Some '_' when c=protect_char -> Replace (' ', ' ')
  | c, Some '.' when c=protect_char -> Compress ' '
  | c, Some 't' when c=protect_char -> Compress '\t'
  | c, Some 'n' when c=protect_char -> Compress '\n'
  | c, Some 'r' when c=protect_char -> Compress '\r'
  | c, Some  d  when c=protect_char && d=protect_char ->
    Compress protect_char
  | _ ,      _                      -> No_change

let protect = map2_unlikely protect_fn
let unprotect = map2_unlikely unprotect_fn
