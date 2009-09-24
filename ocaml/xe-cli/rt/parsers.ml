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
(* Functions to parse the output of several programs *)

(* Explode *)

let explode str c =
  let rec inner start cur =
    try
      let i = String.index_from str start c in
      inner (i+1) ((String.sub str start (i-start))::cur)
    with 
      Not_found -> (String.sub str start (String.length str - start))::cur
  in List.rev (inner 0 [])


(* Just remove whitespace from beginning and end *)
let zap_whitespace s =
  let is_whitespace = function | ' ' -> true | '\t' -> true | '\r' -> true | '\n' -> true | _ -> false in
  let rec first_nonwhitespace n =
    if n>=String.length s then 0 else
    if (is_whitespace s.[n]) then first_nonwhitespace (n+1) else n in
  let rec last_nonwhitespace n =
    if n<0 then n else
    if (is_whitespace s.[n]) then last_nonwhitespace (n-1) else n in
  let f = first_nonwhitespace 0 in
  let l = last_nonwhitespace (String.length s - 1) in
  if l= -1 then "" else String.sub s f (l-f+1)


(* Two functions to parse the output of 'xe'. Result is a list of association lists *)
let parse_record ls =
   let rec inner ls cur =
     match ls with
       ""::rest -> (cur,rest)
     | l::ls ->
       let colon = String.index l ':' in
       let token = zap_whitespace (String.sub l 0 colon) in
       let value = zap_whitespace (String.sub l (colon+1) (String.length l - colon - 1)) in
       inner ls ((token,value)::cur)
     | _ -> (cur,[])
  in
  inner ls []

let parse ls = 
   let rec inner ls cur =
     match ls with 
       [] -> cur
     | records -> 
        let (record,rest) = parse_record ls in
        inner rest (record::cur)
   in 
   inner ls []


(* Get the last domid from a run of 'list_domains' *)
let parse_ld_line l =
  let args = explode l '|' in
  let args = List.map zap_whitespace args in
  let domid = int_of_string (List.nth args 0) in
  let uuid = List.nth args 1 in
  let state = List.nth args 2 in
  (domid,uuid,state)

let parse_ld ls =
  let rec inner ls cur =
    match ls with
      [] -> cur
    | l::ls ->
	inner ls ((parse_ld_line l)::cur)
  in inner (List.tl ls) []


(* Extract the ip address from a windows ipconfig type command *)
let parse_ipconfig ls =
  let line2 = List.nth ls 1 in
  if String.sub line2 0 7 <> "Windows" then raise (Failure ""); (* trapped in parse_ip *)
  let ipline = List.nth ls 7 in
  let colon = String.index ipline ':' in
  let ipaddr = zap_whitespace (String.sub ipline (colon+1) (String.length ipline - colon - 1)) in
  ipaddr
  

(* Extract the ip address from a linux guest - done on guest now*)
let parse_ifconfig ls =
  try
    zap_whitespace (List.hd ls)
  with
    _ -> raise (Failure "Failed to parse IP address")
    

(* Try to parse it as a windows ipconfig output first, if not, a linux type *)
let parse_ip ls =
  try 
    parse_ipconfig ls
  with 
    _ -> parse_ifconfig ls



      
