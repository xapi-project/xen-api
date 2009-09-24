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
exception No_value
let value ex = function
  | None -> raise ex
  | Some x -> x

let may f = function
  | None -> None
  | Some x -> Some (f x)

let default d = function
  | None -> d
  | Some x -> x

let rec optlistToList = function
  | [] -> []
  | None::xs -> optlistToList xs
  | (Some x)::xs -> x::(optlistToList xs)
			    

(* bit o sets *)

let rec remove fromset element = match fromset with
| [] -> []
| (x::xs) -> 
    if x = element then xs 
    else x::(remove xs element) 

(* this looks like a fold *)
let rec subtract fromset elements = match elements with
| [] -> fromset
| (x::xs) -> subtract (remove fromset x) xs

exception Empty_List

let hd = function
  | [] -> raise Empty_List
  | (x::_) -> x

let tl = function
  | [] -> raise Empty_List
  | (_::xs) -> xs


let rec member set x = match set with 
| [] -> false
| (y::ys) -> if (x = y) then true else (member ys x)


let rec length = function
  | [] -> 0
  | (_::xs) -> 1 + (length xs)


let rec explode s = 
  if (String.length s = 0) then []
  else (String.get s 0)::(explode (String.sub s 1 (String.length s - 1)))

let rec implode chars = 
  let s = String.create (length chars) in
  let rec setchar n = function
    | [] -> ()
    | (c::cs) -> String.set s n c; setchar (n+1) cs

  in  setchar 0 chars; s

let split s c =
  let rec search results acc tocome = match tocome with
  | [] -> results @ [acc]
  | (x::xs) -> if (c x) then search (results @ [acc]) [] xs
      else search results (acc @ [x]) xs

  in
  List.map implode (search [] [] (explode s))

let tokenize s = 
  let whitespace = function
    | ' '  -> true
    | '\t' -> true
    | '\r' -> true
    | '\n' -> true
    | _    -> false
  and not_blank = function x -> (x <> "") in

  let split_string = split s whitespace in
  let tokens = List.filter not_blank split_string in
  tokens
