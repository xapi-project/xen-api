(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

(** Printer combinators *)

type 'a printer = 'a -> string


(** Printers for basic types *)

let unit      : unit printer      = (fun () -> "()")
let int64     : int64 printer     = Printf.sprintf "%Ld"
let int32     : int32 printer     = Printf.sprintf "%ld"
let nativeint : nativeint printer = Printf.sprintf "%nd"
let int       : int printer       = Printf.sprintf "%d"
let string    : string printer    = Printf.sprintf "%S"
let char      : char printer      = Printf.sprintf "%C"
let float     : float printer     = Printf.sprintf "%F" 
let exn       : exn printer       = Printexc.to_string


(** Printer combinators for compound types *)

let option : 'a printer -> 'a option printer = fun pr x ->
	match x with
	| None -> "None"
	| Some s -> pr s |> Printf.sprintf "Some %s"

let either : 'a printer -> 'b printer -> ('a, 'b) Either.t printer = fun pr_a pr_b x ->
	match x with
	| Either.Left a ->  pr_a a |> Printf.sprintf "Left %s"
	| Either.Right b -> pr_b b |> Printf.sprintf "Right %s"

(* Utility function to bracket a string *)
let bracket l r x = Printf.sprintf "%s%s%s" l x r

let pair : 'a printer -> 'b printer -> ('a * 'b) printer = fun pr_a pr_b (a, b) ->
	[pr_a a; pr_b b] |> String.concat ", " |> bracket "(" ")"

(* Print a pair from an association list as "key: value" *)
let assoc_pair : 'a printer -> 'b printer -> ('a * 'b) printer = fun pr_a pr_b (a, b) ->
	[pr_a a; pr_b b] |> String.concat ": "

let array : 'a printer -> 'a array printer = fun pr x ->
	x |> Array.map pr |> Array.to_list |> String.concat "; " |> bracket "[|" "|]"

let list : 'a printer -> 'a list printer = fun pr x -> 
	x |> List.map pr |> String.concat "; " |> bracket "[" "]"

(* Print an association list as "key: value; key: value" *)
let assoc_list : 'a printer -> 'b printer -> ('a * 'b) list printer = fun pr_a pr_b x ->
	x |> (assoc_pair pr_a pr_b |> list)

