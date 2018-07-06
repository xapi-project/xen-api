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
let bool      : bool printer      = Printf.sprintf "%B"
let exn       : exn printer       = Printexc.to_string


(** Printer combinators for compound types *)

let option : 'a printer -> 'a option printer = fun pr x ->
	match x with
	| None -> "None"
	| Some s -> pr s |> Printf.sprintf "Some %s"

let either : 'a printer -> 'b printer -> ('a, 'b) Xapi_stdext_monadic.Either.t printer = fun pr_a pr_b x ->
	match x with
	| Xapi_stdext_monadic.Either.Left a ->  pr_a a |> Printf.sprintf "Left %s"
	| Xapi_stdext_monadic.Either.Right b -> pr_b b |> Printf.sprintf "Right %s"

(* Utility function to bracket a string *)
let bracket l r x = Printf.sprintf "%s%s%s" l x r

let pair : 'a printer -> 'b printer -> ('a * 'b) printer = fun pr_a pr_b (a, b) ->
	[pr_a a; pr_b b] |> String.concat ", " |> bracket "(" ")"

let tuple3 : 'a printer -> 'b printer -> 'c printer ->
	('a * 'b * 'c) printer = fun pr_a pr_b pr_c (a, b, c) ->
	[pr_a a; pr_b b; pr_c c] |> String.concat ", " |> bracket "(" ")"

let tuple4 : 'a printer -> 'b printer -> 'c printer -> 'd printer ->
	('a * 'b * 'c * 'd) printer = fun pr_a pr_b pr_c pr_d (a, b, c, d) ->
	[pr_a a; pr_b b; pr_c c; pr_d d] |> String.concat ", " |> bracket "(" ")"

let tuple5 : 'a printer -> 'b printer -> 'c printer -> 'd printer -> 'e printer ->
	('a * 'b * 'c * 'd * 'e) printer = fun pr_a pr_b pr_c pr_d pr_e (a, b, c, d, e) ->
	[pr_a a; pr_b b; pr_c c; pr_d d; pr_e e] |> String.concat ", " |> bracket "(" ")"

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

