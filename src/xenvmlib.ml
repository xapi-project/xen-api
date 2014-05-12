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
(* api to connect/start mini monitor *)

(** xenvm library *)

open Xstringext

type answer = Ok | Error of string | Msg of string | Unknown of string

let string_of_answer answer =
	match answer with
	| Ok        -> "CMD_OK"
	| Error s   -> "CMD_ERROR " ^ s
	| Msg s     -> "CMD_MSG " ^ s
	| Unknown s -> s

let answer_of_string s =
	if s = "CMD_OK" then
		Ok
	else if String.startswith "CMD_ERROR " s then
		Error (String.sub s 10 (String.length s - 10))
	else if String.startswith "CMD_MSG " s then
		Msg (String.sub s 8 (String.length s - 8))
	else
		Unknown s

let dowrite fd buf =
	let len = String.length buf in
	let left = ref len in
	while !left > 0
	do
		let wr = Unix.write fd buf (len - !left) (!left) in
		left := !left - wr
	done

let doread fd =
	let b = Buffer.create 1024 in
	let buf = String.create 1024 in
	let quit = ref false in
	while not !quit
	do
		let rd = Unix.read fd buf 0 1024 in
		if rd = 0 then
			quit := true
		else
			Buffer.add_substring b buf 0 rd
	done;
	Buffer.contents b

(** request s from a running xenvm identified by id (uuid or name) *)
let request id s =
	let filename = Printf.sprintf "/var/lib/xenvm/vm-%s" id in
	let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
	Unix.connect fd (Unix.ADDR_UNIX filename);

	dowrite fd (s ^ "!");
	let answer = doread fd in
	Unix.close fd;

	answer_of_string answer
