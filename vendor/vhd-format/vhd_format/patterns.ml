(*
 * Copyright (C) 2011-2013 Citrix Inc
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

(* Interesting set of I/O patterns for testing a vhd implementation *)

let kib = 1024L
let mib = Int64.(mul 1024L kib)
let gib = Int64.(mul 1024L mib)
let max_disk_size = Int64.(mul 2040L gib)

(* Interesting virtual sizes of disks *)
let sizes = [
  0L;            (* edge case: minimum size *)
  4194304L;      (* common case: 2 blocks *)
  max_disk_size; (* edge case: maximum size *)
]

(* Places within an array (either a sector bitmap or BAT) *)
type choice =
  | First        (* edge case: first entry *)
  | Last         (* edge case: last entry *)

let choices = [ First; Last ]

let string_of_choice = function
  | First  -> "first"
  | Last   -> "last"

(* Position to read or write in a vhd *)
type position = {
  block: choice;
  sector: choice;
}

let rec allpairs xs ys = match xs with
  | [] -> []
  | x :: xs -> List.map (fun y -> x, y) ys @ (allpairs xs ys)

let positions =
  List.map (fun (block, sector) -> { block; sector }) (allpairs choices choices)

(* Individual step *)
type operation =
  | Create of int64 (* Create a vhd of a given size; open file for I/O *)
  | Snapshot        (* Snapshot current file; open new file for I/O *)
  | Write of (position * string) (* Write copies of a given string over a specific sector *)

let descr_of_operation = function
  | Create x -> [
      Printf.sprintf "filename := Vhd.create(size = %Ld)" x;
      "current := Vhd.open(filename)"
    ]
  | Snapshot -> [
      "filename := Vhd.snapshot(current)"; 
      "current := Vhd.open(filename)"
    ]
  | Write (p, message) -> [
      Printf.sprintf "Vhd.write(current, block = %s, sector = %s, data = \"%s\")"
        (string_of_choice p.block) (string_of_choice p.sector)
        (String.escaped message)
    ]

type program = operation list

let string_of_operation = function
  | Create x -> Printf.sprintf "Create:%Ld" x
  | Snapshot -> "Snapshot"
  | Write (p, _) -> Printf.sprintf "Write:%s:%s" (string_of_choice p.block) (string_of_choice p.sector)

let descr_of_program p =
  let lines = List.concat (List.map descr_of_operation p) in
  List.rev (fst (List.fold_left (fun (sofar, next) line ->
    Printf.sprintf "%d %s" (next * 10) line :: sofar, next + 1
  ) ([], 1) lines))

let string_of_program p = String.concat "_" (List.map string_of_operation p)

let first_write_message = "This is a sector which contains simple data.\n"
let second_write_message = "All work and no play makes Dave a dull boy.\n"
let first_write p = Write(p, first_write_message)
let second_write p = Write(p, second_write_message)

(* Check writing and then reading back works *)
let create_write_read =
  List.map (fun (size, p) ->
    [ Create size; first_write p ]
  ) (allpairs sizes positions)

(* Check writing and then reading back works in a simple chain *)
let create_write_read_leaf =
  List.map (fun (size, p) ->
    [ Create size; Snapshot; first_write p ]
  ) (allpairs sizes positions)

(* Check writing and then reading back works in a chain where the writes are in the parent *)
let create_write_read_parent =
  List.map (fun (size, p) ->
    [ Create size; first_write p; Snapshot ]
  ) (allpairs sizes positions)

(* Check writing and then reading back works in a chain where there are writes in both parent and leaf *)
let create_write_overwrite =
  List.map (fun (size, (p1, p2)) ->
    [ Create size; first_write p1; Snapshot; second_write p2 ]
  ) (allpairs sizes (allpairs positions positions))

(* TODO: ... and all of that again with a larger leaf *)

let programs =
  List.concat [
    create_write_read;
    create_write_read_leaf;
    create_write_read_parent;
    create_write_overwrite;
]
