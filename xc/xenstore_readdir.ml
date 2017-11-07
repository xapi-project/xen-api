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

let debug_enabled = ref false
let print_error fmt = Printf.fprintf stderr fmt
let print_debug fmt = Printf.kprintf (fun s -> if !debug_enabled then output_string stderr s) fmt

open Printf
open Xenstore

let catch_invalid message f x =
  try
    f x
  with Xenbus.Xb.Invalid as e ->
    print_error "Caught Xenbus.Xb.Invalid while executing: %s" message;
    raise e

let readdir ?(full_paths=true) ~xs path =
  print_debug "Calling list of <%s>" path;
  let paths = xs.Xs.directory path in
  print_debug "paths = [ %s ]" (String.concat "  " paths);
  if paths = [ "" ] then [] (* empty directory *)
  else (if full_paths (* Note "//" is an Invalid path *)
        then List.map (fun x -> (if path = "/" then "" else path) ^ "/" ^ x) paths
        else paths)

let ls ~xs = function
  | [ path ] ->
    print_endline ("Contents of " ^ path ^ ":");
    let paths = readdir xs path in
    print_debug "paths = [ %s ]" (String.concat "  " paths);
    let longest = List.fold_left max 0 (List.map String.length paths) in
    (*      let perms = List.map (fun x -> catch_invalid (Printf.sprintf "getperms %s" x) xs.Xs.getperms x)
            	paths in *)
    List.iter (fun path ->
        let perm = catch_invalid (Printf.sprintf "getperms %s" path)
            xs.Xs.getperms path in
        print_string "    ";
        print_string path;
        for i = String.length path to longest + 5
        do
          print_string " "
        done;
        print_endline (Xsraw.string_of_perms perm)) paths
  | _ -> failwith "ls takes exactly one argument"

let debug ~xs commands = 
  let contents = xs.Xs.debug commands in
  print_endline contents

let read ~xs = function
  | [ path ] ->
    let contents = xs.Xs.read path in
    print_endline contents
  | _ -> failwith "read takes exactly one argument"

let rm ~xs = function
  | [ path ] -> xs.Xs.rm path
  | _ -> failwith "rm takes exactly one argument"

let mkdir ~xs = function
  | [ path ] -> xs.Xs.mkdir path
  | _ -> failwith "mkdir takes exactly one argument"

let watch ~xs = function
  | [ path ] ->
    xs.Xs.watch path "1";
    print_debug "Added watch to path: %s" path;
    while true do
      let x, _ = Xs.read_watchevent xs in
      Printf.printf "%s\n" x;
      flush stdout
    done
  | _ -> failwith "watch takes exactly one argument"

let write ~xs = function
  | [ path; data ] ->
    xs.Xs.write path data
  | _ -> failwith "write takes a path and a value to write"

let functions = [ "ls", (ls, "List a directory");
                  "rm", (rm, "Remove a path");
                  "mkdir", (mkdir, "Make a directory");
                  "read", (read, "Read a value from a path");
                  "write", (write, "Write a value to a path");
                  "watch", (watch, "Watch a path");
                  "debug", (debug, "Debug commands");
                ]

let available_commands () =
  String.concat "\n" (List.map (fun (command, (_, descr)) -> command ^ ": " ^ descr) functions)

let _ =
  let paths = ref [] in
  let mode = ref "" in
  Arg.parse []
    (fun x -> if !mode = "" then mode := x
      else paths := x :: !paths)
    (Printf.sprintf "Manipulate xenstore\nAvailable commands are:\n%s\n" (available_commands ()));
  let paths = List.rev !paths in
  let mode = !mode in

  let xs = Xs.daemon_open () in

  if not(List.mem_assoc mode functions)
  then begin
    Printf.printf "Available commands are:\n%s\n" (available_commands ());
    exit 1
  end;
  let cmd, _ = List.assoc mode functions in
  cmd ~xs paths

