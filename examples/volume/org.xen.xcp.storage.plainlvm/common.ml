(*
 * Copyright (C) Citrix Systems Inc.
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

let version = "1.0.0"
let project_url = "https://github.com/xapi-project/ezlvm"

(* Utility functions common to all scripts.
   Perhaps these should be moved into the Xcp_service library? *)

let ignore_string (_: string) = ()

let log fmt =
  Printf.ksprintf
    (fun s ->
      output_string stderr s;
      output_string stderr "\n";
      flush stderr;
      ) fmt
let debug fmt = log fmt
let warn fmt = debug fmt
let error fmt = debug fmt

type t = {
  verbose: bool;
  debug: bool;
  test: bool;
  json: bool;
}
(** options common to all subcommands *)

let make verbose debug test json =
  { verbose; debug; test; json }

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let string_of_file filename =
  let ic = open_in filename in
  let output = Buffer.create 1024 in
  try
    while true do
      let block = String.make 4096 '\000' in
      let n = input ic block 0 (String.length block) in
      if n = 0 then raise End_of_file;
      Buffer.add_substring output block 0 n
    done;
    "" (* never happens *)
  with End_of_file ->
    close_in ic;
    Buffer.contents output

let file_of_string filename string =
  let oc = open_out filename in
  finally
    (fun () ->
      debug "write >%s" filename;
      output oc string 0 (String.length string)
    ) (fun () -> close_out oc)

let startswith prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  x' >= prefix' && (String.sub x 0 prefix' = prefix)

let remove_prefix prefix x =
  let prefix' = String.length prefix in
  let x' = String.length x in
  String.sub x prefix' (x' - prefix')

let endswith suffix x =
  let suffix' = String.length suffix in
  let x' = String.length x in
  x' >= suffix' && (String.sub x (x' - suffix') suffix' = suffix)

let iso8601_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%04d%02d%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec


(** create a directory, and create parent if doesn't exist *)
let mkdir_rec dir perm =
  let mkdir_safe dir perm =
    try Unix.mkdir dir perm with Unix.Unix_error (Unix.EEXIST, _, _) -> () in
  let rec p_mkdir dir =
    let p_name = Filename.dirname dir in
    if p_name <> "/" && p_name <> "."
    then p_mkdir p_name;
    mkdir_safe dir perm in
  p_mkdir dir

let rm_f x =
  try
    Unix.unlink x;
    debug "rm %s" x
   with _ ->
    debug "%s already deleted" x;
    ()

let ( |> ) a b = b a

let retry_every n f =
  let finished = ref false in
  while (not !finished) do
    try
      let () = f () in
      finished := true;
    with e ->
      debug "Caught %s: sleeping %f. before trying again" (Printexc.to_string e) n;
      Thread.delay n
  done

(* From Xcp_service: *)
let colon = Re_str.regexp_string ":"

let canonicalise x =
  if not(Filename.is_relative x)
  then x
  else begin
    (* Search the PATH and XCP_PATH for the executable *)
    let paths = Re_str.split colon (Sys.getenv "PATH") in
    let xen_paths = try Re_str.split colon (Sys.getenv "XCP_PATH") with _ -> [] in
    let first_hit = List.fold_left (fun found path -> match found with
      | Some hit -> found
      | None ->
        let possibility = Filename.concat path x in
        if Sys.file_exists possibility
        then Some possibility
        else None
    ) None (paths @ xen_paths) in
    match first_hit with
    | None ->
      warn "Failed to find %s on $PATH ( = %s) or $XCP_PATH ( = %s)" x (Sys.getenv "PATH") (try Sys.getenv "XCP_PATH" with Not_found -> "unset");
      x
    | Some hit -> hit
  end

let run ?(env= [| |]) ?stdin cmd args =
  let cmd = canonicalise cmd in
  debug "%s %s" cmd (String.concat " " args);
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let to_close = ref [ null ] in
  let close fd =
    if List.mem fd !to_close then begin
      to_close := List.filter (fun x -> x <> fd) !to_close;
      Unix.close fd
    end in
  let close_all () = List.iter close !to_close in
  try
    let b = Buffer.create 128 in
    let tmp = String.make 4096 '\000' in
    let readable, writable = Unix.pipe () in
    to_close := readable :: writable :: !to_close;

    let stdin_readable, stdin_writable = Unix.pipe () in
    to_close := stdin_readable :: stdin_writable :: !to_close;

    let pid = Unix.create_process_env cmd (Array.of_list (cmd :: args)) env stdin_readable writable Unix.stderr in
    close writable;
    close stdin_readable;
    (* assume 'stdin' is small such that this won't block *)
    begin match stdin with
    | None -> ()
    | Some txt ->
      let n = Unix.write stdin_writable txt 0 (String.length txt) in
      if n <> (String.length txt)
      then failwith (Printf.sprintf "short write to process stdin: only wrote %d bytes" n);
    end;
    close stdin_writable;
    let finished = ref false in
    while not !finished do
      let n = Unix.read readable tmp 0 (String.length tmp) in
      Buffer.add_substring b tmp 0 n;
      finished := n = 0
    done;
    close_all ();
    let _, status = Unix.waitpid [] pid in
    match status with
    | Unix.WEXITED 0 -> Buffer.contents b
    | Unix.WEXITED n ->
      failwith (Printf.sprintf "%s %s: %d (%s)" cmd (String.concat " " args) n (Buffer.contents b))
    | _ ->
      failwith (Printf.sprintf "%s %s failed" cmd (String.concat " " args))
  with e ->
    close_all ();
    raise e

module Int64 = struct
  include Int64

  let ( + ) = Int64.add
  let ( - ) = Int64.sub
  let ( * ) = Int64.mul
  let ( / ) = Int64.div
end

let kib = 1024L
let mib = Int64.(kib * kib)
let gib = Int64.(mib * kib)
let tib = Int64.(gib * kib)

open Cmdliner

let _common_options = "COMMON OPTIONS"

let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  let test =
    let doc = "Perform self-tests." in
    Arg.(value & flag & info ["test"] ~docs ~doc) in
  let json =
    let doc = "Expect json on stdin, print json on stdout." in
    Arg.(value & flag & info ["json"] ~docs ~doc) in
  Term.(pure make $ debug $ verb $ test $ json)

let help = [
 `S _common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

module type Command = sig
  module In : sig
    type t
    val t_of_rpc: Rpc.t -> t
    val rpc_of_t: t -> Rpc.t
  end
  module Out : sig
    type t
    val t_of_rpc: Rpc.t -> t
    val rpc_of_t: t -> Rpc.t
  end
  module CommandLine : sig
    val doc: string
    val t: In.t Cmdliner.Term.t
  end
  val command: t -> In.t -> Out.t
end
module type Test = sig
  val test: t -> unit
end

module Make(C: Command)(T: Test) = struct

let wrap common args =
  let args =
    if common.json
    then read_line () |> Jsonrpc.of_string |> C.In.t_of_rpc
    else args in
  if common.test then begin
    T.test common;
    `Ok ()
  end else begin
    let result = C.command common args in
    let rpc = C.Out.rpc_of_t result in
    print_endline (Jsonrpc.to_string rpc);
    `Ok ()
  end

let cmd =
  let doc = C.CommandLine.doc in
  let man = help in
  Term.(ret (pure wrap $ common_options_t $ C.CommandLine.t)),
  Term.info Sys.argv.(0) ~version:version ~sdocs:_common_options ~doc ~man

let main () =
  match Term.eval cmd with
  | `Error _ -> exit 1
  | _ -> exit 0

end

let vg_of_uri uri =
  let uri' = Uri.of_string uri in
  match Uri.scheme uri' with
  | Some "vg" ->
    let vg = Uri.path uri' in
    if vg <> "" && vg.[0] = '/' then String.sub vg 1 (String.length vg - 1) else vg
  | _ -> raise (Xapi_storage.V.SR_does_not_exist uri)
