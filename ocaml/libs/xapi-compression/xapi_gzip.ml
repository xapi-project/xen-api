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

(* This is a minimal test CLI to exercise gzip for compression and
   uncompression using the Gzip module; which in turn spawns a process via
   Forkexecd *)

module C = Cmdliner

module Gzip = Xapi_compression.Make (struct
  let executable = "/bin/gzip"

  let compress_options = []

  let decompress_options = ["--decompress"; "--stdout"]
end)

let help =
  [
    `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ]

(** copy bytes from file descriptor [src] to [dst] *)
let copy ~src ~dst =
  let size = 1024 * 64 in
  let buffer = Bytes.create size in
  let rec loop () =
    match Unix.read src buffer 0 size with
    | 0 ->
        ()
    | n ->
        ignore (Unix.write dst buffer 0 n) ;
        loop ()
  in
  loop ()

let gzip ~src ~dst = Gzip.compress dst (fun dst -> copy ~src ~dst)

let gunzip ~src ~dst = Gzip.decompress_passive src (fun src -> copy ~src ~dst)

let compress uncomp =
  match uncomp with
  | false ->
      gzip ~src:Unix.stdin ~dst:Unix.stdout
  | true ->
      gunzip ~src:Unix.stdin ~dst:Unix.stdout

let decompress =
  let doc = "Decompress." in
  C.Arg.(
    value
    & flag
    & info ["decompress"; "uncompress"; "d"] ~docv:"DECOMPRESS" ~doc
  )

let main =
  let doc = "Test compression and uncompression" in
  C.Term.(const compress $ decompress, info "xapi-gzip" ~doc ~man:help)

let () = if !Sys.interactive then () else C.Term.(exit @@ eval main)
