(*
 * Copyright (C) 2015 Citrix Inc
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
open Result
open Lwt

let project_url = "http://github.com/djs55/shared-block-ring"

module Log = struct
  let debug fmt = Lwt_log.debug_f fmt
  let info fmt = Lwt_log.info_f fmt
  let error fmt = Lwt_log.error_f fmt

  let trace _ = Lwt.return ()
end

module R = Shared_block.Ring.Make(Log)(Block)(struct
  type t = string
  let to_cstruct x =
    let r = Cstruct.create (String.length x) in
    Cstruct.blit_from_string x 0 r 0 (String.length x);
    r
  let of_cstruct x = Some (Cstruct.to_string x)
end)
open R

let rec bind fn f = fn () >>= function
  | Error `Suspended -> fail (Failure "Ring is suspended")
  | Error (`Msg x) -> fail (Failure x)
  | Error `Retry ->
    Lwt_unix.sleep 5.
    >>= fun () ->
    bind fn f
  | Ok x -> f x
let (>>|=) = bind

let produce filename _interval =
  let t =
    Block.connect filename >>= fun disk ->
    Producer.attach ~disk
    >>|= fun p ->
    let rec loop () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun item ->
      let write () =
        Producer.push ~t:p ~item
        >>|= fun position ->
        Producer.advance ~t:p ~position
        >>|= fun () ->
        return () in
      write ()
      >>= fun () ->
      loop () in
    loop () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error (false, Printexc.to_string e)

let consume filename _interval =
  let t =
    Block.connect filename >>= fun disk ->
    Consumer.attach ~disk
    >>|= fun c ->
    let rec loop () =
      Consumer.pop ~t:c
      >>|= fun (position, item) ->
      Lwt_io.write_line Lwt_io.stdout item
      >>= fun () ->
      Consumer.advance ~t:c ~position
      >>|= fun () ->
      loop () in
    loop () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error(false, Printexc.to_string e)

let create filename =
  let t =
    Block.connect filename >>= fun disk ->
    let module Eraser = Shared_block.EraseBlock.Make(Block) in
    Eraser.erase ~pattern:(Printf.sprintf "shared-block-ring/example/main.ml erased the %s volume; " filename) disk
    >>= function
    | Error _ -> fail (Failure (Printf.sprintf "Failed to erase %s" filename))
    | Ok () ->
    Producer.create ~disk >>|= fun _ -> return () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error(false, Printexc.to_string e)

let diagnostics filename =
  let t =
    Block.connect filename >>= fun disk ->
    Consumer.attach ~disk
    >>|= fun c ->
    let rec loop ?from () =
      Consumer.pop ~t:c ?from ()
      >>= function
      | Error `Retry ->
        Lwt_io.write_line Lwt_io.stdout "-- there are no more items"
      | x ->
        (fun () -> return x) >>|= fun (from, buf) ->
        Lwt_io.write_line Lwt_io.stdout (Printf.sprintf "%s: %s" (Sexplib.Sexp.to_string (Consumer.sexp_of_position from)) buf)
        >>= fun () ->
        loop ~from () in
    loop () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error(false, Printexc.to_string e)

let suspend filename =
  let t =
    Block.connect filename >>= fun disk ->
    Consumer.attach ~disk
    >>|= fun c ->
    (fun () -> Consumer.suspend c)
    >>|= fun () ->
    return () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error(false, Printexc.to_string e)

let resume filename =
  let t =
    Block.connect filename >>= fun disk ->
    Consumer.attach ~disk
    >>|= fun c ->
    (fun () -> Consumer.resume c)
    >>|= fun () ->
    return () in
  try
    `Ok (Lwt_main.run t)
  with e ->
    `Error(false, Printexc.to_string e)

open Cmdliner

(* Help sections common to all commands *)

let help = [
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

let filename =
  let doc = "Path to the device or file containing the ring." in
  Arg.(value & pos 0 file "test.raw" & info [] ~doc)

let interval =
  let doc = "Time in seconds between I/O retries." in
  Arg.(value & opt int 5 & info [ "interval" ] ~doc)

let produce_cmd =
  let doc = "Push data onto the ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Read lines of text from stdin and push them as individual items onto the ring.";
  ] @ help in
  Term.(ret(pure produce $ filename $ interval)),
  Term.info "produce" ~doc ~man

let consume_cmd =
  let doc = "Pop data from the ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Read lines of text from the ring and print them to stdout.";
  ] @ help in
  Term.(ret(pure consume $ filename $ interval)),
  Term.info "consume" ~doc ~man

let create_cmd =
  let doc = "Create an empty ring" in
  let man = [
    `S "DESCRIPTION";
    `P "Initialise a device or file with an empty ring."
  ] @ help in
  Term.(ret(pure create $ filename)),
  Term.info "create" ~doc ~man

let diagnostics_cmd =
  let doc = "Display the current state of a ring." in
  let man = [
    `S "DESCRIPTION";
    `P "Display the current ring state including producer and consumer pointers, together with the current ring contents for diagnostic purposes.";
    `P "Note: the ring will not be modified."
  ] @ help in
  Term.(ret(pure diagnostics $ filename)),
  Term.info "diagnostics" ~doc ~man

let suspend_cmd =
  let doc = "Suspend the ring." in
  let man = [
    `S "DESCRIPTION";
    `P "Perform a co-operative suspend of the ring. Once finished, the producer will have acknowledged and promise not to send any more data.";
  ] @ help in
  Term.(ret(pure suspend $ filename)),
  Term.info "suspend" ~doc ~man

let resume_cmd =
  let doc = "Resume the ring." in
  let man = [
    `S "DESCRIPTION";
    `P "Perform a co-operative resume of the ring. Once finished, the producer will have acknowledged and will be able to produce data.";
  ] @ help in
  Term.(ret(pure resume $ filename)),
  Term.info "resume" ~doc ~man

let default_cmd =
  let doc = "manipulate shared rings on block devices" in
  let man = help in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~doc ~man

let cmds = [create_cmd; produce_cmd; consume_cmd; suspend_cmd; resume_cmd; diagnostics_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
