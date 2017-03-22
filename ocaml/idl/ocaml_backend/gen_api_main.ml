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
(* Front end around the API code generator. Central place for filtering *)

open Datamodel_types

let filter = ref None
let filterinternal = ref false

module Field = struct
  let filter_internal x = if !filterinternal then not x.internal_only else true
  let opensource x = (List.mem "3.0.3"  x.release.opensource) && (filter_internal x)
  let closed x     = (List.mem "closed" x.release.internal) && (filter_internal x)
  let debug x      = (List.mem "debug"  x.release.internal) && (filter_internal x)
  let nothing x    = filter_internal x
end
module Message = struct
  let filter_internal x = if !filterinternal then not x.msg_db_only else true
  let opensource x = (List.mem "3.0.3"  x.msg_release.opensource) && (filter_internal x)
  let closed x     = (List.mem "closed" x.msg_release.internal) && (filter_internal x)
  let debug x      = (List.mem "debug"  x.msg_release.internal) && (filter_internal x)
  let nothing x    = filter_internal x
end

let filter_api () =
  let api_used = Datamodel.all_api in
  (* Add all implicit messages to the API directly *)
  let api_used = Datamodel_utils.add_implicit_messages api_used in
  let filterfn = Dm_api.filter (fun _ -> true) in
  match !filter with
  | None ->                  api_used
  | Some "opensource" ->     filterfn Field.opensource Message.opensource api_used
  | Some "closed" ->         filterfn Field.closed Message.closed api_used
  | Some "debug" ->          filterfn Field.debug Message.debug api_used
  | Some "nothing" ->        filterfn Field.nothing Message.nothing api_used
  | Some x ->                Printf.eprintf "Unknown filter mode: %s\n" x;
    api_used

let set_gendebug () =
  Gen_server.enable_debugging := true

let mode = ref None

let _ =
  Arg.parse
    [
      "-mode",
      Arg.Symbol (["client"; "server"; "api"; "db"; "actions"; "sql"; "rbac"; "test"],
                  fun x -> mode := Some x),
      "Choose which file to output";
      "-filter",
      Arg.Symbol (["opensource"; "closed"; "debug"; "nothing"],
                  fun x -> filter := Some x),
      "Apply a filter to the API";

      "-filterinternal",
      Arg.Bool (fun x -> filterinternal := x),
      "Filter internal fields and messages";

      "-gendebug",
      Arg.Unit (fun _ -> set_gendebug ()),
      "Add debugging code to generated output";

      "-output",
      Arg.String (fun s -> begin try Unix.mkdir (Filename.dirname s) 0o755 with Unix.Unix_error(Unix.EEXIST,_,_) -> () end ; Gen_api.oc := (open_out s)),
      "Output to the specified file";

    ] (fun x -> Printf.eprintf "Ignoring argument: %s\n" x)
    "Generate ocaml code from the datamodel. See -help";

  let api = filter_api () in
  match !mode with
  | None -> Printf.eprintf "Must select an output type with -mode\n"
  | Some "client" ->
    Gen_api.gen_client api
  | Some "api" ->
    Gen_api.gen_client_types api
  | Some "server" ->
    Gen_api.gen_server api
  | Some "db" ->
    Gen_api.gen_db_actions api
  | Some "actions" ->
    Gen_api.gen_custom_actions api
  | Some "rbac" ->
    Gen_api.gen_rbac api
  | Some "test" ->
    Gen_test.gen_test api
  | Some x -> Printf.eprintf "Didn't recognise mode: %s\n" x
