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

open Test_highlevel

let dir = Filename.concat "test" "jsonrpc_files"

let pp_jsonrpc fmt rpc = Format.fprintf fmt "%s" (Jsonrpc.to_string rpc)

module Input_json_object = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = (Rpc.t, exn) result

    let string_of_input_t = Test_printers.string

    let string_of_output_t =
      Fmt.(str "%a" Dump.(result ~ok:pp_jsonrpc ~error:exn))
  end

  let good_call =
    let fin = open_in (Filename.concat dir "good_call.json") in
    let s = input_line fin in
    close_in fin ; Jsonrpc.of_string s

  exception Parse_error

  let transform filename =
    let fin = open_in (Filename.concat dir filename) in
    let response =
      try
        let json =
          Jsonrpc_client.timeout_read
            (Unix.descr_of_in_channel fin)
            (Mtime.Span.(5 * s) |> Xapi_stdext_unix.Unixext.Timeout.of_span)
        in
        let rpc = Jsonrpc.of_string ~strict:false json in
        Ok rpc
      with
      | End_of_file ->
          Error End_of_file
      | _ ->
          Error Parse_error
    in
    close_in fin ; response

  let tests =
    `QuickAndAutoDocumented
      [
        (* A file containing exactly one JSON object. *)
        (* It has got curly braces inside strings to make it interesting. *)
        ("good_call.json", Ok good_call)
      ; (* A file containing a partial JSON object. *)
        ("short_call.json", Error Parse_error)
      ; (* A file containing a JSON object, plus some more characters at the
           end. *)
        ("good_call_plus.json", Ok good_call)
      ; (* A file containing some invalid JSON object. *)
        ("bad_call.json", Error Parse_error)
      ]
end)

let tests = [("json_rpc_client_input_json_object", Input_json_object.tests)]
