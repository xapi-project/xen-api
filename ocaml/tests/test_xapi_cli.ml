(*
 * Copyright (C) 2026 Vates.
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

(* What the xe client receives when the CLI server reports an exception *)
let reported e =
  let server, client = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close server ; Unix.close client)
    (fun () ->
      Xapi_cli.exception_handler server e ;
      Cli_protocol.unmarshal client
    )

let check_reported name e expected () =
  match reported e with
  | Cli_protocol.Command (Cli_protocol.PrintStderr msg) ->
      Alcotest.(check string) name expected msg
  | m ->
      Alcotest.failf "%s: unexpected message %s" name
        (Cli_protocol.string_of_message m)

let tests =
  [
    ( "Xapi_cli"
    , [
        ( "parameter defined multiple times"
        , `Quick
        , check_reported "multiple definitions"
            (Cli_args.Multiple_definitions "k")
            "Error: Parameter k is defined multiple times, define it only once.\n"
        )
      ; ( "record failure"
        , `Quick
        , check_reported "record failure"
            (Record_util.Record_failure "bad value") "Error: bad value\n"
        )
      ]
    )
  ]
