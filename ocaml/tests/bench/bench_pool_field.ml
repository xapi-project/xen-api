(*
 * Copyright (C) Cloud Software Group
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

open Bechamel

let () =
  Suite_init.harness_init () ;
  Printexc.record_backtrace true ;
  Debug.set_level Syslog.Emerg

let date = "20250102T03:04:05Z"

let json_dict =
  [
    ("fingerprint_sha256", String.make 64 'd')
  ; ("not_before", date)
  ; ("not_after", date)
  ; ("subject", String.make 100 'x')
  ; ("san", String.make 50 'y')
  ]

let json_str =
  Rpc.Dict (List.map (fun (k, v) -> (k, Rpc.rpc_of_string v)) json_dict)
  |> Jsonrpc.to_string

let __context = Test_common.make_test_database ()

let () =
  let host = Test_common.make_host ~__context () in
  let pool = Test_common.make_pool ~__context ~master:host () in
  Db.Pool.set_license_server ~__context ~self:pool
    ~value:[("jsontest", json_str)]

let get_all () : API.pool_t list =
  Db.Pool.get_all_records ~__context |> List.map snd

let all = get_all ()

let serialize () : Rpc.t list = all |> List.map API.rpc_of_pool_t

let serialized = serialize ()

let deserialize () : API.pool_t list = serialized |> List.map API.pool_t_of_rpc

let str_sexpr_json = SExpr.(string_of (String json_str))

let sexpr_of_json_string () = SExpr.(string_of (String json_str))

let str_of_sexpr_json () = SExpr.mkstring str_sexpr_json

let date_of_iso8601 () = Clock.Date.of_iso8601 date

let local_session_hook () =
  Xapi_local_session.local_session_hook ~__context ~session_id:Ref.null

let benchmarks =
  [
    Test.make ~name:"local_session_hook" (Staged.stage local_session_hook)
  ; Test.make ~name:"Date.of_iso8601" (Staged.stage date_of_iso8601)
  ; Test.make ~name:"sexpr_of_json_string" (Staged.stage sexpr_of_json_string)
  ; Test.make ~name:"str_of_sexp_json" (Staged.stage str_of_sexpr_json)
  ; Test.make ~name:"Db.Pool.get_all_records" (Staged.stage get_all)
  ; Test.make ~name:"pool_t -> Rpc.t" (Staged.stage serialize)
  ; Test.make ~name:"Rpc.t -> pool_t" (Staged.stage deserialize)
  ]

let () = Bechamel_simple_cli.cli benchmarks
