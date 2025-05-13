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
  Debug.set_level Syslog.Emerg ;
  Xapi_event.register_hooks ()

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

let host = Test_common.make_host ~__context ()

let pool = Test_common.make_pool ~__context ~master:host ()

let () =
  Db.Pool.set_license_server ~__context ~self:pool
    ~value:[("jsontest", json_str)] ;
  let open Xapi_database in
  Db_ref.update_database
    (Context.database_of __context)
    (Db_cache_types.Database.register_callback "redo_log"
       Redo_log.database_callback
    )

let vm = Test_common.make_vm ~__context ~name_label:"test" ()

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

let atomic = Atomic.make 0

let atomic_inc () = Atomic.incr atomic

let mutex = Mutex.create ()

let locked_ref = ref 0

let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

let inc_locked () = incr locked_ref

let inc_with_mutex () = with_lock mutex inc_locked

let noop () = Sys.opaque_identity ()

let db_lock_uncontended () : unit = Xapi_database.Db_lock.with_lock noop

let event =
  let open Event_types in
  {
    id= "id"
  ; ts= "1000"
  ; ty= "test"
  ; op= `add
  ; reference= "test"
  ; snapshot= Some (Rpc.Dict [])
  }

let test_rpc_of_event () = Event_types.rpc_of_event event

let counter = Atomic.make 0

let test_set_vm_nvram () : unit =
  let c = Atomic.fetch_and_add counter 1 mod 0x7F in
  (* use different value each iteration, otherwise it becomes a noop *)
  Db.VM.set_NVRAM ~__context ~self:vm
    ~value:[("test", String.make 32768 (Char.chr @@ c))]

let test_db_pool_write () =
  let c = Atomic.fetch_and_add counter 1 mod 0x7F in
  Db.Pool.set_tags ~__context ~self:pool ~value:[String.make 16 (Char.chr @@ c)]

let test_db_pool_read () = Db.Pool.get_tags ~__context ~self:pool

let benchmarks =
  [
    Test.make ~name:"local_session_hook" (Staged.stage local_session_hook)
  ; Test.make ~name:"Date.of_iso8601" (Staged.stage date_of_iso8601)
  ; Test.make ~name:"sexpr_of_json_string" (Staged.stage sexpr_of_json_string)
  ; Test.make ~name:"str_of_sexp_json" (Staged.stage str_of_sexpr_json)
  ; Test.make ~name:"Db.Pool.get_all_records" (Staged.stage get_all)
  ; Test.make ~name:"pool_t -> Rpc.t" (Staged.stage serialize)
  ; Test.make ~name:"Rpc.t -> pool_t" (Staged.stage deserialize)
  ; Test.make ~name:"Atomic.incr" (Staged.stage atomic_inc)
  ; Test.make ~name:"Mutex+incr" (Staged.stage inc_with_mutex)
  ; Test.make ~name:"Db_lock.with_lock uncontended"
      (Staged.stage db_lock_uncontended)
  ; Test.make ~name:"rpc_of_event" (Staged.stage test_rpc_of_event)
  ; Test.make ~name:"Db.Pool.set_tags" (Staged.stage test_db_pool_write)
  ; Test.make ~name:"Db.Pool.get_tags" (Staged.stage test_db_pool_read)
  ; Test.make ~name:"Db.VM.set_NVRAM" (Staged.stage test_set_vm_nvram)
  ]

let () = Bechamel_simple_cli.cli benchmarks
