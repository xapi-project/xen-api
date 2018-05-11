(*
 * Copyright (C) 2017 Citrix Systems Inc.
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


open Client
open Quicktest_common
open Http


let import_raw_vdi ~session_id ~task_id f =
  let req = Http.Request.make ~version:"1.0"
      ~user_agent:"quicktest"
      ~query:[
        "session_id", Ref.string_of session_id;
        "sr_id","fake_sr";
        "task_id", Ref.string_of task_id
      ]
      Http.Put "/import_raw_vdi" in
  http req (fun (_, fd) -> f fd)

let start session_id =
  let task_id = Client.Task.create ~rpc:!rpc ~session_id
      ~label:"quicktest import raw vdi"
      ~description:"" in
  try   
    import_raw_vdi ~session_id ~task_id (fun fd ->()) |> ignore;
    Alcotest.fail "No exception was raised by import_raw_vdi"
  with e -> 
    let a = Client.Task.get_record ~rpc:!rpc ~session_id ~self:task_id in
    Client.Task.destroy ~rpc:!rpc ~session_id ~self:task_id;
    if a.API.task_status <> `failure then
    Alcotest.fail "The status of the original task is incorrect"

let tests session_id =
  [ "import_raw_vdi", `Slow, (fun () -> start session_id) ]

