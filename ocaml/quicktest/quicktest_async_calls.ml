
let rpc = Quicktest_common.rpc

let rec wait_for_task_complete session_id task =
  Thread.delay 1.;
  match Client.Client.Task.get_status !rpc session_id task with
  | `pending | `cancelling -> wait_for_task_complete session_id task
  | _ -> ()

let extract_ref xml =
  xml
  |> Astring.String.cuts ~empty:false ~sep:"<value>" |> List.hd
  |> Astring.String.cuts ~empty:false ~sep:"</value>" |> List.hd
  |> API.Ref.of_string

(* Test a couple of async calls - VDIs are good for this, again! *)
let async_test session_id sr_info =
  let sr = sr_info.Storage_test.sr in
  print_endline "Async.VDI.copy test";
  Storage_test.VDI.with_new session_id sr (fun newvdi ->
      let task = Client.Client.Async.VDI.copy !rpc session_id newvdi sr Ref.null Ref.null in
      wait_for_task_complete session_id task;
      print_endline (Printf.sprintf "Task completed!%!");
      let status = Client.Client.Task.get_status !rpc session_id task in
      let result = Client.Client.Task.get_result !rpc session_id task in
      print_endline (Printf.sprintf "Status: %s  result: %s%!"
                       (match status with
                        | `pending -> "pending"
                        | `success -> "success"
                        | `failure -> "failure"
                        | `cancelling -> "cancelling"
                        | `cancelled -> "cancelled")
                       result);
      match status with
      | `failure -> Alcotest.fail (Printf.sprintf "Failure of VDI copy! error_info: %s" (String.concat "," (Client.Client.Task.get_error_info !rpc session_id task)))
      | `success ->
        let self = result |> extract_ref in
        Client.Client.VDI.destroy ~rpc:!rpc ~session_id ~self
      | `cancelled -> ()
      | `cancelling | `pending -> Alcotest.fail "Task should be finished!"
    )

let tests session_id =
  let module F = Storage_test.Sr_filter in
  [ "async", `Slow, async_test, F.(not_iso ||> allowed_operations [`vdi_create] ||> random)
  ]
  |> Storage_test.get_test_cases session_id
