
let rec wait_for_task_complete rpc session_id task =
  Thread.delay 1.;
  match Client.Client.Task.get_status rpc session_id task with
  | `pending | `cancelling -> wait_for_task_complete rpc session_id task
  | _ -> ()

let extract_ref xml =
  xml
  |> Astring.String.cuts ~empty:false ~sep:"<value>" |> List.hd
  |> Astring.String.cuts ~empty:false ~sep:"</value>" |> List.hd
  |> API.Ref.of_string

(* Test a couple of async calls - VDIs are good for this, again! *)
let async_test rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  print_endline "Async.VDI.copy test";
  Qt.VDI.with_new rpc session_id sr (fun newvdi ->
      let task = Client.Client.Async.VDI.copy rpc session_id newvdi sr Ref.null Ref.null in
      wait_for_task_complete rpc session_id task;
      Printf.printf "Task completed!\n%!";
      let status = Client.Client.Task.get_status rpc session_id task in
      let result = Client.Client.Task.get_result rpc session_id task in
      print_endline (Printf.sprintf "Status: %s  result: %s%!"
                       (match status with
                        | `pending -> "pending"
                        | `success -> "success"
                        | `failure -> "failure"
                        | `cancelling -> "cancelling"
                        | `cancelled -> "cancelled")
                       result);
      match status with
      | `failure -> Alcotest.failf "Failure of VDI copy! error_info: %s" (String.concat "," (Client.Client.Task.get_error_info rpc session_id task))
      | `success ->
        let self = result |> extract_ref in
        Client.Client.VDI.destroy ~rpc:rpc ~session_id ~self
      | `cancelled -> ()
      | `cancelling | `pending -> Alcotest.fail "Task should be finished!"
    )

let tests () =
  let open Qt_filter in
  [ ["async", `Slow, async_test] |> conn |> sr SR.(all |> not_iso |> allowed_operations [`vdi_create] |> random)
  ]
  |> List.concat
