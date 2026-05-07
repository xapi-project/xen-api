open Quicktest_api_helpers
open Client.Client
open Quicktest_trace
open Quicktest_trace_api
open Quicktest_trace_rpc

let all_possible_tests = [16; 1; 3; 8]

let check_tasks tasks =
  tasks |> List.map @@ function Ok x -> x | Error exn -> raise exn

let one t ~host ~vm ~workload_vm n =
  Trace.with_ __FUNCTION__ @@ fun scope ->
  workload t ~host ~workload_vm ;
  let vms = fill_mem_n t ~workaround_migration:true ~host ~vm ~n in

  let migration_host, migration_vm = List.nth vms 0 in

  if n > 1 then begin
    (* shutdown one to make room *)
      let ((_, vm_for_shutdown) as host_vm_shutdown) =
        List.nth (vms |> List.tl) 0
      in
      shutdown_vms t [vm_for_shutdown] ;

      Api.VM.with_call t "unpause" migration_vm
      @@ Client.Client.VM.unpause ~vm:migration_vm ;

      let () =
        Trace.with_ ~scope "localhost migrate1" @@ fun _ ->
        let task t =
          Api.VM.task t "localhost migrate" ignore migration_vm
          @@ Async.VM.pool_migrate ~vm:migration_vm
               ~options:[("force", "true")]
               ~host:migration_host
        in
        let (_ : _ list) =
          Api.batched_run_or_cancel t "localhost migrate" [task]
        in
        ()
      in

      (* start it up again *)
      start_vms t [host_vm_shutdown] ;

      Api.VM.with_call t "pause" migration_vm
      @@ Client.Client.VM.pause ~vm:migration_vm
  end ;

  let () =
    match call t @@ Client.Client.Host.get_all |> List.filter (( <> ) host) with
    | [] ->
        () (* not in a pool: SKIP *)
    | other :: _ ->
        let tasks =
          List.map
            (fun (_, vm) t ->
              Api.VM.task t "unpause" ignore vm @@ Async.VM.unpause ~vm
            )
            vms
        in
        let (_ : unit list) =
          Api.batched_run_or_cancel t "unpause" tasks |> check_tasks
        in
        let tasks =
          List.map
            (fun (_, vm) t ->
              Api.VM.task t "pool migrate" ignore vm
              @@ Client.Client.Async.VM.pool_migrate ~host:other ~vm
                   ~options:[("force", "true")]
            )
            vms
        in
        let (_ : unit list) =
          Api.batched_run_or_cancel t "pool migrate" tasks |> check_tasks
        in

        let tasks =
          List.map
            (fun (host, vm) t ->
              Api.VM.task t "pool migrate back" ignore vm
              @@ Client.Client.Async.VM.pool_migrate ~host ~vm
                   ~options:[("force", "true")]
            )
            vms
        in
        let (_ : unit list) =
          Api.batched_run_or_cancel t "pool migrate back" tasks |> check_tasks
        in
        ()
  in

  hard_reboot_vms t [(migration_host, migration_vm)] ;

  hard_reboot_vms t vms ;

  shutdown_vms t (List.map snd vms)

let test rpc session_id template iso () =
  let t = {rpc= RPC.wrap ~log_body:true rpc; session_id} in
  let host = call t @@ Host.get_by_uuid ~uuid:Qt.localhost_uuid in
  Qt.VM.with_new rpc session_id ~template ~iso @@ fun workload_vm ->
  Qt.VM.with_new rpc session_id ~template ~iso @@ fun vm ->
  List.iter (one t ~host ~vm ~workload_vm) all_possible_tests

let tests () =
  let open Qt_filter in
  [
    [("VM memory tests", `Slow, test)]
    |> conn
    |> vm_template Qt.VM.Template.other
    |> memtest_iso ?prefix:None
  ]
  |> List.concat
