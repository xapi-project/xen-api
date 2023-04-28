open Xapi_blobstore_core
open Lwt.Syntax
module Direct2Lwt(D: Types.KVDirect) :
  Types.KVLwt with type t = D.t  and type config = D.config
= struct
  module IO = struct type 'a t = 'a Lwt.t end
  
  type t = D.t
  type config = D.config
  
  let name = D.name ^ " (direct2lwt)"
  
  let pp_config = D.pp_config
  
  let connect = Lwt.wrap1 D.connect
  let disconnect = Lwt.wrap1 D.disconnect
end

module Make(KV: Types.KVLwt) = struct
  open Logs_lwt
  let test_conn_disconn config _ () = 
    let* () = debug (fun m -> m "Connecting to %a" KV.pp_config config) in    
    let* t = KV.connect config in

    let* () = debug (fun m -> m "Connected. Disconnecting... ") in
    let* () = KV.disconnect t in
    debug (fun m -> m "disconnected")
    
  let max_vm_per_host = 1000
    
  let test_multiple_conn_disconn config _  () =
    let* () = info (fun m -> m "Creating %d connections concurrently" max_vm_per_host) in
    List.init max_vm_per_host Fun.id |> Lwt_list.iter_p @@ fun _ ->
    test_conn_disconn config () ()

  let tests make_test_config =
    let config = make_test_config () in
    let open Alcotest_lwt in
    KV.name, [ test_case "connect/disconnect" `Quick @@ test_conn_disconn config
    ; test_case "multiple connect/disconnect" `Slow @@ test_multiple_conn_disconn config
    ]
end

(** Reporter from https://ocaml.org/p/logs/latest/doc/Logs_lwt/index.html#report_ex.
    Otherwise all log calls would be blocking, which may change timings in unexpected ways during the test.
*)
let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    Fmt.with_buffer ~like b,
    fun () -> let m = Buffer.contents b in Buffer.reset b; m
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  (* use stdout for better integration with alcotest: stderr would be immediately shown, stdout would only be shown if test fails *)
  let dst, dst_flush = buf_fmt ~like:Fmt.stdout in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () = match level with
      | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
      | _ -> Lwt_io.write Lwt_io.stdout (dst_flush ()) (* note stdout instead of stderr *)
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf;
  in
  { Logs.report = report }

let tests =
  let module M = Make(Direct2Lwt(Safe_table.Connection)) in
  [ M.tests Fun.id ]

let () =
  Logs.set_reporter @@ lwt_reporter ();
  Logs.set_level (Some Logs.Debug);
  Lwt_main.run @@ Alcotest_lwt.run Sys.executable_name tests