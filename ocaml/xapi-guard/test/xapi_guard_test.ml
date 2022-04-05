open Xapi_guard
open Varstored_interface
open Alcotest_lwt
open Lwt.Syntax
open Xen_api_lwt_unix

module D = Debug.Make (struct let name = "xapi-guard-test" end)

let expected_session_id : [`session] Ref.t = Ref.make ()

let vm : [`VM] Ref.t = Ref.make ()

let nvram_contents = ref []

(* simulates what xapi would do *)
let xapi_rpc call =
  D.debug "Got rpc %s" call.Rpc.name ;
  let ret_ok contents =
    Lwt.return
      Rpc.{success= true; contents= Rpc.String contents; is_notification= false}
  in
  let cmp_ref a b = String.equal (Ref.string_of a) (Ref.string_of b) in
  let expect_session_id session_id_rpc =
    let actual = API.ref_session_of_rpc session_id_rpc in
    let ref = Alcotest.testable (Fmt.of_to_string Ref.string_of) cmp_ref in
    Alcotest.(check' ref)
      ~expected:expected_session_id ~actual ~msg:"session id"
  in
  let expect_vm vm_rpc =
    let ref = Alcotest.testable (Fmt.of_to_string Ref.string_of) cmp_ref in
    let actual = API.ref_VM_of_rpc vm_rpc in
    Alcotest.(check' ref) ~expected:vm ~actual ~msg:"vm ref"
  in
  match (call.Rpc.name, call.Rpc.params) with
  | "session.login_with_password", _ ->
      ret_ok (Ref.string_of expected_session_id)
  | "session.logout", [session_id_rpc] ->
      expect_session_id session_id_rpc ;
      ret_ok ""
  | "VM.get_by_uuid", [session_id_rpc; _uuid] ->
      expect_session_id session_id_rpc ;
      ret_ok (Ref.string_of vm)
  | "VM.get_NVRAM", [session_id_rpc; vm_rpc] ->
      expect_session_id session_id_rpc ;
      expect_vm vm_rpc ;
      Lwt.return
        Rpc.
          {
            success= true
          ; contents= API.rpc_of_string_to_string_map !nvram_contents
          ; is_notification= false
          }
        
  | "VM.set_NVRAM_EFI_variables", [session_id_rpc; vm_rpc; contents] ->
      expect_session_id session_id_rpc ;
      expect_vm vm_rpc ;
      nvram_contents := [("EFI-variables", API.string_of_rpc contents)] ;
      ret_ok ""
  | _ ->
      Fmt.failwith "XAPI RPC call %s not expected in test" call.Rpc.name

let vm_uuid = Uuidx.(to_string (make ()))

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook :=
    fun exn ->
      D.log_backtrace () ;
      D.error "Lwt caught async exception: %s" (Printexc.to_string exn) ;
      old_hook exn

let with_rpc f switch () =
  Lwt_io.with_temp_dir ~prefix:"xapi_guard" @@ fun tmp ->
  let cache = SessionCache.create ~rpc:xapi_rpc ~login ~logout in
  (Lwt_switch.add_hook (Some switch) @@ fun () -> SessionCache.destroy cache) ;
  let path = Filename.concat tmp "socket" in
  (* Create an internal server on 'path', the socket that varstored/swtpm would connect to *)
  let* stop_server = make_server_rpcfn ~cache path vm_uuid in
  (* rpc simulates what varstored/swtpm would do *)
  let uri = Uri.make ~scheme:"file" ~path () |> Uri.to_string in
  D.debug "Connecting to %s" uri ;
  let rpc = Xen_api_lwt_unix.make uri in
  Lwt.finalize
    (fun () ->
      (* not strictly necessary to login/logout here - since we only get dummy sessions *)
      let* session_id =
        Session.login_with_password ~rpc ~uname:"root" ~pwd:"" ~version:"0.0"
          ~originator:"test"
      in
      let logout () = Session.logout ~rpc ~session_id in
      Lwt.finalize logout @@ f ~rpc ~session_id
    )
    stop_server

let test_change_nvram ~rpc ~session_id () =
  let* self = VM.get_by_uuid ~rpc ~session_id ~uuid:vm_uuid in
  let* nvram0 = VM.get_NVRAM ~rpc ~session_id ~self in
  let dict = Alcotest.(list @@ pair string string) in
  Alcotest.(check' dict) ~msg:"nvram initial" ~expected:[] ~actual:nvram0 ;
  let contents = "nvramnew" in
  let* () = VM.set_NVRAM_EFI_variables ~rpc ~session_id ~self ~value:contents in
  let* nvram1 = VM.get_NVRAM ~rpc ~session_id ~self in
  Alcotest.(check' dict)
    ~msg:"nvram changed"
    ~expected:[("EFI-variables", contents)]
    ~actual:nvram1 ;
  Lwt.return_unit

let uefi_tests =
  ("UEFI", [test_case "NVRAM" `Quick @@ with_rpc test_change_nvram])

let () =
  Debug.log_to_stdout () ;
  Lwt_main.run @@ Alcotest_lwt.run "xapi_guard_test" [uefi_tests]
