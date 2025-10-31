open Xapi_guard_server
module SessionCache = Xen_api_client_lwt.Xen_api_lwt_unix.SessionCache
open Alcotest_lwt
open Lwt.Syntax
open Xen_api_client_lwt.Xen_api_lwt_unix

module D = Debug.Make (struct let name = "xapi-guard-test" end)

let expected_session_id : [`session] Ref.t = Ref.make_secret ()

let vm : [`VM] Ref.t = Ref.make ()

let vm_bad : [`VM] Ref.t = Ref.make ()

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

let vm_uuid = Uuidm.v4_gen (Random.State.make_self_init ()) ()

let vm_uuid_str = Uuidm.to_string vm_uuid

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook :=
    fun exn ->
      D.log_backtrace exn ;
      D.error "Lwt caught async exception: %s" (Printexc.to_string exn) ;
      old_hook exn

let with_rpc f switch () =
  Lwt_io.with_temp_dir ~prefix:"xapi_guard" @@ fun tmp ->
  let cache =
    SessionCache.create_rpc xapi_rpc ~uname:"root" ~pwd:""
      ~version:Xapi_version.version ~originator:"test" ()
  in
  (Lwt_switch.add_hook (Some switch) @@ fun () -> SessionCache.destroy cache) ;
  let path = Filename.concat tmp "socket" in
  let push_nothing _ = Lwt_result.return () in
  (* Create an internal server on 'path', the socket that varstored would connect to *)
  let* stop_server =
    Server_interface.make_server_varstored push_nothing ~cache path vm_uuid
  in
  (* rpc simulates what varstored would do *)
  let uri = Uri.make ~scheme:"file" ~path () in
  D.debug "Connecting to %s" (Uri.to_string uri) ;
  let rpc = make uri in
  Lwt.finalize
    (fun () ->
      (* not strictly necessary to login/logout here - since we only get dummy sessions *)
      let* session_id =
        Session.login_with_password ~rpc ~uname:"root" ~pwd:""
          ~version:Xapi_version.version ~originator:"test"
      in
      let logout () = Session.logout ~rpc ~session_id in
      Lwt.finalize logout @@ f ~rpc ~session_id
    )
    stop_server

let dict = Alcotest.(list @@ pair string string)

let test_change_nvram ~rpc ~session_id () =
  let* self = VM.get_by_uuid ~rpc ~session_id ~uuid:vm_uuid_str in
  let* nvram0 = VM.get_NVRAM ~rpc ~session_id ~self in
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
  ( "UEFI"
  , [test_case "NVRAM change contents" `Quick @@ with_rpc test_change_nvram]
  )

(* xapi-guard filters API calls, and ignores VM / session refs, and replaces it
   with the ref the daemon is supposed to use. It doesn't reject bad refs,
   although it could in the future, and then the tests below should be updated
*)

let test_bad_get_nvram ~rpc ~session_id () =
  let* _ = VM.get_NVRAM ~rpc ~session_id ~self:vm_bad in
  Lwt.return_unit

let test_bad_set_nvram ~rpc ~session_id () =
  let* () =
    VM.set_NVRAM_EFI_variables ~rpc ~session_id ~self:vm_bad ~value:"bad"
  in
  let* vm_ref = VM.get_by_uuid ~rpc ~session_id ~uuid:vm_uuid_str in
  let* nvram = VM.get_NVRAM ~rpc ~session_id ~self:vm_ref in
  Alcotest.(check' dict)
    ~msg:"only managed to change own nvram" ~actual:nvram
    ~expected:[("EFI-variables", "bad")] ;
  Lwt.return_unit

let bad_params_tests =
  ( "Bad params"
  , [
      test_case "VM.get_NVRAM" `Quick @@ with_rpc test_bad_get_nvram
    ; test_case "VM.set_NVRAM_EFI_variables" `Quick
      @@ with_rpc test_bad_set_nvram
    ]
  )

let linux_count_fds () =
  if Sys.file_exists "/proc/self/fd" then
    Sys.readdir "/proc/self/fd" |> Array.length
  else
    0

let test_shutdown _ () =
  let _fd0 = linux_count_fds () in
  let noop ~rpc:_ ~session_id:_ () = Lwt.return_unit in
  let* () = with_rpc noop Server_interface.shutdown () in
  let* () = Lwt_switch.turn_off Server_interface.shutdown in
  let _fd1 = linux_count_fds () in
  (* Sometimes fd1 is lower than fd0, feel free to find the root cause and
     uncomment the test! *)
  (* Alcotest.(check' int) ~msg:"No FD leak" ~expected:_fd0 ~actual:_fd1 ; *)
  Lwt.return_unit

let shutdown_tests = ("Shutdown", [test_case "shutdown" `Quick test_shutdown])

let () =
  Debug.log_to_stdout () ;
  Lwt_main.run
  @@ Alcotest_lwt.run "xapi_guard_test"
       [uefi_tests; bad_params_tests; shutdown_tests]
