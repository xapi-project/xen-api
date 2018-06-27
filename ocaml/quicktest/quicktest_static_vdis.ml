
let run_cmd cmd =
  print_endline cmd;
  Alcotest.(check int) cmd 0 (Sys.command cmd)

let static_vdis args =
  let static_vdis = !Xapi_globs.static_vdis in
  let cmd = static_vdis ^ " " ^ (String.concat " " args) in
  run_cmd cmd

let test_static_vdis rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  Qt.VDI.with_new rpc session_id sr (fun self ->
      let uuid = Client.Client.VDI.get_uuid ~rpc ~session_id ~self in
      static_vdis ["add"; uuid; "quicktest_static_vdis"];
      static_vdis ["list"];
      static_vdis ["attach"; uuid];
      static_vdis ["detach"; uuid];
      let vbds = Client.Client.VDI.get_VBDs ~rpc ~session_id ~self in
      Alcotest.(check int) "There must be no VBDs after static-vdis detach"
        0 (List.length vbds);
      static_vdis ["del"; uuid]
    )

let tests () =
  let open Qt_filter in
  [ [ "test_static_vdis for SMAPIv1", `Slow, test_static_vdis ]
    (* We need to be able to run VDI.generate_config on SMAPIv1 SRs *)
    |> conn |> sr SR.(all |> smapiv1 |> allowed_operations [`vdi_create; `vdi_destroy] |> has_capabilities Sr_capabilities.[vdi_generate_config])
  ; [ "test_static_vdis for SMAPIv3", `Slow, test_static_vdis ]
    |> conn |> sr SR.(all |> smapiv3 |> allowed_operations [`vdi_create; `vdi_destroy])
  ]
  |> List.concat
