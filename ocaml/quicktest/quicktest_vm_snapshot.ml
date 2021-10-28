(** Set up snapshot test: create a small VM with a selection of VBDs *)
let with_setup rpc session_id sr vm_template f =
  print_endline "Setting up test VM" ;
  let uuid = Client.Client.VM.get_uuid rpc session_id vm_template in
  print_endline (Printf.sprintf "Template has uuid: %s%!" uuid) ;
  let vdi =
    Client.Client.VDI.create rpc session_id "small" __LOC__ sr 4194304L `user
      false false [] [] [] []
  in
  let vdi2 =
    Client.Client.VDI.create rpc session_id "small2" __LOC__ sr 4194304L `user
      false false [] [] [] []
  in
  Qt.VM.with_new rpc session_id ~template:vm_template (fun vm ->
      print_endline (Printf.sprintf "Installed new VM") ;
      print_endline
        (Printf.sprintf "Using SR: %s"
           (Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr)
        ) ;
      ignore
        (Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi
           ~userdevice:"0" ~bootable:false ~mode:`RW ~_type:`Disk
           ~unpluggable:true ~empty:false ~other_config:[]
           ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~device:""
           ~currently_attached:true
        ) ;
      ignore
        (Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi2
           ~userdevice:"1" ~bootable:false ~mode:`RW ~_type:`Disk
           ~unpluggable:true ~empty:false ~other_config:[]
           ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~device:""
           ~currently_attached:true
        ) ;
      f rpc session_id vm vdi vdi2 ;
      Client.Client.VDI.destroy rpc session_id vdi ;
      Client.Client.VDI.destroy rpc session_id vdi2
  )

let test_snapshot rpc session_id vm vdi vdi2 =
  let snapshot = Client.Client.VM.snapshot rpc session_id vm "Snapshot" [] in
  let vbds = Client.Client.VM.get_VBDs rpc session_id snapshot in
  let snap_vbd =
    match
      List.find_opt
        (fun vbd -> Client.Client.VBD.get_userdevice rpc session_id vbd = "0")
        vbds
    with
    | None ->
        Alcotest.fail "Couldn't find VBD on snapshot"
    | Some vbd ->
        vbd
  in
  let snap_vbd2 =
    match
      List.find_opt
        (fun vbd -> Client.Client.VBD.get_userdevice rpc session_id vbd = "1")
        vbds
    with
    | None ->
        Alcotest.fail "Couldn't find VBD on snapshot"
    | Some vbd ->
        vbd
  in
  let snap_vdi = Client.Client.VBD.get_VDI rpc session_id snap_vbd in
  let snap_vdi2 = Client.Client.VBD.get_VDI rpc session_id snap_vbd2 in
  let orig_vdi = Client.Client.VDI.get_snapshot_of rpc session_id snap_vdi in
  let orig_vdi2 = Client.Client.VDI.get_snapshot_of rpc session_id snap_vdi2 in
  assert (orig_vdi = vdi) ;
  assert (orig_vdi2 = vdi2)

let test_snapshot_ignore_vdi rpc session_id vm vdi vdi2 =
  let snapshot =
    Client.Client.VM.snapshot rpc session_id vm "Snapshot" [vdi2]
  in
  let vbds = Client.Client.VM.get_VBDs rpc session_id snapshot in
  let snap_vbd =
    match
      List.find_opt
        (fun vbd -> Client.Client.VBD.get_userdevice rpc session_id vbd = "0")
        vbds
    with
    | None ->
        Alcotest.fail "Couldn't find VBD on snapshot"
    | Some vbd ->
        vbd
  in
  assert (
    not
      (List.exists
         (fun vbd -> Client.Client.VBD.get_userdevice rpc session_id vbd = "1")
         vbds
      )
  ) ;
  let snap_vdi = Client.Client.VBD.get_VDI rpc session_id snap_vbd in
  let orig_vdi = Client.Client.VDI.get_snapshot_of rpc session_id snap_vdi in
  assert (orig_vdi = vdi)

let test rpc session_id sr_info vm_template () =
  let sr = sr_info.Qt.sr in
  List.iter
    (with_setup rpc session_id sr vm_template)
    [test_snapshot; test_snapshot_ignore_vdi]

let tests () =
  let open Qt_filter in
  [
    [("VM snapshot tests", `Slow, test)]
    |> conn
    |> sr SR.(all |> allowed_operations [`vdi_create])
    |> vm_template Qt.VM.Template.other
  ]
  |> List.concat
