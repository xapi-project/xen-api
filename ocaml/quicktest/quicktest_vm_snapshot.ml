(* Copyright (C) 2026 Vates.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)
let ( let@ ) f x = f x

let create_vbd_disk rpc session_id vm vdi n =
  Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:n
    ~bootable:false ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false
    ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~device:""
    ~currently_attached:true

(** Set up snapshot test: create a small VM with a selection of VBDs *)
let with_setup rpc session_id sr vm_template f =
  print_endline "Setting up test VM" ;
  let uuid = Client.Client.VM.get_uuid ~rpc ~session_id ~self:vm_template in
  print_endline (Printf.sprintf "Template has uuid: %s%!" uuid) ;
  let@ vm = Qt.VM.with_new rpc session_id ~template:vm_template ~sr in
  print_endline (Printf.sprintf "Installed new VM") ;
  print_endline
    (Printf.sprintf "Using SR: %s"
       (Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr)
    ) ;
  let@ vdi =
    Qt.VDI.with_new rpc session_id ~name_label:"small" ~name_description:__LOC__
      ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
      sr
  in
  ignore (create_vbd_disk rpc session_id vm vdi "0") ;
  let@ vdi2 =
    Qt.VDI.with_new rpc session_id ~name_label:"small2"
      ~name_description:__LOC__
      ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
      sr
  in
  ignore (create_vbd_disk rpc session_id vm vdi2 "1") ;
  f rpc session_id vm vdi vdi2

let take_snapshot ?(ignore_vdis = []) rpc session_id vm ~origin =
  Client.Client.VM.snapshot ~rpc ~session_id ~vm
    ~new_name:(Printf.sprintf "Snapshot:%s" origin)
    ~ignore_vdis

let is_user_device rpc session_id n vbd =
  Client.Client.VBD.get_userdevice ~rpc ~session_id ~self:vbd = n

let get_vdi_with_user_device rpc session_id vbds n =
  let vbd =
    match List.find_opt (is_user_device rpc session_id n) vbds with
    | None ->
        Alcotest.fail (Printf.sprintf "Couldn't find VBD on snapshot %s" n)
    | Some vbd ->
        vbd
  in
  Client.Client.VBD.get_VDI ~rpc ~session_id ~self:vbd

let get_snapshot_of_vdi rpc session_id vbds n =
  let snap = get_vdi_with_user_device rpc session_id vbds n in
  Client.Client.VDI.get_snapshot_of ~rpc ~session_id ~self:snap

let vm_ref : [`VM] Ref.t Alcotest.testable = Alcotest.testable Ref.pp ( = )

let vdi_ref : [`VDI] Ref.t Alcotest.testable = Alcotest.testable Ref.pp ( = )

let check_vm_snapshot_of rpc session_id ~snapshot ~vm =
  let snapshot_of =
    Client.Client.VM.get_snapshot_of ~rpc ~session_id ~self:snapshot
  in
  Alcotest.(check vm_ref)
    "The expected VM is different from the one in snapshot_of" vm snapshot_of

let check_vdi_snapshot_of rpc session vbds ~vdi n =
  let snapshot_of = get_snapshot_of_vdi rpc session vbds n in
  Alcotest.(check vdi_ref)
    "The expected vdi is different from the one in snapshot_of" vdi snapshot_of

let check_vdis_different expected result =
  Alcotest.(check @@ neg vdi_ref)
    "The VDIs after a reverting a snapshot must not be the same" expected result

let test_snapshot rpc session_id vm vdi vdi2 =
  let snapshot = take_snapshot rpc session_id vm ~origin:__FUNCTION__ in
  let vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:snapshot in

  check_vm_snapshot_of rpc session_id ~snapshot ~vm ;
  check_vdi_snapshot_of rpc session_id vbds ~vdi "0" ;
  check_vdi_snapshot_of rpc session_id vbds ~vdi:vdi2 "1"

let test_snapshot_ignore_vdi rpc session_id vm vdi vdi2 =
  let snapshot =
    take_snapshot rpc session_id vm ~origin:__FUNCTION__ ~ignore_vdis:[vdi2]
  in
  let vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:snapshot in
  let has_been_snapshot n =
    List.exists (is_user_device rpc session_id n) vbds
  in
  Alcotest.(check bool)
    "The vbd with user_device 1 cannot be present in the snapshot" false
    (has_been_snapshot "1") ;
  check_vdi_snapshot_of rpc session_id vbds ~vdi "0"

let test_revert rpc session_id vm vdi vdi2 =
  let snapshot = take_snapshot rpc session_id vm ~origin:__FUNCTION__ in
  Client.Client.VM.revert ~rpc ~session_id ~snapshot ;

  let vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:vm in
  let vdi_after = get_vdi_with_user_device rpc session_id vbds "0" in
  let vdi_after2 = get_vdi_with_user_device rpc session_id vbds "1" in

  (* Xapi forces VDI clones, the VDIs' IDs will always change *)
  check_vdis_different vdi vdi_after ;
  check_vdis_different vdi2 vdi_after2

let a_test with_setup tests rpc session_id sr_info vm_template () =
  let sr = sr_info.Qt.sr in
  List.iter (with_setup rpc session_id sr vm_template) tests

let suite name with_setup tests sr_ops =
  let open Qt_filter in
  [(name, `Slow, a_test with_setup tests)]
  |> conn
  |> sr SR.(all |> allowed_operations sr_ops)
  |> vm_template Qt.VM.Template.other

let tests () =
  List.concat
    [
      suite "VM snapshot tests" with_setup
        [test_snapshot; test_snapshot_ignore_vdi]
        [`vdi_create]
    ; suite "VM revert tests" with_setup [test_revert] [`vdi_create; `vdi_clone]
    ]
