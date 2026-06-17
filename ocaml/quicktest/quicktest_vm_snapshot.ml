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

let create_vbd_cd rpc session_id vm vdi n =
  Client.Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:n
    ~bootable:false ~mode:`RO ~_type:`CD ~unpluggable:true ~empty:false
    ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[] ~device:""
    ~currently_attached:true

let with_cd_setup rpc session_id sr vm_template f =
  print_endline (Printf.sprintf "%s: Setting up VM" __FUNCTION__) ;
  let uuid = Client.Client.VM.get_uuid ~rpc ~session_id ~self:vm_template in
  print_endline (Printf.sprintf "Template has uuid: %s%!" uuid) ;
  let@ vm = Qt.VM.with_new rpc session_id ~template:vm_template ~sr in
  print_endline (Printf.sprintf "Installed new VM") ;
  print_endline
    (Printf.sprintf "Using SR: %s"
       (Client.Client.SR.get_name_label ~rpc ~session_id ~self:sr)
    ) ;
  let@ vdi =
    Qt.VDI.with_new rpc session_id ~name_label:"small CD"
      ~name_description:__LOC__
      ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
      sr
  in
  ignore (create_vbd_cd rpc session_id vm vdi "0") ;
  let@ vdi2 =
    Qt.VDI.with_new rpc session_id ~name_label:"small CD 2"
      ~name_description:__LOC__
      ~virtual_size:Int64.(mul (mul 4L 1024L) 1024L)
      sr
  in
  ignore (create_vbd_cd rpc session_id vm vdi2 "1") ;
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

let check_vdis_same expected result =
  Alcotest.(check vdi_ref)
    "The VDIs after a reverting a snapshot must unchanged" expected result

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

let test_revert rpc session_id vm vdi vdi2 ~change =
  let snapshot = take_snapshot rpc session_id vm ~origin:__FUNCTION__ in
  Client.Client.VM.revert ~rpc ~session_id ~snapshot ;

  let vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:vm in
  let vdi_after = get_vdi_with_user_device rpc session_id vbds "0" in
  let vdi_after2 = get_vdi_with_user_device rpc session_id vbds "1" in

  let check =
    if change then
      (* Xapi forces VDI clones, the VDIs' IDs will always change *)
      check_vdis_different
    else
      check_vdis_same
  in
  check vdi vdi_after ; check vdi2 vdi_after2

let test_revert_cds rpc session_id vm vdi vdi2 =
  let snapshot = take_snapshot rpc session_id vm ~origin:__FUNCTION__ in

  let snap_vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:snapshot in
  Alcotest.(check int) "Snapshot must only have 2 VBDs" 2 (List.length snap_vbds) ;

  Client.Client.VM.revert ~rpc ~session_id ~snapshot ;

  let vbds = Client.Client.VM.get_VBDs ~rpc ~session_id ~self:vm in
  let vdi_after = get_vdi_with_user_device rpc session_id vbds "0" in
  let vdi_after2 = get_vdi_with_user_device rpc session_id vbds "1" in

  Alcotest.(check int) "VM must only have 2 VBDs" 2 (List.length vbds) ;
  (* CD VDIs are considered immutable and the clone code ignores them *)
  check_vdis_same vdi vdi_after ;
  check_vdis_same vdi2 vdi_after2

let a_test with_setup tests rpc session_id sr_info vm_template () =
  let sr = sr_info.Qt.sr in
  List.iter (with_setup rpc session_id sr vm_template) tests

let suite name with_setup tests sr_ops =
  let open Qt_filter in
  [(name, `Slow, a_test with_setup tests)]
  |> conn
  |> sr SR.(all |> allowed_operations sr_ops)
  |> vm_template Qt.VM.Template.other

let suite_split_revert name with_setup =
  let open Qt_filter in
  let needed_ops = [`vdi_create] in
  let old_ops = [`vdi_clone] in
  let new_ops = [`vdi_revert] in
  let sr_candidates = SR.(all |> allowed_operations needed_ops) in
  let sr_native = sr_candidates |> SR.allowed_operations new_ops in
  let sr_clonables =
    sr_candidates
    |> SR.unavailable_operations new_ops
    |> SR.allowed_operations old_ops
  in
  let tests (filter_name, sr_filter) tests_f =
    let name = Printf.sprintf "%s (%s)" name filter_name in
    [(name, `Slow, a_test with_setup tests_f)]
    |> conn
    |> sr sr_filter
    |> vm_template Qt.VM.Template.other
  in
  tests ("with VDI.revert", sr_native) [test_revert ~change:false]
  @ tests ("with cloning method", sr_clonables) [test_revert ~change:true]

let tests () =
  List.concat
    [
      suite "VM snapshot" with_setup
        [test_snapshot; test_snapshot_ignore_vdi]
        [`vdi_create]
    ; suite_split_revert "VM revert" with_setup
    ; suite "VM revert with CD" with_cd_setup [test_revert_cds]
        [`vdi_create; `vdi_clone]
    ]
