
(** If VDI_CREATE and VDI_DELETE are present then make sure VDIs appear and disappear correctly *)
(** VDI_CREATE should make a fresh disk; VDI_DELETE should remove it *)
let vdi_create_destroy rpc session_id sr_info () =
  let sr = sr_info.Qt.sr in
  (* Request the following sizes and demand the disk is at least this big (or our data won't fit!) *)
  let sizes_to_try = Sizes.[ 4L ** mib +* 1L; 5L ** mib +* 1L; 6L ** mib +* 1L; 7L ** mib +* 1L; 8L ** mib +* 1L ] in

  List.iter
    (fun virtual_size ->
       let uuid = Qt.VDI.with_new rpc session_id ~virtual_size sr (fun vdi ->
           let actual_size = Client.Client.VDI.get_virtual_size rpc session_id vdi in
           if actual_size < virtual_size then begin
             print_endline (Printf.sprintf "VDI requested size of %Ld but was given only %Ld" virtual_size actual_size);
             Alcotest.fail "VDI.create created too small a VDI"
           end;
           Client.Client.VDI.get_uuid rpc session_id vdi
         )
       in
       (* check that the new disk has gone already (after only one SR.scan) *)
       try
         let vdi = Client.Client.VDI.get_by_uuid rpc session_id uuid in
         print_endline "VDI still exists: checking to see whether it is marked as managed";
         if Client.Client.VDI.get_managed rpc session_id vdi
         then Alcotest.fail "VDI was not destroyed (or marked as unmanaged) properly after one SR.scan"
       with _ -> ()
    ) sizes_to_try

(** For an SR which may be shared, return one plugged in PBD *)
let choose_active_pbd rpc session_id sr =
  let pbds = Client.Client.SR.get_PBDs rpc session_id sr in
  match List.filter (fun pbd -> Client.Client.PBD.get_currently_attached rpc session_id pbd) pbds with
  | [] -> failwith (Printf.sprintf "SR %s has no attached PBDs" (Client.Client.SR.get_uuid rpc session_id sr))
  | x :: _ -> x

(** If VDI_GENERATE_CONFIG is present then try it out *)
let vdi_generate_config_test rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      let sr = sr_info.Qt.sr in
      let pbd = choose_active_pbd rpc session_id sr in
      let host = Client.Client.PBD.get_host rpc session_id pbd in
      Alcotest.(check unit)
        "VDI_GENERATE_CONFIG should not fail" ()
        ((Client.Client.VDI.generate_config rpc session_id host vdi : string) |> ignore)
    )

(** If VDI_UPDATE is present then try it out *)
let vdi_update_test rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      Alcotest.(check unit) "VDI_UPDATE should not fail" () (Client.Client.VDI.update rpc session_id vdi)
    )

(** If VDI_RESIZE is present then try it out *)
let vdi_resize_test rpc session_id sr_info () =
  Qt.VDI.with_new rpc session_id sr_info.Qt.sr (fun vdi ->
      let current = Client.Client.VDI.get_virtual_size rpc session_id vdi in
      print_endline (Printf.sprintf "current size = %Ld" current);
      (* Make it 1 MiB bigger *)
      let new_size = Int64.add current 1048576L in
      print_endline (Printf.sprintf "requested size = %Ld" new_size);
      Client.Client.VDI.resize rpc session_id vdi new_size;
      let actual_size = Client.Client.VDI.get_virtual_size rpc session_id vdi in
      print_endline (Printf.sprintf "final size = %Ld" actual_size);
      if actual_size < new_size then begin
        Alcotest.fail "The final size should be >= the requested size";
      end
    )

(** Make sure that I can't call VDI.db_forget *)
(** VDI.db_forget should always fail without authorisation *)
let vdi_db_forget rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      try
        Client.Client.VDI.db_forget rpc session_id vdi;
        Alcotest.fail "Call succeeded but it shouldn't have"
      with
      | Api_errors.Server_error(code, _) when code = Api_errors.permission_denied ->
        print_endline "Caught PERMISSION_DENIED"
      | e ->
        Alcotest.fail (Printf.sprintf "Caught wrong error: %s" (Printexc.to_string e))
    )

(** If VDI_INTRODUCE is present then attempt to introduce a VDI with a duplicate location
    and another with a bad UUID to make sure that is reported as an error *)
let vdi_bad_introduce rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      let vdir = Client.Client.VDI.get_record rpc session_id vdi in
      begin
        try
          print_endline (Printf.sprintf "Introducing a VDI with a duplicate UUID (%s)" vdir.API.vDI_uuid);
          let (_: API.ref_VDI) = Client.Client.VDI.introduce ~rpc ~session_id
              ~uuid:vdir.API.vDI_uuid ~name_label:"bad uuid" ~name_description:""
              ~sR:vdir.API.vDI_SR ~_type:vdir.API.vDI_type ~sharable:false ~read_only:false ~other_config:[]
              ~location:(Ref.string_of (Ref.make ())) ~xenstore_data:[] ~sm_config:[]
              ~managed:true ~virtual_size:0L ~physical_utilisation:0L ~metadata_of_pool:Ref.null
              ~is_a_snapshot:false ~snapshot_time:Xapi_stdext_date.Date.never ~snapshot_of:Ref.null
          in
          Alcotest.fail "vdi_bad_introduce: A bad VDI with a duplicate UUID was introduced";
        with Api_errors.Server_error(_, _) as e ->
          Printf.printf "API error caught as expected: %s\n" (Printexc.to_string e);
      end;
      begin
        try
          print_endline (Printf.sprintf "Introducing a VDI with a duplicate location (%s)" vdir.API.vDI_location);
          let (_: API.ref_VDI) = Client.Client.VDI.introduce ~rpc ~session_id
              ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ()))
              ~name_label:"bad location" ~name_description:""
              ~sR:vdir.API.vDI_SR ~_type:vdir.API.vDI_type ~sharable:false ~read_only:false ~other_config:[]
              ~location:vdir.API.vDI_location ~xenstore_data:[] ~sm_config:[]
              ~managed:true ~virtual_size:0L ~physical_utilisation:0L ~metadata_of_pool:Ref.null
              ~is_a_snapshot:false ~snapshot_time:Xapi_stdext_date.Date.never ~snapshot_of:Ref.null
          in
          Alcotest.fail "vdi_bad_introduce: A bad VDI with a duplicate location was introduced";
        with Api_errors.Server_error(_, _) as e ->
          Printf.printf "API error caught as expected: %s\n" (Printexc.to_string e);
      end
    )

(** When cloning/snapshotting perform field by field comparisons to look for
    problems *)
let check_clone_snapshot_fields rpc session_id original_vdi new_vdi =
  (** Clones and snapshots should have some identical fields and some different
      fields: *)
  let clone_snapshot_fields =
    [ `Same, "virtual_size", (fun vdi -> vdi.API.vDI_virtual_size |> Int64.to_string)
    ; `Different, "location", (fun vdi -> vdi.API.vDI_location)
    ]
  in

  let a = Client.Client.VDI.get_record ~rpc ~session_id ~self:original_vdi in
  let b = Client.Client.VDI.get_record ~rpc ~session_id ~self:new_vdi in
  Qt.VDI.check_fields clone_snapshot_fields a b

let check_vdi_snapshot rpc session_id vdi =
  let snapshot_start = Qt.Time.now () in
  let snapshot_vdi = Client.Client.VDI.snapshot ~rpc ~session_id ~vdi ~driver_params:[] in
  Qt.VDI.with_destroyed rpc session_id snapshot_vdi (fun () ->
      let snapshot_finish = Qt.Time.now () in
      let r = Client.Client.VDI.get_record ~rpc ~session_id ~self:snapshot_vdi in
      Qt.Time.(check (of_field r.API.vDI_snapshot_time)) ~after:snapshot_start ~before:snapshot_finish;
      Alcotest.(check bool) "VDI.is_a_snapshot of must be true for snapshot VDI"
        true r.API.vDI_is_a_snapshot;
      Alcotest.(check bool) "VDI.snapshot_of must not be null for snapshot VDI"
        true (r.API.vDI_snapshot_of <> API.Ref.null);
      check_clone_snapshot_fields rpc session_id vdi snapshot_vdi;
      Qt.VDI.test_update rpc session_id snapshot_vdi
    )

let test_vdi_snapshot rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info
    (check_vdi_snapshot rpc session_id)

let test_vdi_clone rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      let vdi' = Client.Client.VDI.clone ~rpc ~session_id ~vdi ~driver_params:[] in
      Qt.VDI.with_destroyed rpc session_id vdi' (fun () ->
          check_clone_snapshot_fields rpc session_id vdi vdi')
    )

(* Helper function to make a VBD *)
let vbd_create_helper ~rpc ~session_id ~vM ~vDI ?(userdevice="autodetect") () : API.ref_VBD =
  Client.Client.VBD.create ~rpc ~session_id ~vM ~vDI ~userdevice ~bootable:false ~mode:`RW
    ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[]
    ~qos_algorithm_type:"" ~qos_algorithm_params:[]

(** Check that snapshot works regardless which host has the VDI activated *)
let vdi_snapshot_in_pool rpc session_id sr_info () =
  Qt.VDI.with_any rpc session_id sr_info (fun vdi ->
      let hosts = Client.Client.Host.get_all rpc session_id in
      let do_test () =
        check_vdi_snapshot rpc session_id vdi
      in
      let test_snapshot_on host =
        let name = Client.Client.Host.get_name_label rpc session_id host in
        let dom0 = Qt.VM.dom0_of_host rpc session_id host in
        let vbd = vbd_create_helper ~rpc ~session_id ~vM:dom0 ~vDI:vdi () in

        print_endline (Printf.sprintf "Plugging in to host %s" name);
        Client.Client.VBD.plug rpc session_id vbd;
        Xapi_stdext_pervasives.Pervasiveext.finally do_test
          (fun () ->
             print_endline (Printf.sprintf "Unplugging from host %s" name);
             Client.Client.VBD.unplug rpc session_id vbd;
             print_endline "Destroying VBD";
             Client.Client.VBD.destroy rpc session_id vbd
          )
      in
      List.iter test_snapshot_on hosts;

      do_test ()
    )

(** Make sure that VDI_CREATE; plug; VDI_DESTROY; VDI_CREATE; plug results in a device of
    the correct size in dom0 *)
(** Checking the disk size is correct when a disk is plugged in *)
let vdi_create_destroy_plug_checksize rpc session_id sr_info () =
  let exception Not_this_host in
  (* Query /sys to find the actual size of the plugged in device *)
  let size_of_dom0_vbd rpc session_id vbd =
    let device = Client.Client.VBD.get_device rpc session_id vbd in
    let path = Printf.sprintf "/sys/block/%s/size" device in
    try
      let ic = open_in path in
      let result =
        Xapi_stdext_pervasives.Pervasiveext.finally
          (fun () -> input_line ic) (fun () -> close_in ic)
      in
      Int64.mul (Int64.of_string (Astring.String.trim result)) 512L (* sectors of 512 bytes *)
    with (Sys_error _) ->
      (* Assume this means that the device didn't exist on this host -- must be pooling *)
      raise Not_this_host
  in

  let sr = sr_info.Qt.sr in
  let pbd = choose_active_pbd rpc session_id sr in
  let host = Client.Client.PBD.get_host rpc session_id pbd in
  print_endline (Printf.sprintf "Will plug into host %s" (Client.Client.Host.get_name_label rpc session_id host));

  let plug_in_check_size rpc session_id host vdi =
    let size_should_be = Client.Client.VDI.get_virtual_size rpc session_id vdi in
    let dom0 = Qt.VM.dom0_of_host rpc session_id host in
    let vbd = vbd_create_helper ~rpc ~session_id ~vM:dom0 ~vDI:vdi () in
    Client.Client.VBD.plug rpc session_id vbd;
    Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () ->
         try
           let size_dom0 = size_of_dom0_vbd rpc session_id vbd in
           print_endline (Printf.sprintf "XenAPI reports size: %Ld; dom0 reports size: %Ld" size_should_be size_dom0);
           if size_should_be <> size_dom0 then begin
             Alcotest.fail (Printf.sprintf "Size should have been: %Ld" size_should_be)
           end
         with Not_this_host ->
           print_endline "Skipping size check: disk is plugged into another host"
      )
      (fun () ->
         Client.Client.VBD.unplug rpc session_id vbd;
         Client.Client.VBD.destroy rpc session_id vbd
      ) in

  let small_size = Sizes.(4L ** mib)
  and large_size = Sizes.(1L ** gib) in
  (* Make sure we zap any attached volume state *)
  print_endline "Unplugging PBD";
  Client.Client.PBD.unplug rpc session_id pbd;
  print_endline "Plugging PBD";
  Client.Client.PBD.plug rpc session_id pbd;

  print_endline (Printf.sprintf "Creating VDI with requested size: %Ld" small_size);
  Qt.VDI.with_new rpc session_id ~virtual_size:small_size sr (fun small_vdi ->
      print_endline (Printf.sprintf "Creating VDI with requested size: %Ld" large_size);
      Qt.VDI.with_new rpc session_id ~virtual_size:large_size sr (fun large_vdi ->
          plug_in_check_size rpc session_id host small_vdi |> ignore;
          plug_in_check_size rpc session_id host large_vdi |> ignore
        )
    )

(** Make a VDI, find a host to put it on, create a VBD to dom0 on that host,
    Attach, Unattach, destroy VBD, destroy VDI *)
let vdi_general_test rpc session_id sr_info () =
  print_endline "VDI.create/copy/destroy test";
  let sr = sr_info.Qt.sr in
  let t = Unix.gettimeofday () in
  Qt.VDI.with_new rpc session_id sr (fun newvdi ->
      let createtime = Unix.gettimeofday () -. t in
      print_endline (Printf.sprintf "Time to create: %f%!" createtime);
      let pbd = List.hd (Client.Client.SR.get_PBDs rpc session_id sr) in
      let host = Client.Client.PBD.get_host rpc session_id pbd in
      let dom0 = Qt.VM.dom0_of_host rpc session_id host in
      let device = List.hd (Client.Client.VM.get_allowed_VBD_devices rpc session_id dom0) in
      print_endline (Printf.sprintf "Creating a VBD connecting the VDI to localhost%!");
      let vbd =
        Client.Client.VBD.create ~rpc ~session_id
          ~vM:dom0 ~vDI:newvdi ~userdevice:device ~bootable:false
          ~mode:`RW ~_type:`Disk ~unpluggable:true ~empty:false
          ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]
      in
      Xapi_stdext_pervasives.Pervasiveext.finally
        (fun () ->
           let t = Unix.gettimeofday () in
           print_endline (Printf.sprintf "Attempting to copy the VDI%!");
           let newvdi2 = Client.Client.VDI.copy rpc session_id newvdi sr Ref.null Ref.null in
           Qt.VDI.with_destroyed rpc session_id newvdi2 (fun () ->
               let copytime = Unix.gettimeofday () -. t in
               print_endline (Printf.sprintf "Time to copy: %f%!" copytime);
               print_endline (Printf.sprintf "Destroying copied VDI%!")
             )
        )
        (fun () -> Client.Client.VBD.destroy rpc session_id vbd)
    )

let multiple_dom0_attach rpc session_id sr_info () =
  let rec loop vdi = function
    | 0 -> ()
    | n -> Qt.VDI.with_attached rpc session_id vdi `RO (fun _ -> loop vdi (n - 1))
  in
  Qt.VDI.with_any rpc session_id sr_info (fun vdi -> loop vdi 20)

let tests () =
  let open Qt_filter in
  [ ["vdi_create_destroy", `Slow, vdi_create_destroy] |> conn |> sr SR.(all |> has_capabilities Sr_capabilities.[vdi_create; vdi_delete] |> not_iso)
  ; ["vdi_generate_config_test", `Slow, vdi_generate_config_test] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_generate_config])
  ; ["vdi_update_test", `Slow, vdi_update_test] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_update] |> with_any_vdi)
  ; ["vdi_resize_test", `Slow, vdi_resize_test] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_resize])
  ; ["vdi_db_forget", `Slow, vdi_db_forget] |> conn |> sr SR.(all |> with_any_vdi)
  ; ["vdi_bad_introduce", `Slow, vdi_bad_introduce] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_introduce] |> with_any_vdi)
  ; ["test_vdi_snapshot", `Slow, test_vdi_snapshot] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_snapshot])
  ; ["test_vdi_clone", `Slow, test_vdi_clone] |> conn |> sr SR.(all |> allowed_operations [`scan] |> has_capabilities [Sr_capabilities.vdi_clone])
  ; ["vdi_snapshot_in_pool", `Slow, vdi_snapshot_in_pool] |> conn |> sr SR.(all |> has_capabilities [Sr_capabilities.vdi_snapshot])
  ; ["vdi_create_destroy_plug_checksize", `Slow, vdi_create_destroy_plug_checksize] |> conn |> sr SR.(all |> has_capabilities Sr_capabilities.[vdi_create; vdi_delete; vdi_attach] |> can_unplug)
  ; ["vdi_general_test", `Slow, vdi_general_test] |> conn |> sr SR.(all |> allowed_operations [`vdi_create; `vdi_destroy])
  ; ["multiple_dom0_attach", `Slow, multiple_dom0_attach] |> conn |> sr SR.(all |> with_any_vdi |> not_type "udev") (* Can't attach empty optical drive *)
  ]
  |> List.concat
