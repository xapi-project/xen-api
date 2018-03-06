(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Stdext
open Xstringext
open Threadext
open Pervasiveext
open Client
open Quicktest_common

let vdi_create   = "VDI_CREATE"
let vdi_delete   = "VDI_DELETE"
let vdi_clone    = "VDI_CLONE"
let vdi_snapshot = "VDI_SNAPSHOT"
let vdi_resize   = "VDI_RESIZE"
let vdi_attach   = "VDI_ATTACH"
let vdi_introduce = "VDI_INTRODUCE"
let vdi_update   = "VDI_UPDATE"
let vdi_generate_config = "VDI_GENERATE_CONFIG"
let sr_probe     = "SR_PROBE"
let sr_update    = "SR_UPDATE"

let iso_path = ref "/opt/xensource/packages/iso"

let only_sr_name = ref None

(** Return a list of all SRs which have at least one plugged-in PBD ie those
    which we can use for stuff *)
let list_srs session_id =
  let all = Client.SR.get_all !rpc session_id in
  List.filter (fun sr ->
      let pbds = Client.SR.get_PBDs !rpc session_id sr in
      List.fold_left (||) false
        (List.map (fun pbd -> Client.PBD.get_currently_attached !rpc session_id pbd) pbds)) all
  (* Filter SR with specific type from CLI *)
  |> List.filter (fun sr ->
         match !only_sr_name with
         | None -> true
         | Some t -> Client.SR.get_name_label !rpc session_id sr = t)

let name_of_sr session_id sr =
  let name_label = Client.SR.get_name_label !rpc session_id sr in
  let ty = Client.SR.get_type !rpc session_id sr in
  Printf.sprintf "%s/%s" name_label ty

(* Helper function to make a disk *)
let vdi_create_helper ~session_id ?(name_label="quicktest") ?(virtual_size=4L ** mib) ~sr () : API.ref_VDI  =
  Client.VDI.create ~rpc:!rpc ~session_id ~name_label ~name_description:""
    ~sR:sr ~virtual_size ~_type:`user ~sharable:false ~read_only:false
    ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]

(** Return the size of the smallest disk we can create in each SR. This wouldn't be necessary
    except the Netapp SR breaks with convention and returns errors rather than rounding up
    for small disks *)
let find_smallest_disk_size session_id sr =
  let sizes = [ 0L; 1L; 1L ** mib; 2L ** mib; 4L ** mib ] in
  let try_one size =
    try
      let vdi = vdi_create_helper ~session_id ~virtual_size:size ~sr () in
      Client.VDI.destroy !rpc session_id vdi;
      Some size
    with _ -> None in
  let find_smallest = List.fold_left
      (fun state size -> if state = None then try_one size else state) None sizes in
  find_smallest

(** For an SR which may be shared, return one plugged in PBD *)
let choose_active_pbd session_id sr =
  let pbds = Client.SR.get_PBDs !rpc session_id sr in
  match List.filter (fun pbd -> Client.PBD.get_currently_attached !rpc session_id pbd) pbds with
  | [] -> failwith (Printf.sprintf "SR %s has no attached PBDs" (Client.SR.get_uuid !rpc session_id sr))
  | x :: _ -> x

(** Scan an SR and return the number of VDIs contained within *)
let count_vdis session_id sr =
  Client.SR.scan !rpc session_id sr;
  let vdis = Client.SR.get_VDIs !rpc session_id sr in
  (* NB vhd backends may delete records beneath us *)
  let managed_vdis = List.filter (fun vdi -> try Client.VDI.get_managed !rpc session_id vdi with Api_errors.Server_error(_ (* handle_invalid *), _) -> false) vdis in
  List.length managed_vdis

(** Common code for VDI.{create,clone,snapshot} which checks to see that a new VDI
    is successfully created and destroyed by the backend *)
let vdi_create_clone_snapshot test session_id sr make_fn =
  let before = count_vdis session_id sr in
  let vdi = make_fn () in
  let vdi_r = Client.VDI.get_record !rpc session_id vdi in
  debug test (Printf.sprintf "Created VDI has uuid: %s (size = %Ld)" vdi_r.API.vDI_uuid vdi_r.API.vDI_virtual_size);
  let during = count_vdis session_id sr in
  if during <= before then begin
    debug test (Printf.sprintf "SR has %d VDIs before the test" before);
    debug test (Printf.sprintf "SR has %d VDIs during the test" during);
    failed test (Printf.sprintf "Before VDI was created there were %d VDIs. After there were %d VDIs." before during);
    failwith "vdi_create_clone_snapshot"
  end;
  Client.VDI.destroy !rpc session_id vdi

(* Helper function to make a VBD *)
let vbd_create_helper ~session_id ~vM ~vDI ?(userdevice="autodetect") () : API.ref_VBD =
  Client.VBD.create ~rpc:!rpc ~session_id ~vM ~vDI ~userdevice ~bootable:false ~mode:`RW
    ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[]
    ~qos_algorithm_type:"" ~qos_algorithm_params:[]

(** If VDI_CREATE and VDI_DELETE are present then make sure VDIs appear and disappear correctly *)
let vdi_create_destroy caps session_id sr =
  if true
  && (List.mem vdi_create caps)
  && (List.mem vdi_delete caps)
  then begin
    let test = make_test "VDI_CREATE should make a fresh disk; VDI_DELETE should remove it" 2 in
    start test;
    let new_uuid = ref None in
    (* Request the following sizes and demand the disk is at least this big (or our data won't fit!) *)
    let sizes_to_try = [ 4L ** mib +* 1L; 5L ** mib +* 1L; 6L ** mib +* 1L; 7L ** mib +* 1L; 8L ** mib +* 1L ] in

    List.iter
      (fun virtual_size ->
         vdi_create_clone_snapshot test session_id sr
           (fun () ->
              let vdi = vdi_create_helper ~session_id ~name_label:"quicktest" ~virtual_size ~sr () in
              let actual_size = Client.VDI.get_virtual_size !rpc session_id vdi in
              if actual_size < virtual_size then begin
                debug test (Printf.sprintf "VDI requested size of %Ld but was given only %Ld" virtual_size actual_size);
                failed test "VDI.create created too small a VDI"
              end;
              new_uuid := Some (Client.VDI.get_uuid !rpc session_id vdi);
              vdi);
         (* check that the new disk has gone already (after only one SR.scan) *)
         maybe (fun uuid ->
             try
               let vdi = Client.VDI.get_by_uuid !rpc session_id uuid in
               debug test "VDI still exists: checking to see whether it is marked as managed";
               if Client.VDI.get_managed !rpc session_id vdi
               then failed test "VDI was not destroyed (or marked as unmanaged) properly after one SR.scan"
             with _ -> ()) !new_uuid
      ) sizes_to_try;

    success test
  end

exception Not_this_host

(* Query /sys to find the actual size of the plugged in device *)
let size_of_dom0_vbd session_id vbd =
  let device = Client.VBD.get_device !rpc session_id vbd in
  let path = Printf.sprintf "/sys/block/%s/size" device in
  try
    let ic = open_in path in
    let result = finally (fun () -> input_line ic) (fun () -> close_in ic) in
    Int64.mul (Int64.of_string (String.strip String.isspace result)) 512L (* sectors of 512 bytes *)
  with (Sys_error _) ->
    (* Assume this means that the device didn't exist on this host -- must be pooling *)
    raise Not_this_host

(** Make sure that VDI_CREATE; plug; VDI_DESTROY; VDI_CREATE; plug results in a device of
    the correct size in dom0 *)
let vdi_create_destroy_plug_checksize caps session_id sr =
  if true
  && (List.mem vdi_create caps)
  && (List.mem vdi_delete caps)
  && (List.mem vdi_attach caps) (* DummySR can't even do this *)
(*
    && (List.mem `vdi_create allowed_ops)  (* The Tools SR is where these two concepts diverge *)
    && (List.mem `vdi_destroy allowed_ops)
*)
  then begin
    let test = make_test "Checking the disk size is correct when a disk is plugged in" 2 in
    start test;
    let pbd = choose_active_pbd session_id sr in
    let host = Client.PBD.get_host !rpc session_id pbd in
    debug test (Printf.sprintf "Will plug into host %s" (Client.Host.get_name_label !rpc session_id host));

    let plug_in_check_size session_id host vdi =
      let size_should_be = Client.VDI.get_virtual_size !rpc session_id vdi in
      let dom0 = dom0_of_host session_id host in
      let vbd = vbd_create_helper ~session_id ~vM:dom0 ~vDI:vdi () in
      Client.VBD.plug !rpc session_id vbd;
      finally
        (fun () ->
           try
             let size_dom0 = size_of_dom0_vbd session_id vbd in
             debug test (Printf.sprintf "XenAPI reports size: %Ld; dom0 reports size: %Ld" size_should_be size_dom0);
             if size_should_be <> size_dom0 then begin
               failed test (Printf.sprintf "Size should have been: %Ld" size_should_be);
               failwith "vdi_create_destroy_plug_checksize"
             end
           with Not_this_host ->
             debug test "Skipping size check: disk is plugged into another host"
        )
        (fun () ->
           Client.VBD.unplug !rpc session_id vbd;
           Client.VBD.destroy !rpc session_id vbd
        ) in

    let small_size = 4L ** mib
    and large_size = 1L ** gib in
    (* Make sure we zap any attached volume state *)
    debug test "Unplugging PBD";
    Client.PBD.unplug !rpc session_id pbd;
    debug test "Plugging PBD";
    Client.PBD.plug !rpc session_id pbd;

    debug test (Printf.sprintf "Creating VDI with requested size: %Ld" small_size);
    let small_vdi = vdi_create_helper ~session_id ~name_label:"small" ~virtual_size:small_size ~sr () in
    debug test (Printf.sprintf "Creating VDI with requested size: %Ld" large_size);
    let large_vdi = vdi_create_helper ~session_id ~name_label:"large" ~virtual_size:large_size ~sr () in
    plug_in_check_size session_id host small_vdi;
    debug test "Destroying VDI";
    Client.VDI.destroy !rpc session_id small_vdi;
    plug_in_check_size session_id host large_vdi;
    debug test "Destroying VDI";
    Client.VDI.destroy !rpc session_id large_vdi;

    success test
  end

(** If VDI_CREATE is supported this will create a fresh VDI, otherwise it will pass an existing
    one for the test function 'f' *)
let with_arbitrary_vdi caps session_id sr f =
  let initial_vdis = count_vdis session_id sr in
  if List.mem vdi_create caps then begin
    let vdi = Client.VDI.create ~rpc:!rpc ~session_id ~name_label:"quicktest" ~name_description:""
        ~sR:sr ~virtual_size:4194304L ~_type:`user ~sharable:false ~read_only:false
        ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[] in
    finally
      (fun () -> f caps session_id sr vdi)
      (fun () -> Client.VDI.destroy !rpc session_id vdi)
  end else begin
    Client.SR.scan !rpc session_id sr;
    match Client.SR.get_VDIs !rpc session_id sr with
    | [] -> ()
    | vdi::_ ->
      f caps session_id sr vdi
  end;
  (* If everything is supposedly ok then: *)
  let test = make_test "Checking for VDI leak" 2 in
  start test;
  let current = count_vdis session_id sr in
  if current <> initial_vdis then begin
    failed test (Printf.sprintf "Initally there were %d VDIs; now there are %d VDIs" initial_vdis current);
    failwith "vdi_leak"
  end else success test

(* When cloning/snapshotting perform field by field comparisons to look for problems *)
let check_fields test list =
  let check (comparison, field, a, b) = match comparison with
    | `Same ->
      if a <> b then failed test (Printf.sprintf "%s field differs: %s <> %s" field a b)
    | `Different ->
      if a = b then failed test (Printf.sprintf "%s field unchanged: %s = %s" field a b) in
  List.iter check list

(* Clones and snapshots should have some identical fields and some different fields: *)
let clone_snapshot_fields a b =
  [ `Same, "virtual_size",
    Int64.to_string a.API.vDI_virtual_size,
    Int64.to_string b.API.vDI_virtual_size;
    `Different, "location",
    a.API.vDI_location, b.API.vDI_location;
  ]

(** If VDI_CLONE and VDI_DELETE are present then make sure VDIs appear and disappear correctly *)
let vdi_clone_destroy caps session_id sr vdi =
  if List.mem vdi_clone caps then begin
    let test = make_test "VDI_CLONE should make a new VDI and VDI_DELETE should remove it" 2 in
    start test;
    vdi_create_clone_snapshot test session_id sr
      (fun () ->
         let vdi' = Client.VDI.clone ~rpc:!rpc ~session_id ~vdi ~driver_params:[] in
         (* Check these look like clones *)
         let a = Client.VDI.get_record ~rpc:!rpc ~session_id ~self:vdi in
         let b = Client.VDI.get_record ~rpc:!rpc ~session_id ~self:vdi' in
         check_fields test (clone_snapshot_fields a b);
         vdi');
    success test;

    Client.SR.scan !rpc session_id sr;
  end

(** If VDI_SNAPSHOT and VDI_DELETE are present then make sure VDIs appear and disappear correctly *)
let vdi_snapshot_destroy ?(indent=2) caps session_id sr vdi =
  if List.mem vdi_snapshot caps then begin
    let test = make_test "VDI_SNAPSHOT should make a new VDI and VDI_DELETE should remove it" indent in
    start test;
    vdi_create_clone_snapshot test session_id sr
      (fun () ->
         let vdi' = Client.VDI.snapshot ~rpc:!rpc ~session_id ~vdi ~driver_params:[] in
         (* Check these look like clones *)
         let a = Client.VDI.get_record ~rpc:!rpc ~session_id ~self:vdi in
         let b = Client.VDI.get_record ~rpc:!rpc ~session_id ~self:vdi' in
         check_fields test (clone_snapshot_fields a b);
         vdi');
    success test
  end

(* Check that snapshot works regardless which host has the VDI activated *)
let vdi_snapshot_in_pool caps session_id sr vdi =
  if (List.mem vdi_snapshot caps) && (List.mem vdi_attach caps)
  then begin
      let hosts = Client.Host.get_all !rpc session_id in
      let do_test () =
        vdi_snapshot_destroy ~indent:4 caps session_id sr vdi in
      let test_snapshot_on host =
        let name = Client.Host.get_name_label !rpc session_id host in
        let test = make_test (Printf.sprintf "Checking VDI.snapshot when plugged in to %s" name) 2 in
        start test;
        let dom0 = dom0_of_host session_id host in
        let vbd = vbd_create_helper ~session_id ~vM:dom0 ~vDI:vdi () in

        debug test (Printf.sprintf "Plugging in to host %s" name);
        Client.VBD.plug !rpc session_id vbd;
        finally do_test
          (fun () ->
            debug test (Printf.sprintf "Unplugging from host %s" name);
            Client.VBD.unplug !rpc session_id vbd;
            debug test "Destroying VBD";
            Client.VBD.destroy !rpc session_id vbd
          );
        success test
      in
      List.iter test_snapshot_on hosts;

      let test = make_test (Printf.sprintf "Checking VDI.snapshot when it is not plugged anywhere") 2 in
      start test;
      do_test ();
      success test
    end


(** If VDI_RESIZE is present then try it out *)
let vdi_resize_test caps session_id sr vdi =
  if List.mem vdi_resize caps then begin
    let test = make_test "VDI_RESIZE should be able to resize a VDI" 2 in
    start test;
    let current = Client.VDI.get_virtual_size !rpc session_id vdi in
    debug test (Printf.sprintf "current size = %Ld" current);
    (* Make it 1 MiB bigger *)
    let new_size = Int64.add current 1048576L in
    debug test (Printf.sprintf "requested size = %Ld" new_size);
    Client.VDI.resize !rpc session_id vdi new_size;
    let actual_size = Client.VDI.get_virtual_size !rpc session_id vdi in
    debug test (Printf.sprintf "final size = %Ld" actual_size);
    if actual_size < new_size then begin
      failed test "The final size should be >= the requested size";
      failwith "vdi_resize"
    end;
    success test
  end

(** If VDI_UPDATE is present then try it out *)
let vdi_update_test caps session_id sr vdi =
  if List.mem vdi_update caps then begin
    let test = make_test "VDI_UPDATE should not fail" 2 in
    start test;
    Client.VDI.update !rpc session_id vdi;
    success test
  end

(** If VDI_GENERATE_CONFIG is present then try it out *)
let vdi_generate_config_test caps session_id sr vdi =
  if List.mem vdi_generate_config caps then begin
    let test = make_test "VDI_GENERATE_CONFIG should not fail" 2 in
    let pbd = choose_active_pbd session_id sr in
    let host = Client.PBD.get_host !rpc session_id pbd in
    start test;
    let (_: string) = Client.VDI.generate_config !rpc session_id host vdi in
    success test
  end

(** If SR_UPDATE is present then try it out *)
let sr_update_test caps session_id sr =
  if List.mem sr_update caps then begin
    let test = make_test "SR_UPDATE should not fail" 2 in
    start test;
    Client.SR.update !rpc session_id sr;
    success test
  end

(** Make sure that I can't call VDI.db_forget *)
let vdi_db_forget caps session_id sr vdi =
  let test = make_test "VDI.db_forget should always fail without authorisation" 2 in
  start test;
  try
    Client.VDI.db_forget !rpc session_id vdi;
    failed test "Call succeeded but it shouldn't have";
    failwith "db_forget"
  with
  | Api_errors.Server_error(code, _) when code = Api_errors.permission_denied ->
    debug test "Caught PERMISSION_DENIED";
    success test
  | e ->
    failed test (Printf.sprintf "Caught wrong error: %s" (Printexc.to_string e))

(** If VDI_INTRODUCE is present then attempt to introduce a VDI with a duplicate location
    and another with a bad UUID to make sure that is reported as an error *)
let vdi_bad_introduce caps session_id sr vdi =
  if List.mem vdi_introduce caps then begin
    let test = make_test "VDI_INTRODUCE should fail when given bad locations or uuids" 2 in
    start test;
    let vdir = Client.VDI.get_record !rpc session_id vdi in
    begin
      try
        debug test (Printf.sprintf "Introducing a VDI with a duplicate UUID (%s)" vdir.API.vDI_uuid);
        let (_: API.ref_VDI) = Client.VDI.introduce ~rpc:!rpc ~session_id
            ~uuid:vdir.API.vDI_uuid ~name_label:"bad uuid" ~name_description:""
            ~sR:vdir.API.vDI_SR ~_type:vdir.API.vDI_type ~sharable:false ~read_only:false ~other_config:[]
            ~location:(Ref.string_of (Ref.make ())) ~xenstore_data:[] ~sm_config:[]
            ~managed:true ~virtual_size:0L ~physical_utilisation:0L ~metadata_of_pool:Ref.null
            ~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null
        in
        failed test "A bad VDI with a duplicate UUID was introduced";
        failwith "vdi_bad_introduce"
      with Api_errors.Server_error(_, _) ->
        debug test "API error caught as expected";
    end;
    begin
      try
        debug test (Printf.sprintf "Introducing a VDI with a duplicate location (%s)" vdir.API.vDI_location);
        let (_: API.ref_VDI) = Client.VDI.introduce ~rpc:!rpc ~session_id
            ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ()))
            ~name_label:"bad location" ~name_description:""
            ~sR:vdir.API.vDI_SR ~_type:vdir.API.vDI_type ~sharable:false ~read_only:false ~other_config:[]
            ~location:vdir.API.vDI_location ~xenstore_data:[] ~sm_config:[]
            ~managed:true ~virtual_size:0L ~physical_utilisation:0L ~metadata_of_pool:Ref.null
            ~is_a_snapshot:false ~snapshot_time:Date.never ~snapshot_of:Ref.null
        in
        failed test "A bad VDI with a duplicate location was introduced";
        failwith "vdi_bad_introduce"
      with Api_errors.Server_error(_, _) ->
        debug test "API error caught as expected";
    end;
    success test
  end



(** Basic support for parsing the SR probe result *)
type sr_probe_sr = { uuid: string }
let parse_sr_probe_xml (xml: string) : sr_probe_sr list =
  match Xml.parse_string xml with
  | Xml.Element("SRlist", _, children) ->
    let parse_sr = function
      | Xml.Element("SR", _, children) ->
        let parse_kv = function
          | Xml.Element(key, _, [ Xml.PCData v ]) ->
            key, String.strip String.isspace v (* remove whitespace at both ends *)
          | _ -> failwith "Malformed key/value pair" in
        let all = List.map parse_kv children in
        { uuid = List.assoc "UUID" all }
      | _ -> failwith "Malformed or missing <SR>" in
    List.map parse_sr children
  | _ -> failwith "Missing <SRlist> element"

(** If SR_PROBE is present then probe for an existing plugged in SR and make sure it can
    be found. *)
let sr_probe_test caps session_id sr =
  if List.mem sr_probe caps then begin
    let test = make_test "SR_PROBE should be able to probe a working SR" 2 in
    start test;
    (* Acquire device config parameters from an attached PBD *)
    let all_pbds = Client.SR.get_PBDs !rpc session_id sr in
    match List.filter (fun pbd -> Client.PBD.get_currently_attached !rpc session_id pbd) all_pbds with
    | [] ->
      failed test "Couldn't find an attached PBD";
      failwith "sr_probe_test"
    | pbd :: _ ->
      let srr = Client.SR.get_record !rpc session_id sr in
      let pbdr = Client.PBD.get_record !rpc session_id pbd in
      Client.PBD.unplug !rpc session_id pbd;
      let xml = finally
        (fun () ->
           Client.SR.probe ~rpc:!rpc ~session_id
               ~host:pbdr.API.pBD_host
               ~device_config:pbdr.API.pBD_device_config
               ~sm_config:srr.API.sR_sm_config
               ~_type:srr.API.sR_type
        )
        (* Restore the original state even if the above code fails *)
        (fun () -> Client.PBD.plug !rpc session_id pbd)
      in
      let srs = parse_sr_probe_xml xml in
      List.iter (fun sr -> debug test (Printf.sprintf "Probe found SR: %s" sr.uuid)) srs;
      if List.length srs = 0 then begin
        failed test "Probe failed to find an SR, even though one is plugged in";
        failwith "sr_probe_test"
      end;
      let all_uuids = List.map (fun sr -> sr.uuid) srs in
      if not(List.mem srr.API.sR_uuid all_uuids) then begin
        failed test (Printf.sprintf "Probe failed to find SR %s even though it is plugged in" srr.API.sR_uuid);
        failwith "sr_probe_test"
      end;
      success test
  end

(** Make sure sr_scan doesn't throw an exception *)
let sr_scan_test caps session_id sr =
  let test = make_test "SR_SCAN should be able to scan a working SR" 2 in
  start test;
  Client.SR.scan !rpc session_id sr;
  success test

let packages_iso_test session_id =
  let test = make_test ("ISO SR should be able to create VDIs for " ^ !iso_path) 2 in
  start test;
  let host = List.hd (Client.Host.get_all !rpc session_id) in
  debug test (Printf.sprintf "Will plug into host %s" (Client.Host.get_name_label !rpc session_id host));
  let sr = Client.SR.introduce ~rpc:!rpc ~session_id ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ()))
      ~name_label:"test tools SR" ~name_description:"" ~_type:"iso" ~content_type:"iso"
      ~shared:true ~sm_config:[] in
  finally
    (fun () ->
       let device_config = [ "location", !iso_path;
                             "legacy_mode", "true" ] in
       let pbd = Client.PBD.create ~rpc:!rpc ~session_id ~sR:sr ~host ~device_config ~other_config:[] in
       finally
         (fun () ->
            debug test "Plugging PBD";
            Client.PBD.plug !rpc session_id pbd;
            Client.SR.scan !rpc session_id sr;
            let is_iso x = String.endswith ".iso" (String.lowercase_ascii x) in
            let files = List.filter is_iso (Array.to_list (Sys.readdir !iso_path)) in
            let vdis = Client.SR.get_VDIs !rpc session_id sr in
            debug test (Printf.sprintf "SR.scan found %d files (directory has %d .isos)" (List.length vdis) (List.length files));
            if List.length files <> List.length vdis then begin
              failed test (Printf.sprintf "%s has %d files; SR has %d VDIs" !iso_path (List.length files) (List.length vdis));
              failwith "packages_iso_test"
            end;
            let locations = List.map (fun vdi -> Client.VDI.get_location !rpc session_id vdi) vdis in
            (* Check each file has a VDI.location *)
            List.iter (fun file ->
                if not(List.mem file locations) then begin
                  failed test (Printf.sprintf "ISO %s has no corresponding VDI" file);
                  failwith "packages_iso_test"
                end) files;
            (* Check each VDI is read-only *)
            List.iter (fun vdi ->
                let vdir = Client.VDI.get_record !rpc session_id vdi in
                if not(vdir.API.vDI_read_only) then begin
                  failed test (Printf.sprintf "ISO VDI has read_only set to false (%s)" vdir.API.vDI_name_label);
                  failwith "packages_iso_test"
                end;
                debug test (Printf.sprintf "ISO VDI %s looks ok" vdir.API.vDI_name_label);
              ) vdis;
            success test
         ) (fun () ->
             Client.PBD.unplug !rpc session_id pbd;
             Client.PBD.destroy !rpc session_id pbd)
    ) (fun () -> Client.SR.forget ~rpc:!rpc ~session_id ~sr)

let sm_caps_of_sr session_id sr =
  let ty = Client.SR.get_type !rpc session_id sr in
  let sm = Client.SM.get_all_records !rpc session_id in
  match List.filter (fun (_, r) -> r.API.sM_type = ty) sm with
  | [ _, plugin ] ->
    plugin.API.sM_capabilities
  | _ ->
    failwith (Printf.sprintf "Failed to query SM plugin type = %s" ty)

(* Even though the SM backend may expose a VDI_CREATE capability attempts
   to actually create a VDI will fail in (eg) the tools SR and any that
   happen to be R/O NFS exports *)
let avoid_vdi_create session_id sr =
  let other_config = Client.SR.get_other_config !rpc session_id sr in
  let is_tools_sr = Client.SR.get_is_tools_sr !rpc session_id sr in
  let special_key = "quicktest-no-VDI_CREATE" in
  let is_marked = List.mem_assoc special_key other_config && List.assoc special_key other_config = "true" in
  is_tools_sr || is_marked

let foreach_sr session_id sr =
  let ty = Client.SR.get_type !rpc session_id sr in
  let name = Client.SR.get_name_label !rpc session_id sr in
  let test = make_test (Printf.sprintf "Querying capabilities of SR type %s (name %s)" ty name) 1 in
  start test;
  let sm = Client.SM.get_all_records !rpc session_id in
  match List.filter (fun (_, r) -> r.API.sM_type = ty) sm with
  | [] ->
    failed test "Failed to query SM plugin"
  | [ _, plugin ] ->
    let caps = plugin.API.sM_capabilities in
    debug test (Printf.sprintf "Capabilities reported: [ %s ]" (String.concat " " caps));
    let oc = Client.SR.get_other_config !rpc session_id sr in
    debug test (Printf.sprintf "SR.other_config = [ %s ]" (String.concat "; " (List.map (fun (k, v) -> k ^ ":" ^ v) oc)));
    let avoid_vdi_create = avoid_vdi_create session_id sr in
    debug test (Printf.sprintf "avoid_vdi_create = %b" avoid_vdi_create);
    (* Mirror the special handling for the XenServer Tools SR; the
       create and delete capabilities are forbidden in that special case.
       See Xapi_sr.valid_operations. *)
    let caps =
      if avoid_vdi_create then
        List.filter
          (fun cap -> not (List.mem cap [ vdi_create; vdi_delete ])) caps
      else
        caps
    in
    debug test (Printf.sprintf "Capabilities filtered to: [ %s ]" (String.concat " " caps));
    success test;

    sr_scan_test       caps session_id sr;
    sr_probe_test      caps session_id sr;
    sr_update_test     caps session_id sr;
    vdi_create_destroy caps session_id sr;
    vdi_create_destroy_plug_checksize caps session_id sr;
    with_arbitrary_vdi caps session_id sr vdi_bad_introduce;
    with_arbitrary_vdi caps session_id sr vdi_db_forget;
    with_arbitrary_vdi caps session_id sr vdi_clone_destroy;
    with_arbitrary_vdi caps session_id sr vdi_snapshot_destroy;
    with_arbitrary_vdi caps session_id sr vdi_snapshot_in_pool;
    with_arbitrary_vdi caps session_id sr vdi_resize_test;
    with_arbitrary_vdi caps session_id sr vdi_update_test;
    with_arbitrary_vdi caps session_id sr vdi_generate_config_test;
  | _ ->
    failed test "Multiple plugins with the same type detected"

let go s =
  let test = make_test "Listing available Storage Repositories" 0 in
  start test;
  let srs = list_srs s in
  debug test (Printf.sprintf "Found %d SRs" (List.length srs));
  success test;
  if !only_sr_name = None then
  packages_iso_test s;
  List.iter (foreach_sr s) srs
