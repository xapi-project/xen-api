(*
 * Copyright (C) Citrix Systems Inc.
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
open Listext
open Threadext
open Xapi_pusb_helpers
module D = Debug.Make(struct let name="xapi" end)
open D

let create ~__context ~uSB_group ~host ~other_config ~path
    ~vendor_id ~vendor_desc ~product_id ~product_desc ~serial ~version ~description =
  let pusb = Ref.make () and uuid = Uuid.make_uuid () in
  let host = Helpers.get_localhost ~__context in
  Db.PUSB.create ~__context ~ref:pusb ~uuid:(Uuid.to_string uuid)
    ~uSB_group ~host ~other_config ~path ~vendor_id ~vendor_desc ~product_id
    ~product_desc ~serial ~version ~description ~passthrough_enabled:false;
  debug "PUSB ref='%s' created" (Ref.string_of pusb);
  pusb

let scan_start ~__context usbs =
  (* We compute two sets and operate on them:
     (1) Local USB devices that are not yet in the database - we create entries for them.
     (2) USB devices only present in the database for the local host - we delete
     them from the database.
  *)
  let host = Helpers.get_localhost ~__context in
  let known_pusbs_in_db =
    Db.PUSB.get_all_records ~__context
    |> List.filter (fun (rf, rc) -> rc.API.pUSB_host = host)
  in
  let known_usb_set =
    known_pusbs_in_db |> get_known_usb
  in
  let local_usb_set = get_local_usb usbs in
  (* Create the newly added pusbs *)
  USBSet.iter (fun s -> let self = create ~__context ~uSB_group:(Ref.null) ~host ~other_config:[] ~path:s.USB.path ~vendor_id:s.USB.vendor_id
                            ~vendor_desc:s.USB.vendor_desc ~product_id:s.USB.product_id ~product_desc:s.USB.product_desc ~serial:s.USB.serial
                            ~version:s.USB.version ~description:s.USB.description in
                let group = Xapi_pusb_helpers.find_or_create ~__context self in
                Db.PUSB.set_USB_group ~__context ~self ~value:group
              ) (USBSet.diff local_usb_set known_usb_set);

  List.filter (fun (rf, rc) -> USBSet.mem (extract_known_usb_info rc) (USBSet.diff known_usb_set local_usb_set)) known_pusbs_in_db
  |> List.iter (fun (self, _) ->
      try
        Xapi_pusb_helpers.destroy_pusb ~__context self;
      with e -> error "Caught exception while removing PUSB %s: %s" (Ref.string_of self) (Printexc.to_string e);
    )

let cond = Condition.create ()
let mutex = Mutex.create ()
let scan_required : bool ref = ref false

let start_thread f =
  ignore(Thread.create (fun () ->
      while true do
        Mutex.execute mutex (fun () ->
            while not !scan_required do
              Condition.wait cond mutex
            done;
            (* scan_required is true now. *)
            scan_required := false;
          );
        try
          f ();
        with e ->
          Printf.printf "Caught exception from scan_start '%s' \n%!" (Printexc.to_string e);
          ()
      done) ())

let scan ~__context ~host =
  (* notify that scan is required. *)
  Mutex.execute mutex (fun () ->
      scan_required := true;
      Condition.broadcast cond
    )

let scan_thread ~__context =
  let f () =
    let usbs = get_script_stdout () |> get_usbs in
    scan_start ~__context usbs
  in
  start_thread f;
  scan ~__context ~host:(Helpers.get_localhost ~__context)

let get_sm_usb_path ~__context vdi =
  try
    let vdi_sc = Db.VDI.get_sm_config ~__context ~self:vdi in
    List.assoc Xapi_globs.usb_path vdi_sc
  with _ -> ""

let set_passthrough_enabled ~__context ~self ~value =
  Mutex.execute mutex (fun () ->
      match value with
      | true ->
        (* Remove the vdi records which 'usb_path' in sm-config has the
           same value with the field 'path' in PUSB. *)
        let pusb_path = Db.PUSB.get_path ~__context ~self in
        let udev_srs = Db.SR.get_refs_where ~__context ~expr:(Eq(Field "type", Literal "udev")) in
        List.iter (fun sr ->
            Db.VDI.get_refs_where ~__context ~expr:(Eq(Field "SR", Literal (Ref.string_of sr))) |>
            List.iter (fun rf ->
                try
                  if (get_sm_usb_path ~__context rf) = pusb_path then begin
                    let vbds = Db.VDI.get_VBDs ~__context ~self:rf in
                    if vbds <> [] then
                      raise (Api_errors.Server_error(Api_errors.pusb_vdi_conflict, [ Ref.string_of self; Ref.string_of rf ]));
                    Xapi_vdi.forget ~__context ~vdi:rf
                  end;
                with e ->
                  debug "Caught failure during remove vdi records.";
                  raise e
              )
          ) udev_srs;
        debug "set passthrough_enabled %b" value;
        Db.PUSB.set_passthrough_enabled ~__context ~self ~value
      | false ->
        try
          let usb_group = Db.PUSB.get_USB_group ~__context ~self in
          let vusbs = Db.USB_group.get_VUSBs ~__context ~self:usb_group in
          (* If the USB is passthroughed to vm, need to unplug it firstly*)
          let _ = match vusbs with
            | [] -> ()
            | _ :: _ :: _ -> raise Api_errors.(Server_error(internal_error,
                                                            [Printf.sprintf "too many vusb on the USB_group: %s" (Ref.string_of usb_group)]))
            | [vusb] ->
              let currently_attached = Db.VUSB.get_currently_attached ~__context ~self:vusb in
              if currently_attached then
                let vm = Db.VUSB.get_VM ~__context ~self:vusb in
                raise (Api_errors.Server_error(Api_errors.usb_already_attached, [Ref.string_of self; Ref.string_of vm]))
          in
          (* If vusb has been created, need to destroy it. *)
          List.iter (fun vusb -> Db.VUSB.destroy ~__context ~self:vusb) vusbs;
          debug "set passthrough_enabled %b." value;
          Db.PUSB.set_passthrough_enabled ~__context ~self ~value;
          (* Re-display the removed vdi records. There is a problem here that
             we scan all the udev SR, as we cannot get the SR corresponding to the PUSB when
             we want to re-display the vdi records. But in udevSR.py we will handle this, as
             if passthrough_enabled = true, we will not re-introduce the vdi.
          *)
          let open Db_filter_types in
          Db.SR.get_refs_where ~__context ~expr:(Eq(Field "type", Literal "udev"))
          |> List.iter (fun sr -> Helpers.call_api_functions ~__context (fun rpc session_id -> Client.Client.SR.scan rpc session_id sr));
        with e ->
          debug "Caught failure during set passthrough_enabled %b." value;
          raise e
    )
