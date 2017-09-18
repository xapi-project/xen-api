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
    ~uSB_group ~host ~attached:Ref.null ~other_config ~path ~vendor_id ~vendor_desc ~product_id
    ~product_desc ~serial ~version ~description ~passthrough_enabled:false;
  debug "PUSB ref='%s' created" (Ref.string_of pusb);
  pusb

let scan_start ~__context usbs =
  let host = Helpers.get_localhost ~__context in
  let known_usb =
    Db.PUSB.get_all_records ~__context
    |> List.filter (fun (rf, rc) -> rc.API.pUSB_host = host)
    |> get_known_usb
  in
  let local_usb = get_local_usb usbs in
  (* Create the newly added pusbs *)
  USBSet.iter (fun s -> let self = create ~__context ~uSB_group:(Ref.null) ~host ~other_config:[] ~path:s.USB.path ~vendor_id:s.USB.vendor_id
                   ~vendor_desc:s.USB.vendor_desc ~product_id:s.USB.product_id ~product_desc:s.USB.product_desc ~serial:s.USB.serial
                   ~version:s.USB.version ~description:s.USB.description in
               let group = Xapi_pusb_helpers.find_or_create ~__context self in
               Db.PUSB.set_USB_group ~__context ~self ~value:group
  ) (USBSet.diff local_usb known_usb);

  List.filter (fun (rf, rc) -> USBSet.mem (extract_known_usb_info rc) (USBSet.diff known_usb local_usb)) (Db.PUSB.get_all_records ~__context)
  |> List.iter (fun (self, _) ->
                  let usb_group = Db.PUSB.get_USB_group ~__context ~self in
                  Db.PUSB.destroy ~__context ~self;
                  let vusbs = Db.USB_group.get_VUSBs ~__context ~self:usb_group in
                  List.iter (fun vusb -> Db.VUSB.destroy ~__context ~self:vusb) vusbs;
                  Db.USB_group.destroy ~__context ~self:usb_group)

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

let scan ~__context =
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
  scan ~__context
