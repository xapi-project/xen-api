(*
 * Copyright (C) 2006-2011 Citrix Systems Inc.
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

open Client
open Stdext
open Xstringext

module D = Debug.Make(struct let name="xapi" end)
open D

let make_task ~__context =
  let uuid = Uuid.make_uuid () in
  let ref = Ref.make () in
  Db.DR_task.create ~__context ~ref ~uuid:(Uuid.to_string uuid);
  ref

(* A type to represent an SR record parsed from an sr_probe result. *)
type sr_probe_sr = {
  uuid: string;
  name_label: string;
  name_description: string;
  metadata_detected: bool;
}

(* Attempt to parse a key/value pair from XML. *)
let parse_kv = function
  | Xml.Element(key, _, [ Xml.PCData v ]) ->
    key, String.strip String.isspace v (* remove whitespace at both ends *)
  | Xml.Element(key, _, []) ->
    key, ""
  | _ ->
    failwith "Malformed key/value pair"

(* Parse a list of SRs from an iscsi/hba SR probe response with sm-config:metadata=true *)
let parse_sr_probe xml =
  match Xml.parse_string xml with
  | Xml.Element("SRlist", _, children) ->
    let parse_sr = function
      | Xml.Element("SR", _, children) ->
        let all = List.map parse_kv children in
        {
          uuid = List.assoc "UUID" all;
          name_label = List.assoc "name_label" all;
          name_description = List.assoc "name_description" all;
          metadata_detected = (List.assoc "pool_metadata_detected" all = "true");
        }
      | _ -> failwith "Malformed or missing <SR>" in
    List.map parse_sr children
  | _ -> failwith "Missing <SRlist> element"

(* Make a best-effort attempt to create an SR and associate it with the DR_task. *)
(* If anything goes wrong, unplug all PBDs which were created, and forget the SR. *)
let try_create_sr_from_record ~__context ~_type ~device_config ~dr_task ~sr_record =
  Helpers.call_api_functions ~__context
    (fun rpc session_id ->
       (* Create the SR record. *)
       debug "Introducing SR %s" sr_record.uuid;
       let sr = Client.SR.introduce ~rpc ~session_id
           ~uuid:sr_record.uuid ~name_label:sr_record.name_label
           ~name_description:sr_record.name_description
           ~_type ~content_type:"" ~shared:true
           ~sm_config:[]
       in
       try
         (* Create and plug PBDs. *)
         Xapi_pool_helpers.call_fn_on_master_then_slaves ~__context
           (fun ~rpc ~session_id ~host ->
              debug "Attaching SR %s to host %s" sr_record.uuid (Db.Host.get_name_label ~__context ~self:host);
              let pbd = Client.PBD.create ~rpc ~session_id ~host ~sR:sr ~device_config ~other_config:[] in
              Client.PBD.plug ~rpc ~session_id ~self:pbd);
         (* Wait until the asynchronous scan is complete and metadata_latest has been updated for all metadata VDIs. *)
         Xapi_dr.wait_until_sr_is_ready ~__context ~sr;
         Db.SR.set_introduced_by ~__context ~self:sr ~value:dr_task
       with e ->
         Backtrace.is_important e;
         (* Clean up if anything goes wrong. *)
         warn "Could not successfully attach SR %s - caught %s" sr_record.uuid (Printexc.to_string e);
         let pbds = Xapi_sr.get_pbds ~__context ~self:sr ~attached:true ~master_pos:`Last in
         List.iter (fun pbd -> Client.PBD.unplug ~rpc ~session_id ~self:pbd) pbds;
         Client.SR.forget ~rpc ~session_id ~sr;
         raise e)

let create ~__context ~_type ~device_config ~whitelist =
  (* Check if licence allows disaster recovery. *)
  Pool_features.assert_enabled ~__context ~f:Features.DR;
  (* Check that the SR type supports metadata. *)
  if not (List.mem_assoc Smint.Sr_metadata (Sm.features_of_driver _type)) then
    raise (Api_errors.Server_error (Api_errors.operation_not_allowed,
                                    [Printf.sprintf "Disaster recovery not supported on SRs of type %s" _type]));
  (* Probe the specified device for SRs. *)
  let master = Helpers.get_master ~__context in
  let probe_result = Helpers.call_api_functions ~__context
      (fun rpc session_id ->
         Client.SR.probe ~rpc ~session_id
           ~host:master ~device_config
           ~_type ~sm_config:["metadata", "true"])
  in
  (* Parse the probe result. *)
  let sr_records =
    try
      parse_sr_probe probe_result
    with Failure msg ->
      raise (Api_errors.Server_error(Api_errors.internal_error,
                                     [Printf.sprintf "SR probe response was malformed: %s" msg]))
  in
  (* If the SR record has a UUID, make sure it's in the whitelist. *)
  let sr_records = List.filter
      (fun sr_record ->
         List.mem sr_record.uuid whitelist)
      sr_records
  in
  (* SR probe went ok, so create the DR task. *)
  let dr_task = make_task ~__context in
  (* Create the SR records and attach each SR to each host. *)
  List.iter
    (fun sr_record ->
       try
         ignore (Db.SR.get_by_uuid ~__context ~uuid:sr_record.uuid);
         (* If an SR with this UUID has already been introduced, don't mess with it. *)
         (* It may have been manually introduced, or introduced by another DR_task. *)
         debug "SR %s has already been introduced, so not adding it to this disaster recovery task." sr_record.uuid;
       with Db_exn.Read_missing_uuid(_, _, _) ->
         try_create_sr_from_record ~__context ~_type ~device_config ~dr_task ~sr_record)
    sr_records;
  dr_task

let destroy ~__context ~self =
  let open Db_filter_types in
  let introduced_SRs = Db.DR_task.get_introduced_SRs ~__context ~self in
  List.iter (fun sr ->
      let pbds = Xapi_sr.get_pbds ~__context ~self:sr ~attached:true ~master_pos:`Last in
      List.iter (fun pbd ->
          debug "Unplugging PBD %s" (Db.PBD.get_uuid ~__context ~self:pbd);
          Helpers.call_api_functions ~__context
            (fun rpc session_id -> Client.PBD.unplug ~rpc ~session_id ~self:pbd)
        ) pbds;
      (* Forget the SR. *)
      debug "Forgetting SR %s (%s)" (Db.SR.get_uuid ~__context ~self:sr) (Db.SR.get_name_label ~__context ~self:sr);
      Helpers.call_api_functions ~__context
        (fun rpc session_id -> Client.SR.forget ~rpc ~session_id ~sr)
    ) introduced_SRs;
  Db.DR_task.destroy ~__context ~self
