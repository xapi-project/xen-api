(*
 * Copyright (C) 2026 Cloud Software Group
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

(* The release profile does not make warning 8 an error, so without this a new
   case or kind with no reading would still compile there. *)
[@@@warning "@8"]

module D = Debug.Make (struct let name = __MODULE__ end)

module Wire = Gpumon_interface
module Partition_mode = Gpu.Partition_mode
module Probe_result = Wire.Probe.Case
module Probe = Wire.Probe

type xapi_kind =
  | Helper_absent
  | Helper_not_executable
  | Caller_timeout
  | Helper_crashed
  | Decode_failed
  | Card_busy
[@@deriving rpcty]

type reason = Helper of Wire.Kind.t | Xapi of xapi_kind

let typ_of_xapi = Wire.Member.typ_of_open_enum typ_of_xapi_kind

let wire_name typ v = Rpcmarshal.marshal typ v |> Rpc.string_of_rpc

let string_of_reason = function
  | Helper kind ->
      wire_name Wire.Kind.typ_of kind
  | Xapi kind ->
      wire_name typ_of_xapi (Known kind)

type t =
  | Partitions of {mode: Partition_mode.mode}
  | None_carved of {mode: Partition_mode.mode}
  | Unreadable of {mode: Partition_mode.mode option; reason: reason}
  | Not_applicable

type reply = {probe: t}

type output = {exit: int option; stdout: string}

type run_result = (output, xapi_kind) result

let schema_major = 0

let helper_args ~page_size bdf =
  [
    "mig"
  ; "--id=" ^ bdf
  ; "--geometry"
  ; Printf.sprintf "--schema=%d" schema_major
  ; Printf.sprintf "--page-size=%d" page_size
  ; "--page-encodings=identity"
  ]

let axes = function
  | Partitions {mode; _} | None_carved {mode; _} ->
      [Some mode]
  | Unreadable {mode; _} ->
      [mode]
  | Not_applicable ->
      [Some Partition_mode.Not_supported]

let ( let* ) = Result.bind

let decode_failed = Error (Xapi Decode_failed)

let require ok =
  if ok then
    Ok ()
  else
    decode_failed

(* A raw value, when given, agrees with the name *)
let mig_enabled name raw =
  match (name, raw) with
  | Some (Wire.Member.Known Wire.Mig_mode.Mode.Enabled), (Some 1 | None) ->
      Ok (Some true)
  | Some (Known Disabled), (Some 0 | None) ->
      Ok (Some false)
  | Some (Known (Enabled | Disabled)), Some _
  | Some (Known Unknown), Some (0 | 1) ->
      decode_failed
  | (None | Some (Known Unknown | Unknown _)), _ ->
      Ok None

let mode_of_mig (m : Wire.Mig_mode.t) =
  let* current = mig_enabled m.current m.current_raw in
  let* after = mig_enabled m.pending m.pending_raw in
  match (m.capable, current, after) with
  | Some true, Some enabled, Some after ->
      Ok (Some Partition_mode.(axis_to_mode (axis ~enabled ~after)))
  | (None | Some false), _, _ | _, None, _ | _, _, None ->
      Ok None

(* A helper never reports one of xapi's own reasons *)
let helper_reason kind =
  match
    Rpcmarshal.unmarshal typ_of_xapi
      (Rpc.String (wire_name Wire.Kind.typ_of kind))
  with
  | Ok (Known _) ->
      decode_failed
  | Ok (Unknown _) | Error _ ->
      Ok (Helper kind)

(* The case is decided by data.probe; only a usage error, which stops before
   the probe, has none *)
let case_of_probe ~has_gpu_instance ~(error : Wire.Envelope.nullable_error)
    (data : Probe.data) =
  let* mode = Option.fold ~none:(Ok None) ~some:mode_of_mig data.mig_mode in
  let mig_on =
    Option.fold ~none:false
      ~some:(fun m -> (Partition_mode.axis_of_mode m).enabled)
      mode
  and capable = Option.bind data.mig_mode (fun m -> m.capable)
  and profiles =
    Option.map (fun (g : Wire.Geometry.t) -> g.profiles_status) data.geometry
  in
  (* With MIG on, the profiles never say that MIG is disabled, and are read
     when none is carved *)
  let* () = require (not (mig_on && profiles = Some (Known Mig_disabled))) in
  let unreadable = function
    | Some kind ->
        let* reason = helper_reason kind in
        Ok (Unreadable {mode; reason})
    | None ->
        decode_failed
  in
  match (data.probe, mode) with
  | Some {result= Known Probe_result.Partitions; _}, Some mode
    when mig_on && has_gpu_instance ->
      Ok (Partitions {mode})
  | Some {result= Known Probe_result.None_carved; _}, Some mode
    when (not mig_on) || profiles = Some (Known Wire.Geometry.Profiles_status.Ok)
    ->
      Ok (None_carved {mode})
  | Some {result= Known Probe_result.Not_applicable; _}, _
    when capable = Some false ->
      Ok Not_applicable
  | Some {result= Known Probe_result.Unreadable; reason; _}, _ ->
      unreadable reason
  | Some {result= Unknown _ as kind; _}, _ ->
      Ok (Unreadable {mode; reason= Helper kind})
  | None, _ -> (
    match error with
    | Some {Wire.Envelope.kind= Known Wire.Kind.Usage as kind; _} ->
        unreadable (Some kind)
    | Some _ | None ->
        decode_failed
  )
  | ( Some
        {
          result= Known (Probe_result.Partitions | None_carved | Not_applicable)
        ; _
        }
    , _ ) ->
      decode_failed

(* One reply per line, each shorter than the page size asked for *)
let replies_of_stdout ~bdf ~page_size ~no_document stdout =
  let lines = String.split_on_char '\n' (String.trim stdout) in
  let* () =
    require (List.for_all (fun l -> String.length l < page_size) lines)
  in
  let reply line =
    match Rpcmarshal.unmarshal Probe.typ_of_reply (Jsonrpc.of_string line) with
    | Ok ({schema= "gpu-partition-helper"; schema_version= m, _; _} as reply)
      when m = schema_major ->
        Ok reply
    | Ok {schema= "gpu-partition-helper"; _} ->
        Error (Helper (Known Schema_unsupported))
    | Ok _ ->
        decode_failed
    (* rpclib reads an integer beyond 64 bits as a Failure *)
    | Error (`Msg why) | (exception (Yojson.Json_error why | Failure why)) ->
        D.debug "%s: GPU %s: %s" __FUNCTION__ bdf why ;
        Error no_document
  in
  Xapi_stdext_std.Listext.List.try_map reply lines

(* Pages come in order, all with the first page's count and size, and the
   reply ends at its last page *)
let check_pages ~page_size ~no_document (replies : Probe.reply list) =
  let page (r : Probe.reply) =
    match r.page with
    | None when List.compare_length_with replies 1 = 0 ->
        Some (1, 1, page_size)
    | Some {index; count; size; encoding= "identity"; _} when size <= page_size
      ->
        Some (index, count, size)
    | None | Some _ ->
        None
  in
  let pages = List.map page replies in
  match pages with
  | Some (_, count, size) :: _
    when pages = List.mapi (fun i _ -> Some (i + 1, count, size)) pages ->
      let n = List.length pages in
      if n < count then
        Error no_document
      else
        require (n = count)
  | _ ->
      decode_failed

(* The card reads only decode_failed, so the reader's reason is logged *)
let data_unreadable ~bdf (`Msg why) =
  D.debug "%s: GPU %s: %s" __FUNCTION__ bdf why ;
  Xapi Decode_failed

(* Every page repeats the card asked for, its mode and its case *)
let case_of_pages ~bdf (replies : Probe.reply list) =
  let header (r : Probe.reply) =
    (r.device, Result.map (fun (d : Probe.data) -> (d.mig_mode, d.probe)) r.data)
  and has_gpu_instance (r : Probe.reply) =
    match r.data with
    | Ok {geometry= Some {gpu_instances= Some (_ :: _); _}; _} ->
        true
    | Ok _ | Error _ ->
        false
  in
  match replies with
  | first :: _
    when Option.fold ~none:true
           ~some:(fun (d : Wire.Device.t) -> d.bdf = bdf)
           first.device
         && List.for_all (fun r -> header r = header first) replies ->
      let* data = Result.map_error (data_unreadable ~bdf) first.data in
      case_of_probe
        ~has_gpu_instance:(List.exists has_gpu_instance replies)
        ~error:first.error data
  | _ ->
      decode_failed

let decode ~bdf ~page_size run =
  let probe =
    let* {exit; stdout} = Result.map_error (fun kind -> Xapi kind) run in
    (* Without a document, the exit code says why *)
    let no_document =
      match exit with
      | Some 0 ->
          Xapi Decode_failed
      | Some 127 ->
          Xapi Helper_absent
      | Some 126 ->
          Xapi Helper_not_executable
      | Some _ | None ->
          Xapi Helper_crashed
    in
    let* replies = replies_of_stdout ~bdf ~page_size ~no_document stdout in
    let* () = check_pages ~page_size ~no_document replies in
    case_of_pages ~bdf replies
  in
  let unreadable reason = Unreadable {mode= None; reason} in
  {probe= Result.fold ~ok:Fun.id ~error:unreadable probe}
