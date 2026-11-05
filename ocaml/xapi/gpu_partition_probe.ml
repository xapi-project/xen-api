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

(* The snapshot that the views below read; a view of a later snapshot names
   its own *)
module Wire = Gpumon_interface.Partition_helper.V1_0
module Partition_mode = Gpu.Partition_mode
module Probe = Wire.Probe

(* What the mode and the case are read from: the GPU's address, the MIG mode
   and probe sections, and three members of the geometry section. It is read
   after the core, and a fault in any other member never reaches it *)
module Mode_view = struct
  open Wire

  type device = {bdf: string} [@@deriving rpcty]

  type nullable_device = device Member.nullable

  let typ_of_nullable_device = Member.typ_of_nullable typ_of_device

  type geometry = {
      status: Section_status.t
    ; gpu_instances: Geometry.nullable_gpu_instance_list
    ; profiles_status: Geometry.Profiles_status.t
  }
  [@@deriving rpcty]

  type data = {
      mig_mode: Mig_mode.t option
    ; geometry: geometry option
    ; probe: Probe.t option
  }
  [@@deriving rpcty]

  type t = {device: nullable_device; data: data} [@@deriving rpcty]
end

(* What a page after the first is read from: the GPU's address and the page's
   slice of the GPU instances, which it may leave out *)
module Next_view = struct
  type geometry = {gpu_instances: Wire.Geometry.gpu_instance list option}
  [@@deriving rpcty]

  type data = {geometry: geometry option} [@@deriving rpcty]

  type t = {device: Mode_view.device; data: data} [@@deriving rpcty]
end

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

(* Page 1 as read, built only by [read_page] *)
type first_page = {core: Wire.Envelope.Core.t; mode: Mode_view.t}

(* A page after the first as read, built only by [read_next_page] *)
type next_page = {next: Next_view.t}

(* The pages of one reply, page 1 first *)
type pages = {first: first_page; next: next_page list}

type request = {bdf: string; page_size: int}

let request ~page_size bdf = {bdf; page_size}

type reply = {probe: t}

type output = {exit: int option; stdout: string}

type run_result = (output, xapi_kind) result

let schema_major = fst Wire.schema_version

let helper_args {bdf; page_size; _} =
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
let mig_enabled (name : Wire.Mig_mode.Mode.nullable)
    (raw : Wire.Member.nullable_int) =
  match (name, raw) with
  | Value (Known Enabled), (Value 1 | Null) ->
      Ok (Some true)
  | Value (Known Disabled), (Value 0 | Null) ->
      Ok (Some false)
  | Value (Known (Enabled | Disabled)), Value _
  | Value (Known Wire.Mig_mode.Mode.Unknown), Value (0 | 1) ->
      decode_failed
  | (Null | Value (Known Wire.Mig_mode.Mode.Unknown | Unknown _)), _ ->
      Ok None

let mode_of_mig (m : Wire.Mig_mode.t) =
  let* current = mig_enabled m.current m.current_raw in
  let* after = mig_enabled m.pending m.pending_raw in
  match (m.capable, current, after) with
  | Value true, Some enabled, Some after ->
      Ok (Some Partition_mode.(axis_to_mode (axis ~enabled ~after)))
  | (Null | Value false), _, _ | _, None, _ | _, _, None ->
      Ok None

(* A helper never reports one of xapi's own reasons *)
let helper_reason kind =
  match
    Rpcmarshal.unmarshal typ_of_xapi (Rpcmarshal.marshal Wire.Kind.typ_of kind)
  with
  | Ok (Known _) ->
      decode_failed
  | Ok (Unknown _) | Error _ ->
      Ok (Helper kind)

(* The case is decided by data.probe; only a usage error, which stops before
   the probe, has none *)
let case_of_probe ~has_gpu_instance ~(error : Wire.Envelope.Core.nullable_error)
    (data : Mode_view.data) =
  let* mode = Option.fold ~none:(Ok None) ~some:mode_of_mig data.mig_mode in
  let mig_on =
    Option.fold ~none:false
      ~some:(fun m -> (Partition_mode.axis_of_mode m).enabled)
      mode
  in
  let capable =
    Option.map (fun (m : Wire.Mig_mode.t) -> m.capable) data.mig_mode
  in
  let profiles =
    Option.map (fun (g : Mode_view.geometry) -> g.profiles_status) data.geometry
  in
  (* With MIG on, the profiles never say that MIG is disabled, and are read
     when none is carved *)
  let* () = require (not (mig_on && profiles = Some (Known Mig_disabled))) in
  let unreadable = function
    | Wire.Member.Value kind ->
        let* reason = helper_reason kind in
        Ok (Unreadable {mode; reason})
    | Null ->
        decode_failed
  in
  match (data.probe, mode) with
  | Some {result= Known Probe.Case.Partitions; _}, Some mode
    when mig_on && has_gpu_instance ->
      Ok (Partitions {mode})
  | Some {result= Known Probe.Case.None_carved; _}, Some mode
    when (not mig_on) || profiles = Some (Known Wire.Geometry.Profiles_status.Ok)
    ->
      Ok (None_carved {mode})
  | Some {result= Known Probe.Case.Not_applicable; _}, _
    when capable = Some (Value false) ->
      Ok Not_applicable
  | Some {result= Known Probe.Case.Unreadable; reason; _}, _ ->
      unreadable reason
  (* A newer helper's case reads as Unreadable; the log names the case *)
  | Some {result= Unknown name; _}, _ ->
      Ok (Unreadable {mode; reason= Helper (Unknown name)})
  | None, _ -> (
    match error with
    | Value {Wire.Envelope.Core.kind= Known Wire.Kind.Usage as kind; _} ->
        unreadable (Value kind)
    | Value _ | Null ->
        decode_failed
  )
  | ( Some
        {
          result= Known (Probe.Case.Partitions | None_carved | Not_applicable)
        ; _
        }
    , _ ) ->
      decode_failed

(* A view that cannot be read gets the reason of a reply without a document,
   so the detailed reason is logged *)
let data_unreadable ~bdf ~no_document (`Msg why) =
  D.debug "%s: GPU %s: %s" __FUNCTION__ bdf why ;
  no_document

let read_view typ_of ~bdf ~no_document rpc =
  Rpcmarshal.unmarshal typ_of rpc
  |> Result.map_error (data_unreadable ~bdf ~no_document)

let read_page ~bdf ~no_document (core, rpc) =
  let* mode = read_view Mode_view.typ_of ~bdf ~no_document rpc in
  Ok {core; mode}

let read_next_page ~bdf ~no_document (_, rpc) =
  let* next = read_view Next_view.typ_of ~bdf ~no_document rpc in
  Ok ({next} : next_page)

(* One reply per line, each shorter than the page size asked for; its core is
   read first, and its views only once every page's core is checked *)
let replies_of_stdout ~bdf ~page_size ~no_document stdout =
  let lines = String.split_on_char '\n' (String.trim stdout) in
  let* () =
    require (List.for_all (fun l -> String.length l < page_size) lines)
  in
  let read_core rpc =
    (rpc, Rpcmarshal.unmarshal Wire.Envelope.Core.typ_of rpc)
  in
  let reply line =
    match read_core (Jsonrpc.of_string line) with
    | rpc, Ok ({schema= "gpu-partition-helper"; schema_version= m, _; _} as core)
      when m = schema_major ->
        Ok (core, rpc)
    | _, Ok {schema= "gpu-partition-helper"; _} ->
        Error (Helper (Known Schema_unsupported))
    | _, Ok _ ->
        decode_failed
    (* rpclib reads an integer beyond 64 bits as a Failure *)
    | _, Error (`Msg why) | (exception (Yojson.Json_error why | Failure why)) ->
        D.debug "%s: GPU %s: %s" __FUNCTION__ bdf why ;
        Error no_document
  in
  Xapi_stdext_std.Listext.List.try_map reply lines

let read_pages ~bdf ~no_document = function
  | first :: next ->
      let* first = read_page ~bdf ~no_document first in
      let* next =
        Xapi_stdext_std.Listext.List.try_map
          (read_next_page ~bdf ~no_document)
          next
      in
      Ok {first; next}
  | [] ->
      decode_failed

(* Pages come in order, all with the first page's count and size, and the
   reply ends at its last page *)
let check_pages ~page_size ~no_document cores =
  let page (core : Wire.Envelope.Core.t) =
    match core.page with
    | None when List.compare_length_with cores 1 = 0 ->
        Some (1, 1, page_size)
    | Some {index; count; size; encoding= "identity"; _} when size <= page_size
      ->
        Some (index, count, size)
    | None | Some _ ->
        None
  in
  let pages = List.map page cores in
  match pages with
  | Some (_, count, size) :: _
    when pages = List.mapi (fun i _ -> Some (i + 1, count, size)) pages ->
      let n = List.length pages in
      if n < count then
        Error no_document
      else
        require (n = count)
  | (Some _ | None) :: _ | [] ->
      decode_failed

(* Page 1 gives the mode and the case. No page names another GPU, and a page
   after the first lists GPU instances only if page 1 read them *)
let case_of_pages ~bdf {first; next} =
  let on_first, opened =
    match first.mode.data.geometry with
    | Some {gpu_instances= Value l; _} ->
        (l <> [], true)
    | Some {gpu_instances= Null; _} | None ->
        (false, false)
  in
  let lists (p : next_page) =
    match p.next.data.geometry with
    | Some {gpu_instances= Some (_ :: _); _} ->
        true
    | Some {gpu_instances= Some [] | None; _} | None ->
        false
  in
  let asked =
    match first.mode.device with Null -> true | Value d -> d.bdf = bdf
  in
  if
    asked
    && List.for_all (fun (p : next_page) -> p.next.device.bdf = bdf) next
    && (opened || not (List.exists lists next))
  then
    case_of_probe
      ~has_gpu_instance:(on_first || List.exists lists next)
      ~error:first.core.error first.mode.data
  else
    decode_failed

let decode {bdf; page_size; _} run =
  let pages =
    let* {exit; stdout} = Result.map_error (fun kind -> Xapi kind) run in
    (* Without a document, the exit code says why. An exec can fail after the
       access check passed, e.g. for a missing interpreter: 127 or 126 *)
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
    let* () = check_pages ~page_size ~no_document (List.map fst replies) in
    read_pages ~bdf ~no_document replies
  in
  let case = Result.bind pages (case_of_pages ~bdf) in
  let unreadable reason = Unreadable {mode= None; reason} in
  {probe= Result.fold ~ok:Fun.id ~error:unreadable case}
