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

open Gpu_partition_probe
module Wire = Gpumon_interface
module Partition_mode = Gpu.Partition_mode
module Mode = Wire.Mig_mode.Mode
module Probe = Wire.Probe

(* Replies of the GPU partition helper with only the members xapi reads; the
   others are absent where the type allows, and a lenient member absent is
   written as {}, a value its reader refuses, so every case also shows that
   an unreadable profile catalogue costs only itself *)
let card = "0000:c4:00.0"

let device bdf : Wire.Device.t =
  {
    bdf
  ; nvml_bus_id= Wire.Member.absent
  ; pci_device_id= Wire.Member.absent
  ; pci_subsystem_id= Wire.Member.absent
  ; name= Wire.Member.absent
  ; name_status= None
  ; architecture= Wire.Member.absent
  ; architecture_raw= Wire.Member.absent
  }

let mig_mode current pending : Wire.Mig_mode.t =
  let raw = function Mode.Enabled -> 1 | Disabled -> 0 | Unknown -> 2 in
  {
    status= Known Wire.Section_status.Ok
  ; kind= None
  ; capable= Some true
  ; current= Some (Known current)
  ; pending= Some (Known pending)
  ; current_raw= Some (raw current)
  ; pending_raw= Some (raw pending)
  }

let geometry profiles_status gpu_instances : Wire.Geometry.t =
  {
    status= Known Wire.Section_status.Ok
  ; kind= Wire.Member.absent
  ; gpu_instances
  ; profiles_status= Known profiles_status
  ; profiles= Wire.Member.absent
  ; profile_index_bound= Wire.Member.absent
  ; type_profiles= Wire.Member.absent
  }

let gpu_instance : Wire.Geometry.gpu_instance =
  {
    gi_id= 1
  ; profile_id= 47
  ; profile_name= "MIG 1g.24gb"
  ; name_status= None
  ; placement= {start= 0; size= 1}
  ; compute_instances= []
  }

let error kind : Wire.Envelope.nullable_error =
  Some
    {
      kind
    ; message= "failed"
    ; nvml= Wire.Member.absent
    ; symbol= Wire.Member.absent
    ; schema_majors= None
    }

let reply ?mig_mode ?geometry ?reason result : Probe.reply =
  {
    schema= "gpu-partition-helper"
  ; schema_version= (0, 0)
  ; page= Some {index= 1; count= 1; size= 4096; encoding= "identity"}
  ; implementation= Wire.Member.absent
  ; command= Wire.Member.absent
  ; status= Known Wire.Envelope.Status.Ok
  ; error= None
  ; nvml= Wire.Member.absent
  ; device= Some (device card)
  ; lock= Wire.Member.absent
  ; data= Ok {mig_mode; geometry; vgpu= None; probe= Some {result; reason}}
  }

let partitions =
  reply ~mig_mode:(mig_mode Enabled Enabled)
    ~geometry:(geometry Wire.Geometry.Profiles_status.Ok (Some [gpu_instance]))
    (Known Partitions)

let none_carved =
  reply
    ~mig_mode:(mig_mode Disabled Disabled)
    ~geometry:(geometry Mig_disabled None)
    (Known None_carved)

let not_applicable =
  reply
    ~mig_mode:{(mig_mode Disabled Disabled) with capable= Some false}
    (Known Not_applicable)

(* A reply that stops at an error of the helper *)
let failed kind =
  {
    (reply ~reason:kind (Known Unreadable)) with
    status= Known Wire.Envelope.Status.Error
  ; error= error kind
  ; device= None
  }

let with_data f (r : Probe.reply) = {r with data= Result.map f r.data}

let with_mig m = with_data (fun d -> {d with Probe.mig_mode= Some m})

let with_geometry profiles_status gpu_instances =
  with_data (fun d ->
      {d with Probe.geometry= Some (geometry profiles_status gpu_instances)}
  )

let with_probe ?reason result =
  with_data (fun d -> {d with Probe.probe= Some {result; reason}})

let without_probe r = with_data (fun d -> {d with Probe.probe= None}) r

let mig_on_none_carved =
  none_carved
  |> with_mig (mig_mode Enabled Enabled)
  |> with_geometry Wire.Geometry.Profiles_status.Ok (Some [])

(* A later page of the helper's page format holds an empty slice of the GPU
   instances *)
let later_page =
  with_geometry Wire.Geometry.Profiles_status.Ok (Some []) partitions

let page ?(size = 4096) ?(encoding = "identity") ~count index (r : Probe.reply)
    =
  {r with page= Some {index; count; size; encoding}}

(* The GPU instance is on page 2 *)
let pages ~count =
  List.map (function
    | 2 ->
        page ~count 2 partitions
    | index ->
        page ~count index later_page
    )

(* Two pages whose second is r *)
let second_page_differs r = pages ~count:2 [1] @ [page ~count:2 2 r]

let written r = Jsonrpc.to_string (Rpcmarshal.marshal Probe.typ_of_reply r)

(* The helper prints each page on one line *)
let lines replies =
  String.concat "" (List.map (fun r -> written r ^ "\n") replies)

let without_last_bytes n s = String.sub s 0 (String.length s - n)

(* One written line of the reply, padded with blanks to length bytes *)
let padded_to length r =
  let line = written r in
  without_last_bytes 1 line
  ^ String.make (length - String.length line) ' '
  ^ "}\n"

let with_member key value r =
  match Rpcmarshal.marshal Probe.typ_of_reply r with
  | Rpc.Dict members ->
      Jsonrpc.to_string
        (Rpc.Dict ((key, value) :: List.remove_assoc key members))
  | rpc ->
      Jsonrpc.to_string rpc

(* What xapi reads from a reply: its case and the partition mode read from it *)
let reading : (Gpu_partition_probe.t * API.partition_mode) Alcotest.testable =
  let describe = function
    | Partitions {mode; _} ->
        "partitions " ^ Partition_mode.string_of_mode mode
    | None_carved {mode; _} ->
        "none carved " ^ Partition_mode.string_of_mode mode
    | Unreadable {reason; _} ->
        "unreadable " ^ string_of_reason reason
    | Not_applicable ->
        "not applicable"
  in
  Alcotest.(
    pair
      (testable (Fmt.of_to_string describe) ( = ))
      (testable (Fmt.of_to_string API.partition_mode_to_string) ( = ))
  )

let read_as ?(asked = card) ?(exit = 0) stdout expected () =
  let {probe; _} =
    decode ~bdf:asked ~page_size:4096 (Ok {exit= Some exit; stdout})
  in
  (probe, Partition_mode.(to_api (of_observations (axes probe))))
  |> Alcotest.check reading "case and partition mode" expected

let reads name ?asked ?exit replies expected =
  Alcotest.test_case name `Quick (read_as ?asked ?exit (lines replies) expected)

(* Output that no writer of the reply makes *)
let reads_output name ?exit stdout expected =
  Alcotest.test_case name `Quick (read_as ?exit stdout expected)

let partitioned = (Partitions {mode= Enabled}, `enabled)

let unknown_because reason = (Unreadable {mode= None; reason}, `unknown)

let helper_said kind = unknown_because (Helper kind)

let refused = unknown_because (Xapi Decode_failed)

let crashed = unknown_because (Xapi Helper_crashed)

let mode_kept reason = (Unreadable {mode= Some Enabled; reason}, `enabled)

let reasons_are_spelt_as_the_helper_spells_kinds () =
  List.map string_of_reason [Helper (Known Lock_timeout); Xapi Card_busy]
  |> Alcotest.(check (list string)) "reasons" ["lock_timeout"; "card_busy"]

let decoding =
  [
    Alcotest.test_case "reasons are spelt as the helper spells kinds" `Quick
      reasons_are_spelt_as_the_helper_spells_kinds
  ; reads "a card with GPU instances reads as enabled" [partitions] partitioned
  ; reads "a card with MIG off and none carved reads as disabled" [none_carved]
      (None_carved {mode= Disabled}, `disabled)
  ; reads "a card whose MIG turns on at the next reset is enable_on_reboot"
      [with_mig (mig_mode Disabled Enabled) none_carved]
      (None_carved {mode= Enable_on_reboot}, `enable_on_reboot)
  ; reads "a card that cannot be partitioned reads as not_supported"
      [not_applicable]
      (Not_applicable, `not_supported)
  ; reads "a card whose MIG mode the backend cannot tell reads as unknown"
      [
        with_probe ~reason:(Known Mode_unknown) (Known Unreadable)
          (with_mig (mig_mode Unknown Enabled) mig_on_none_carved)
      ]
      (helper_said (Known Mode_unknown))
  ; reads "a later minor version is read"
      [{mig_on_none_carved with schema_version= (0, 7)}]
      (None_carved {mode= Enabled}, `enabled)
  ; reads "a partial reply with a backend error reads its partitions"
      [{partitions with status= Known Partial; error= error (Known Nvml_error)}]
      partitioned
  ; reads "a reply in four pages reads as one"
      (pages ~count:4 [1; 2; 3; 4])
      partitioned
  ; reads "a lock timeout is kept as the reason" ~exit:75
      [failed (Known Lock_timeout)]
      (helper_said (Known Lock_timeout))
  ; reads "an unsafe lock is kept as the reason" ~exit:77
      [failed (Known Lock_unsafe)]
      (helper_said (Known Lock_unsafe))
  ; reads "a schema the helper does not support is kept as the reason" ~exit:64
      [failed (Known Schema_unsupported)]
      (helper_said (Known Schema_unsupported))
  ; reads "a backend error is kept as the reason" ~exit:1
      [failed (Known Nvml_error)]
      (helper_said (Known Nvml_error))
  ; reads "a usage error, which has no probe, is kept as the reason" ~exit:64
      [without_probe (failed (Known Usage))]
      (helper_said (Known Usage))
  ; reads "an unknown error kind is kept as the reason" ~exit:1
      [failed (Unknown "x_future")]
      (helper_said (Unknown "x_future"))
  ; reads "an unknown probe result keeps the mode read"
      [with_probe (Unknown "x_future") partitions]
      (mode_kept (Helper (Unknown "x_future")))
  ; reads "an unreadable card whose mode was read keeps it" ~exit:1
      [with_probe ~reason:(Unknown "x_future") (Known Unreadable) partitions]
      (mode_kept (Helper (Unknown "x_future")))
  ; reads "an unknown envelope status is not read" ~exit:1
      [
        with_probe ~reason:(Known Not_supported) (Known Unreadable)
          {partitions with status= Unknown "x_future"}
      ]
      (mode_kept (Helper (Known Not_supported)))
  ; reads "a helper giving one of xapi's own reasons is refused" ~exit:75
      [failed (Unknown "caller_timeout")]
      refused
  ; reads "a reply without a probe, other than a usage error, is refused"
      ~exit:75
      [without_probe (failed (Known In_use))]
      refused
  ; reads "another major version reads as schema_unsupported"
      [{mig_on_none_carved with schema_version= (2, 0)}]
      (helper_said (Known Schema_unsupported))
  ; reads "a helper's refusal in another major reads as schema_unsupported"
      ~exit:64
      [
        without_probe
          {(failed (Known Schema_unsupported)) with schema_version= (2, 0)}
      ]
      (helper_said (Known Schema_unsupported))
  ; reads "a reply for another card is refused" ~asked:"0000:27:00.0"
      [partitions] refused
  ; reads "partitions without a GPU instance are refused" [later_page] refused
  ; reads "none carved with MIG on and the profiles unread is refused"
      [with_geometry Error (Some []) mig_on_none_carved]
      refused
  ; reads "none carved without a known MIG mode is refused"
      [with_mig (mig_mode Unknown Enabled) none_carved]
      refused
  ; reads "not applicable on a card that can be partitioned is refused"
      [with_mig (mig_mode Disabled Disabled) not_applicable]
      refused
  ; reads "a reply of another schema is refused"
      [{partitions with schema= "gpu-partition-helper-other"}]
      refused
  ; reads "a raw MIG mode that disagrees with its name is refused"
      [
        with_mig {(mig_mode Enabled Enabled) with current_raw= Some 0} partitions
      ]
      refused
  ; reads "profiles saying MIG is off while MIG is on are refused"
      [with_geometry Mig_disabled (Some [gpu_instance]) partitions]
      refused
  ; reads "a raw value of 0 or 1 with the MIG mode unknown is refused" ~exit:75
      [
        with_mig
          {(mig_mode Unknown Unknown) with current_raw= Some 1}
          (failed (Known In_use))
      ]
      refused
  ; reads_output "a reply whose data cannot be read is refused"
      (with_member "data" (Rpc.Int 1L) partitions ^ "\n")
      refused
  ; reads_output "output that is not JSON, with a failing exit, is a crash"
      ~exit:9 "Segmentation fault\n" crashed
  ; reads_output "no output with exit 127 reads as helper_absent" ~exit:127 ""
      (unknown_because (Xapi Helper_absent))
  ; reads_output "no output with exit 126 reads as helper_not_executable"
      ~exit:126 ""
      (unknown_because (Xapi Helper_not_executable))
  ; reads_output "no output with exit 0 is refused" "" refused
  ; (* Rpc.Int holds 64 bits, so no writer makes this number *)
    reads_output "an integer beyond 64 bits is refused"
      (without_last_bytes 2 (lines [partitions])
      ^ {|,"x":18446744073709551616}|}
      ^ "\n"
      )
      refused
  ; (* Lines are read in a loop, so their number cannot exhaust the stack *)
    reads_output "a page followed by a million empty lines is refused"
      (written partitions ^ String.make 1_000_000 '\n' ^ written partitions)
      refused
  ; reads_output "a line as long as the page size is refused"
      (padded_to 4096 partitions)
      refused
  ; reads_output "a line one byte shorter than the page size is read"
      (padded_to 4095 partitions)
      partitioned
  ; reads "a reply cut after two of four pages, with exit 0, is refused"
      (pages ~count:4 [1; 2])
      refused
  ; reads "a reply cut after two of four pages, with a failing exit, is a crash"
      ~exit:1
      (pages ~count:4 [1; 2])
      crashed
  ; reads "pages out of order are refused" (pages ~count:4 [1; 3; 2; 4]) refused
  ; reads "a repeated page is refused" (pages ~count:4 [1; 2; 2; 3; 4]) refused
  ; reads "a page count that changes between pages is refused"
      (pages ~count:4 [1] @ [page ~count:5 2 partitions] @ pages ~count:4 [3; 4])
      refused
  ; reads "a page larger than the size asked for is refused"
      [page ~size:4097 ~count:1 1 partitions]
      refused
  ; reads "a page whose MIG mode differs from the first page's is refused"
      (second_page_differs (with_mig (mig_mode Enabled Disabled) partitions))
      refused
  ; reads "a page naming another card is refused"
      (second_page_differs
         {partitions with device= Some (device "0000:27:00.0")}
      )
      refused
  ; reads "a page whose case differs from the first page's is refused"
      (second_page_differs (with_probe (Known None_carved) partitions))
      refused
  ; reads "a page after the last is refused"
      (pages ~count:4 [1; 2; 3; 4; 5])
      refused
  ; reads "a page encoding not offered is refused"
      [page ~encoding:"x" ~count:1 1 partitions]
      refused
  ; reads "a lone reply without a page member reads as one page"
      [{partitions with page= None}]
      partitioned
  ; reads_output "a last page without its newline is read"
      (without_last_bytes 1 (lines (pages ~count:4 [1; 2; 3; 4])))
      partitioned
  ; reads_output "a reply cut inside a page, with exit 0, is refused"
      (without_last_bytes 100 (lines (pages ~count:4 [1; 2; 3; 4])))
      refused
  ; reads "a first page announcing pages that never come is refused"
      [page ~count:65 1 partitions]
      refused
  ]

let a_helper_that_cannot_run_is_not_started () =
  let configured = !Xapi_globs.gpu_partition_helper
  and not_executable = Filename.temp_file "gpu-partition-helper" "" in
  let run path =
    Xapi_globs.gpu_partition_helper := path ;
    Gpu_partition_helper.run_helper []
    |> Result.fold
         ~ok:(fun _ -> "ran")
         ~error:(fun k -> string_of_reason (Xapi k))
  in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      [run "/nonexistent/gpu-partition-helper"; run not_executable]
      |> Alcotest.(check (list string))
           "reasons of a missing and a non-executable helper"
           ["helper_absent"; "helper_not_executable"]
    )
    (fun () ->
      Xapi_globs.gpu_partition_helper := configured ;
      Sys.remove not_executable
    )

let nvidia_bdfs = ["0000:27:00.0"; "0000:3e:00.0"; card]

let partition_modes ~__context =
  List.map (fun self ->
      API.partition_mode_to_string (Db.PGPU.get_partition_mode ~__context ~self)
  )

(* A start-up read of three GPUs bound to nvidia, the middle one failing, beside
   GPUs it must not ask *)
let read_at_start () =
  let __context = Test_common.make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let gpu ?(host = host) ?(vendor_id = "10de") ?(driver_name = "nvidia") pci_id
      =
    let pCI =
      Test_common.make_pci ~__context ~host ~vendor_id ~driver_name ~pci_id ()
    in
    Test_common.make_pgpu ~__context ~host ~pCI ~partition_mode:`disabled ()
  in
  let cards = List.map gpu nvidia_bdfs
  and others =
    [
      gpu ~vendor_id:"1002" "0000:3b:00.0"
    ; gpu ~driver_name:"pciback" "0000:3c:00.0"
    ; gpu ~host:(Ref.make ()) "0000:3d:00.0"
    ]
  in
  let modes = partition_modes ~__context in
  (* The card in the middle has no answer, so its read raises *)
  let answers =
    [
      ( "--id=0000:27:00.0"
      , {
          (with_mig (mig_mode Disabled Enabled) none_carved) with
          device= Some (device "0000:27:00.0")
        }
      )
    ; ("--id=" ^ card, partitions)
    ]
  in
  let asked = ref [] and answered = ref 0 and written = ref [] in
  let run args =
    let read = List.filter (( <> ) "unknown") (modes cards) in
    written := (!answered, List.length read) :: !written ;
    asked := args :: !asked ;
    let answer =
      List.assoc (List.find (String.starts_with ~prefix:"--id=") args) answers
    in
    incr answered ;
    Ok {exit= Some 0; stdout= lines [answer]}
  in
  Gpu_partition_helper.(
    read_cards_at_start ~__context ~host
      ~read_card:(read_card ~run ~lock_args:no_lock_args)
  ) ;
  let asked = List.sort (List.compare String.compare) !asked in
  (List.rev !written, asked, modes cards, modes others)

let each_card_is_unknown_until_its_answer_is_written () =
  let written, _, _, _ = read_at_start () in
  Alcotest.(check (list (pair int int)))
    "before each read, the cards already answered are written"
    (List.map (fun (answered, _) -> (answered, answered)) written)
    written

let only_this_hosts_gpus_bound_to_nvidia_are_asked () =
  let _, asked, _, _ = read_at_start () in
  Alcotest.(check (list (list string)))
    "the helper's arguments for each card"
    (List.map
       (fun bdf ->
         [
           "mig"
         ; "--id=" ^ bdf
         ; "--geometry"
         ; "--schema=0"
         ; "--page-size=65536"
         ; "--page-encodings=identity"
         ; "--lock-timeout=0"
         ]
       )
       nvidia_bdfs
    )
    asked

let a_failed_read_costs_only_its_card () =
  let _, _, modes, _ = read_at_start () in
  Alcotest.(check (list string))
    "the cards' partition modes"
    ["enable_on_reboot"; "unknown"; "enabled"]
    modes

let this_hosts_gpus_not_asked_read_not_checked () =
  let _, _, _, modes = read_at_start () in
  Alcotest.(check (list string))
    "the other GPUs' partition modes"
    ["not_checked"; "not_checked"; "disabled"]
    modes

(* GPUs bound to nvidia, by default divided, not divided and pending division,
   read with every read answering [reply] *)
let read_failing ?(stored = [`enabled; `disabled; `enable_on_reboot]) reply =
  let __context = Test_common.make_test_database () in
  let host = Helpers.get_localhost ~__context in
  let gpu pci_id partition_mode =
    let pCI =
      Test_common.make_pci ~__context ~host ~vendor_id:"10de"
        ~driver_name:"nvidia" ~pci_id ()
    in
    Test_common.make_pgpu ~__context ~host ~pCI ~partition_mode ()
  in
  let cards = List.map2 gpu nvidia_bdfs stored and asked = ref 0 in
  let run _ = incr asked ; reply in
  Gpu_partition_helper.(
    read_cards_at_start ~__context ~host
      ~read_card:(read_card ~run ~lock_args:no_lock_args)
  ) ;
  (!asked, partition_modes ~__context cards)

let every_read_fails name reason asked =
  Alcotest.test_case name `Quick @@ fun () ->
  Alcotest.(check (pair int (list string)))
    "GPUs asked, partition modes"
    (asked, ["possibly_enabled"; "unknown"; "possibly_enabled"])
    (read_failing (Error reason))

(* The first GPU asked reads as divided; which GPU is first is not fixed *)
let a_reason_of_the_host_does_not_copy_the_first_gpus_mode () =
  let too_old = with_probe ~reason:(Known Nvml_too_old) (Known Unreadable) in
  let reply = Ok {exit= Some 69; stdout= lines [too_old partitions]} in
  let asked, modes =
    read_failing ~stored:[`disabled; `disabled; `disabled] reply
  in
  Alcotest.(check (pair int (list string)))
    "GPUs asked, partition modes in any order"
    (1, ["enabled"; "unknown"; "unknown"])
    (asked, List.sort String.compare modes)

let starting_up =
  Alcotest.
    [
      test_case "a helper that cannot run is not started" `Quick
        a_helper_that_cannot_run_is_not_started
    ; test_case "each card is unknown until its answer is written" `Quick
        each_card_is_unknown_until_its_answer_is_written
    ; test_case "only this host's GPUs bound to nvidia are asked" `Quick
        only_this_hosts_gpus_bound_to_nvidia_are_asked
    ; test_case "a failed read costs only its card" `Quick
        a_failed_read_costs_only_its_card
    ; test_case
        "this host's GPUs not asked read not_checked, another host's GPU is \
         untouched"
        `Quick this_hosts_gpus_not_asked_read_not_checked
    ; every_read_fails
        "helper absent, a reason of the host: only the first GPU asked, the \
         GPUs that may be divided read possibly_enabled"
        Helper_absent 1
    ; every_read_fails
        "helper timeout, a reason of one GPU: every GPU asked, the GPUs that \
         may be divided read possibly_enabled"
        Caller_timeout 3
    ; test_case
        "library too old, a reason of the host: the GPUs not asked do not take \
         the first GPU's mode"
        `Quick a_reason_of_the_host_does_not_copy_the_first_gpus_mode
    ]

let test = starting_up @ decoding
