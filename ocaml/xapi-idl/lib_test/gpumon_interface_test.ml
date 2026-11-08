open Idl_test_common

module GenPath = struct let test_data_path = "gpu_gen" end

module OldPath = struct let test_data_path = "test_data/gpumon" end

module C = Gpumon_interface.RPC_API (GenTestData (GenPath) (TJsonrpc))
module T = Gpumon_interface.RPC_API (TestOldRpcs (OldPath) (TJsonrpc))
open Gpumon_interface.Partition_helper

(* Replies of the GPU partition helper *)
let placement : Geometry.placement = {start= 0; size= 1}

let profile : Geometry.profile =
  {
    profile_id= 47
  ; name= "MIG 1g.24gb"
  ; name_status= None
  ; instance_count= 4
  ; remaining= Some 3
  ; placements= [placement]
  ; signature=
      Some
        {
          slice_count= Some 1
        ; multiprocessor_count= Some 46
        ; copy_engine_count= Some 1
        ; decoder_count= Some 1
        ; encoder_count= Some 0
        ; jpeg_count= Some 1
        ; ofa_count= Some 0
        ; memory_size_mb= Some 24064
        ; capabilities= None
        }
  }

let mig_mode mode raw : Mig_mode.t =
  {
    status= Known Section_status.Ok
  ; kind= None
  ; capable= Some true
  ; current= Some (Known mode)
  ; pending= Some (Known mode)
  ; current_raw= Some raw
  ; pending_raw= Some raw
  }

let vgpu status kind vgpus : Vgpu.lenient option =
  Some (Ok {status= Known status; kind; vgpus})

let probe result reason : Probe.t option = Some {result= Known result; reason}

let partitions : Probe.reply =
  {
    schema= "gpu-partition-helper"
  ; schema_version= (0, 0)
  ; page= Some {index= 1; count= 1; size= 65536; encoding= "identity"}
  ; implementation=
      Ok {name= "gpu-partition-helper"; version= "1.0"; backend= Known Nvml}
  ; command= Ok ["mig"; "--id=0000:c4:00.0"; "--geometry"]
  ; status= Known Envelope.Status.Ok
  ; error= None
  ; library=
      Ok
        (Some
           {
             name= "libnvidia-ml.so.1"
           ; version= "595.80"
           ; driver_version= "595.80"
           }
        )
  ; device=
      Some
        {
          bdf= "0000:c4:00.0"
        ; library_bus_id= Ok "00000000:C4:00.0"
        ; pci_device_id= Ok (Some "0x2bb510de")
        ; pci_subsystem_id= Ok None
        ; name= Ok "NVIDIA RTX PRO 6000 Blackwell Server Edition"
        ; name_status= None
        ; architecture= Ok (Known Device.Architecture.Blackwell)
        ; architecture_raw= Ok 10
        }
  ; lock=
      Ok
        (Some
           {
             path= "/run/gpu.lock"
           ; inode= Some 12
           ; waited_ms= 0
           ; mode= Some (Known Lock.Mode.Shared)
           ; holders= None
           }
        )
  ; data=
      Ok
        {
          mig_mode= Some (mig_mode Mig_mode.Mode.Enabled 1)
        ; geometry=
            Some
              {
                status= Known Section_status.Ok
              ; kind= Ok None
              ; gpu_instances=
                  Some
                    [
                      {
                        gi_id= 3
                      ; profile_id= 47
                      ; profile_name= "MIG 1g.24gb"
                      ; name_status= None
                      ; placement
                      ; compute_instances=
                          [{ci_id= 0; profile_id= 0; placement}]
                      }
                    ]
              ; profiles_status= Known Geometry.Profiles_status.Ok
              ; profiles= Ok (Some [profile])
              ; profile_index_bound= Ok (Some {gi= 18; ci= 8})
              ; type_profiles=
                  Ok
                    (Some
                       [
                         {
                           type_id= 1561
                         ; gi_profile_id= Some 47
                         ; max_instance_per_gi= Some (Some 1)
                         }
                       ]
                    )
              }
        ; vgpu=
            vgpu Section_status.Ok None
              (Some [{uuid= "<vgpu-uuid>"; gi_id= Some 3; type_id= 1561}])
        ; probe= probe Probe.Case.Partitions None
        }
  }

let none_carved =
  {
    partitions with
    data=
      Ok
        {
          mig_mode= Some (mig_mode Mig_mode.Mode.Disabled 0)
        ; geometry=
            Some
              {
                status= Known Section_status.Ok
              ; kind= Ok None
              ; gpu_instances= None
              ; profiles_status= Known Geometry.Profiles_status.Mig_disabled
              ; profiles= Ok None
              ; profile_index_bound= Ok None
              ; type_profiles= Ok None
              }
        ; vgpu= vgpu Section_status.Ok None (Some [])
        ; probe= probe Probe.Case.None_carved None
        }
  }

let library_call : Envelope.library_call =
  {
    function_= Some "nvmlDeviceGetActiveVgpus"
  ; code= Some 999
  ; name= Some "NVML_ERROR_UNKNOWN"
  ; text= None
  }

let reply_error kind message library_call : Envelope.error =
  {
    kind= Known kind
  ; message
  ; library_call= Ok library_call
  ; symbol= Ok None
  ; schema_majors= None
  }

let vgpus_unreadable =
  {
    partitions with
    status= Known Envelope.Status.Partial
  ; error=
      Some
        (reply_error Kind.Library_error "vGPUs unreadable" (Some library_call))
  ; data=
      Result.map
        (fun d ->
          {
            d with
            Probe.vgpu=
              vgpu Section_status.Error (Some (Known Kind.Library_error)) None
          }
        )
        partitions.data
  }

let lock_timeout =
  {
    partitions with
    status= Known Envelope.Status.Error
  ; error= Some (reply_error Kind.Lock_timeout "host lock busy" None)
  ; device= None
  ; lock=
      Ok
        (Some
           {
             path= "/run/gpu.lock"
           ; inode= None
           ; waited_ms= 5000
           ; mode= None
           ; holders=
               Some
                 [
                   {
                     pid= 4242
                   ; name= "<process name>"
                   ; state= "D"
                   ; mode= Known Lock.Mode.Exclusive
                   ; stuck_threads= Some [4243]
                   }
                 ]
           }
        )
  ; data=
      Ok
        {
          mig_mode= None
        ; geometry= None
        ; vgpu= None
        ; probe= probe Probe.Case.Unreadable (Some (Known Kind.Lock_timeout))
        }
  }

let verb ?(status = Member.Known Version.Verb_status.Supported) ?max_duration_ms
    command : Version.verb =
  {command; status; max_duration_ms}

let version verbs : Version.reply =
  {
    schema= "gpu-partition-helper"
  ; schema_version= (0, 0)
  ; page= partitions.page
  ; implementation= partitions.implementation
  ; command= Ok ["--version"]
  ; status= Known Envelope.Status.Ok
  ; error= None
  ; library= partitions.library
  ; device= None
  ; lock= Ok None
  ; data= Ok {schema_versions= [(0, 0)]; verbs; page_encodings= ["identity"]}
  }

(* The canonical rpc format: members sorted by name *)
let rec canonical = function
  | Rpc.Dict members ->
      Rpc.Dict
        (List.map (fun (k, v) -> (k, canonical v)) members
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
        )
  | Rpc.Enum items ->
      Rpc.Enum (List.map canonical items)
  | rpc ->
      rpc

let json rpc = Jsonrpc.to_string (canonical rpc)

(* Sets the member at a path, or removes it *)
let rec edit rpc (path, value) =
  match (path, rpc) with
  | [k], Rpc.Dict ms ->
      Rpc.Dict
        (List.remove_assoc k ms
        @ Option.to_list (Option.map (fun v -> (k, v)) value)
        )
  | k :: path, Rpc.Dict ms ->
      Rpc.Dict
        (List.map
           (fun (k', m) ->
             ( k'
             , if k = k' then
                 edit m (path, value)
               else
                 m
             )
           )
           ms
        )
  | _ ->
      rpc

(* What the reader makes of a written reply after the edits: it refuses it,
   or writes it back as edited, without the edits, or with one member lost: a
   fault climbs to the nearest lenient member at or above it, which reads as an
   Error and is written {}; with none, the reply is refused. *)
type outcome = Refused | Kept | Dropped | Lost of string list

let case typ (name, reply, edits, outcome) =
  let write = Rpcmarshal.marshal typ in
  let input = List.fold_left edit (write reply) in
  let expected =
    match outcome with
    | Refused ->
        "refused"
    | Kept ->
        json (input edits)
    | Dropped ->
        json (input [])
    | Lost path ->
        json (input (edits @ [(path, Some (Rpc.Dict []))]))
  in
  let test () =
    Rpcmarshal.unmarshal typ (input edits)
    |> Result.fold ~error:(fun _ -> "refused") ~ok:(fun r -> json (write r))
    |> Alcotest.(check string) name expected
  in
  (name, `Quick, test)

let cases =
  [
    ("a reply with partitions reads back as written", partitions, [], Kept)
  ; ("a reply with none carved reads back as written", none_carved, [], Kept)
  ; ("a partial reply reads back as written", vgpus_unreadable, [], Kept)
  ; ("a lock timeout reply reads back as written", lock_timeout, [], Kept)
  ; ("a reply without page reads back", {partitions with page= None}, [], Kept)
  ; ("device absent: refused", partitions, [(["device"], None)], Refused)
  ; ( "an integer as a string: refused"
    , partitions
    , [(["page"; "index"], Some (Rpc.String "1"))]
    , Refused
    )
  ; ( "an integer beyond OCaml's int: data is lost"
    , partitions
    , [(["data"; "mig_mode"; "current_raw"], Some (Rpc.Int Int64.max_int))]
    , Lost ["data"]
    )
  ; ( "command absent: only command is lost"
    , partitions
    , [(["command"], None)]
    , Lost ["command"]
    )
  ; ( "geometry kind absent: only it is lost"
    , partitions
    , [(["data"; "geometry"; "kind"], None)]
    , Lost ["data"; "geometry"; "kind"]
    )
  ; ( "vgpus not a list: only the vgpu section is lost"
    , partitions
    , [(["data"; "vgpu"; "vgpus"], Some (Rpc.Int 3L))]
    , Lost ["data"; "vgpu"]
    )
  ; ( "a newer helper's probe result is kept"
    , partitions
    , [(["data"; "probe"; "result"], Some (Rpc.String "x_future"))]
    , Kept
    )
  ; ( "a newer helper's member is dropped"
    , partitions
    , [(["data"; "x_future"], Some (Rpc.Int 1L))]
    , Dropped
    )
  ; ( "major 2 data: read without its members"
    , partitions
    , [
        (["schema_version"], Some (Rpc.Enum [Rpc.Int 2L; Rpc.Int 0L]))
      ; (["data"], Some (Rpc.Dict [("x_future", Rpc.Int 1L)]))
      ]
    , Lost ["data"]
    )
  ]

let version_cases =
  [
    ( "a --version reply reads back as written"
    , version [verb "--version"; verb "mig --geometry"]
    , []
    , Kept
    )
  ; ( "a command's maximum duration reads back"
    , version [verb ~max_duration_ms:3000 "mig --geometry"]
    , []
    , Kept
    )
  ; ( "a newer helper's command status is kept"
    , version [verb ~status:(Unknown "retired") "mig --geometry"]
    , []
    , Kept
    )
  ]

(* The wire names come from the field names, but for the one [@key] *)
let library_call_on_the_wire () =
  Alcotest.(check string)
    "library call"
    (json
       (Jsonrpc.of_string
          {|{"function":"nvmlDeviceGetActiveVgpus","code":999,"name":"NVML_ERROR_UNKNOWN","text":null}|}
       )
    )
    (json (Rpcmarshal.marshal Envelope.typ_of_library_call library_call))

let ok r = Result.fold r ~ok:Fun.id ~error:(fun (`Msg m) -> Alcotest.fail m)

(* What a round trip cannot see: only the exact lower-case spelling of a known
   value reads as Known, and a kind that is not a probe's reads as Unknown. *)
let reads typ value ~known () =
  ok (Rpcmarshal.unmarshal typ (Rpc.String value))
  |> ( function Member.Known _ -> true | Member.Unknown _ -> false )
  |> Alcotest.(check bool) value known

let values =
  [
    ("enabled is a known mode", reads Mig_mode.Mode.typ_of "enabled" ~known:true)
  ; ( "Enabled is an unknown mode"
    , reads Mig_mode.Mode.typ_of "Enabled" ~known:false
    )
  ; ( "caller_timeout, a caller's reason, is an unknown kind"
    , reads Kind.typ_of "caller_timeout" ~known:false
    )
  ; ( "insufficient_resources, a write's kind, is an unknown kind"
    , reads Kind.typ_of "insufficient_resources" ~known:false
    )
  ; ( "a library call is written with its wire names and null"
    , library_call_on_the_wire
    )
  ]

let tests =
  !C.implementation
  @ !T.implementation
  @ List.map (case Probe.typ_of_reply) cases
  @ List.map (case Version.typ_of_reply) version_cases
  @ List.map (fun (name, test) -> (name, `Quick, test)) values
