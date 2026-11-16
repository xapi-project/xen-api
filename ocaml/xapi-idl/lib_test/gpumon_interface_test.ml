open Idl_test_common

module GenPath = struct let test_data_path = "gpu_gen" end

module OldPath = struct let test_data_path = "test_data/gpumon" end

module C = Gpumon_interface.RPC_API (GenTestData (GenPath) (TJsonrpc))
module T = Gpumon_interface.RPC_API (TestOldRpcs (OldPath) (TJsonrpc))
open Gpumon_interface.Partition_helper.V1_0

(* Replies of the GPU partition helper *)
let placement : Geometry.placement = {start= 0; size= 1}

let profile : Geometry.profile =
  {
    profile_id= 47
  ; name= "MIG 1g.24gb"
  ; name_status= None
  ; instance_count= 4
  ; remaining= Value 3
  ; placements= [placement]
  ; signature=
      Value
        {
          slice_count= Value 1
        ; multiprocessor_count= Value 46
        ; copy_engine_count= Value 1
        ; decoder_count= Value 1
        ; encoder_count= Value 0
        ; jpeg_count= Value 1
        ; ofa_count= Value 0
        ; memory_size_mb= Value 24064
        ; capabilities= Null
        }
  }

let mig_mode mode raw : Mig_mode.t =
  {
    status= Known Section_status.Ok
  ; kind= Null
  ; capable= Value true
  ; current= Value (Known mode)
  ; pending= Value (Known mode)
  ; current_raw= Value raw
  ; pending_raw= Value raw
  }

let vgpu status kind vgpus : Vgpu.t option =
  Some {status= Known status; kind; vgpus}

let probe result reason : Probe.t option = Some {result= Known result; reason}

let partitions : Probe.reply =
  {
    schema= "gpu-partition-helper"
  ; schema_version= (0, 0)
  ; page= Some {index= 1; count= 1; size= 65536; encoding= "identity"}
  ; implementation=
      {name= "gpu-partition-helper"; version= "1.0"; backend= Known Nvml}
  ; command= ["mig"; "--id=0000:c4:00.0"; "--geometry"]
  ; status= Known Envelope.Status.Ok
  ; error= Null
  ; library=
      Value
        {name= "libnvidia-ml.so.1"; version= "595.80"; driver_version= "595.80"}
  ; device=
      Value
        {
          bdf= "0000:c4:00.0"
        ; library_bus_id= "00000000:C4:00.0"
        ; pci_device_id= Value "0x2bb510de"
        ; pci_subsystem_id= Null
        ; name= "NVIDIA RTX PRO 6000 Blackwell Server Edition"
        ; name_status= None
        ; architecture= Known Device.Architecture.Blackwell
        ; architecture_raw= 10
        }
  ; lock=
      Value
        {
          path= "/run/gpu.lock"
        ; inode= Value 12
        ; waited_ms= 0
        ; mode= Some (Known Lock.Mode.Shared)
        ; holders= None
        }
  ; data=
      Probe.make_data
        ~mig_mode:(mig_mode Mig_mode.Mode.Enabled 1)
        ~geometry:
          {
            status= Known Section_status.Ok
          ; kind= Null
          ; gpu_instances=
              Value
                [
                  {
                    gi_id= 3
                  ; profile_id= 47
                  ; profile_name= "MIG 1g.24gb"
                  ; name_status= None
                  ; placement
                  ; compute_instances= [{ci_id= 0; profile_id= 0; placement}]
                  }
                ]
          ; profiles_status= Known Geometry.Profiles_status.Ok
          ; profiles= Value [profile]
          ; profile_index_bound= Value {gi= 18; ci= 8}
          ; type_profiles=
              Value
                [
                  {
                    type_id= 1561
                  ; gi_profile_id= Value 47
                  ; max_instance_per_gi= Some (Value 1)
                  }
                ]
          }
        ?vgpu:
          (vgpu Section_status.Ok Null
             (Value [{uuid= "<vgpu-uuid>"; gi_id= Value 3; type_id= 1561}])
          )
        ?probe:(probe Probe.Case.Partitions Null)
        ()
  }

let none_carved =
  {
    partitions with
    data=
      Probe.make_data
        ~mig_mode:(mig_mode Mig_mode.Mode.Disabled 0)
        ~geometry:
          {
            status= Known Section_status.Ok
          ; kind= Null
          ; gpu_instances= Null
          ; profiles_status= Known Geometry.Profiles_status.Mig_disabled
          ; profiles= Null
          ; profile_index_bound= Null
          ; type_profiles= Null
          }
        ?vgpu:(vgpu Section_status.Ok Null (Value []))
        ?probe:(probe Probe.Case.None_carved Null)
        ()
  }

let library_call : Envelope.library_call =
  {
    function_= Value "nvmlDeviceGetActiveVgpus"
  ; code= Value 999
  ; name= Value "NVML_ERROR_UNKNOWN"
  ; text= Null
  }

let reply_error kind message library_call : Envelope.error =
  {kind= Known kind; message; library_call; symbol= Null; schema_majors= None}

let vgpus_unreadable =
  {
    partitions with
    status= Known Envelope.Status.Partial
  ; error=
      Value
        (reply_error Kind.Library_error "vGPUs unreadable" (Value library_call))
  ; data=
      {
        partitions.data with
        vgpu= vgpu Section_status.Error (Value (Known Kind.Library_error)) Null
      }
  }

let lock_timeout =
  {
    partitions with
    status= Known Envelope.Status.Error
  ; error= Value (reply_error Kind.Lock_timeout "host lock busy" Null)
  ; device= Null
  ; lock=
      Value
        {
          path= "/run/gpu.lock"
        ; inode= Null
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
  ; data=
      Probe.make_data
        ?probe:(probe Probe.Case.Unreadable (Value (Known Kind.Lock_timeout)))
        ()
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
  ; command= ["--version"]
  ; status= Known Envelope.Status.Ok
  ; error= Null
  ; library= partitions.library
  ; device= Null
  ; lock= Null
  ; data=
      Version.make_data
        ~schema_versions:[(0, 0)]
        ~verbs ~page_encodings:["identity"] ()
  }

(* A --version usage error: it has no data, which the helper writes as {} *)
let usage_error =
  {
    (version []) with
    status= Known Envelope.Status.Error
  ; error= Value (reply_error Kind.Usage "--schema=<major> is mandatory" Null)
  }

let creatable_type type_id = Creatable.make_vgpu_type ~type_id ()

(* Partitions' GPU instance 3, which can create two vGPU types *)
let entry_read =
  Creatable.make_entry ~gi_id:3 ~status:(Known Section_status.Ok) ~kind:Null
    ~library_call:Null
    ~vgpu_types:(Value [creatable_type 1561; creatable_type 1562])
    ()

(* A GPU instance whose read failed in the helper itself, with no call *)
let entry_internal =
  Creatable.make_entry ~gi_id:4 ~status:(Known Section_status.Error)
    ~kind:(Value (Known Kind.Internal)) ~library_call:Null ~vgpu_types:Null ()

let creatable_read entries =
  Creatable.make ~status:(Known Section_status.Ok) ~kind:Null
    ~gpu_instances:(Value entries) ()

let creatable_not_read status kind =
  Creatable.make ~status:(Known status) ~kind ~gpu_instances:Null ()

(* Partitions with a creatable section *)
let with_creatable section =
  {partitions with data= {partitions.data with creatable= Some section}}

let one_read = with_creatable (creatable_read [entry_read])

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
  | [k], Rpc.Dict ms when Option.is_some value || List.mem_assoc k ms ->
      Rpc.Dict
        (List.remove_assoc k ms
        @ Option.to_list (Option.map (fun v -> (k, v)) value)
        )
  | k :: path, Rpc.Dict ms when List.mem_assoc k ms ->
      Rpc.Dict
        ((k, edit (List.assoc k ms) (path, value)) :: List.remove_assoc k ms)
  | _ ->
      Alcotest.failf "edit: no member at %s" (String.concat "." path)

(* edit indexes no list, so a row that edits an item writes the whole list
   again: its first item edited, then the others *)
let first_edited typ path (first, others) edits =
  let item = Rpcmarshal.marshal typ in
  ( path
  , Some
      (Rpc.Enum (List.fold_left edit (item first) edits :: List.map item others))
  )

(* The one entry of a creatable section, edited *)
let entry_edited entry =
  first_edited Creatable.typ_of_entry
    ["data"; "creatable"; "gpu_instances"]
    (entry, [])

(* The first vGPU type of entry_read, edited *)
let type_edited =
  first_edited Creatable.typ_of_vgpu_type ["vgpu_types"]
    (creatable_type 1561, [creatable_type 1562])

(* What the reader makes of a written reply after the edits. Each case also
   checks whether Envelope.Core reads it. *)
type outcome =
  (* Written back as edited *)
  | Kept
  (* Written back without the edits *)
  | Dropped
  (* Data whose members are all unknown: data with no sections, written {} *)
  | No_sections
  (* Refused, while Envelope.Core reads it *)
  | Core_only
  (* Refused, and Envelope.Core refuses it too *)
  | Refused

let case typ (name, reply, edits, outcome) =
  let write = Rpcmarshal.marshal typ in
  let input = List.fold_left edit (write reply) in
  let test () =
    let expected =
      match outcome with
      | Kept ->
          (json (input edits), true)
      | Dropped ->
          (json (input []), true)
      | No_sections ->
          (json (input (edits @ [(["data"], Some (Rpc.Dict []))])), true)
      | Core_only ->
          ("refused", true)
      | Refused ->
          ("refused", false)
    in
    let rpc = input edits in
    ( Rpcmarshal.unmarshal typ rpc
      |> Result.fold ~error:(fun _ -> "refused") ~ok:(fun r -> json (write r))
    , Rpcmarshal.unmarshal Envelope.Core.typ_of rpc |> Result.is_ok
    )
    |> Alcotest.(check (pair string bool)) name expected
  in
  (name, `Quick, test)

let major_2 = (["schema_version"], Some (Rpc.Enum [Rpc.Int 2L; Rpc.Int 0L]))

let cases =
  [
    ("a reply with partitions reads back as written", partitions, [], Kept)
  ; ("a reply with none carved reads back as written", none_carved, [], Kept)
  ; ("a partial reply reads back as written", vgpus_unreadable, [], Kept)
  ; ("a lock timeout reply reads back as written", lock_timeout, [], Kept)
  ; ("a reply without page reads back", {partitions with page= None}, [], Kept)
  ; ( "device absent: only Core reads"
    , partitions
    , [(["device"], None)]
    , Core_only
    )
  ; ( "an integer as a string: refused"
    , partitions
    , [(["page"; "index"], Some (Rpc.String "1"))]
    , Refused
    )
  ; ( "an integer beyond OCaml's int: only Core reads"
    , partitions
    , [(["data"; "mig_mode"; "current_raw"], Some (Rpc.Int Int64.max_int))]
    , Core_only
    )
  ; ( "command absent: only Core reads"
    , partitions
    , [(["command"], None)]
    , Core_only
    )
  ; ( "geometry kind absent: only Core reads"
    , partitions
    , [(["data"; "geometry"; "kind"], None)]
    , Core_only
    )
  ; ( "vgpus not a list: only Core reads"
    , partitions
    , [(["data"; "vgpu"; "vgpus"], Some (Rpc.Int 3L))]
    , Core_only
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
  ; ( "major 2 data with only unknown members: data with no sections"
    , partitions
    , [major_2; (["data"], Some (Rpc.Dict [("x_future", Rpc.Int 1L)]))]
    , No_sections
    )
  ; ( "major 2 without data: only Core reads"
    , partitions
    , [major_2; (["data"], None)]
    , Core_only
    )
  ; ( "a creatable section not attempted reads back as written"
    , with_creatable (creatable_not_read Section_status.Not_attempted Null)
    , [(["data"; "creatable"; "gpu_instances"], Some Rpc.Null)]
    , Kept
    )
  ; ( "a creatable entry without gi_id: only Core reads the reply"
    , one_read
    , [entry_edited entry_read [(["gi_id"], None)]]
    , Core_only
    )
  ; ( "a creatable type id beyond an OCaml int: only Core reads the reply"
    , one_read
    , [
        entry_edited entry_read
          [type_edited [(["type_id"], Some (Rpc.Int Int64.max_int))]]
      ]
    , Core_only
    )
  ]

let version_data sections =
  Version.make_data
    ~schema_versions:[(0, 0)]
    ~verbs:[] ~page_encodings:["identity"] ?sections ()

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
  ; ( "a usage error with data {}: only Core reads"
    , usage_error
    , [(["data"], Some (Rpc.Dict []))]
    , Core_only
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

(* Whether a string reads as a Known value, which a round trip cannot see:
   only the exact lower-case spelling of a known value does, and a kind that is
   not a probe's reads as Unknown. *)
let reads typ value ~known () =
  ok (Rpcmarshal.unmarshal typ (Rpc.String value))
  |> ( function Member.Known _ -> true | Member.Unknown _ -> false )
  |> Alcotest.(check bool) value known

(* What a JSON text reads as, compared with the value expected, as written *)
let read typ wire expected () =
  let written v = json (Rpcmarshal.marshal typ v) in
  Alcotest.(check string)
    wire (written expected)
    (written (ok (Rpcmarshal.unmarshal typ (Jsonrpc.of_string wire))))

(* What Envelope.Core reads from a reply written with the whole type, against
   the members it shares with the reply, both as written *)
let core_reads (reply : Probe.reply) =
  let error (e : Envelope.error) : Envelope.Core.error =
    {kind= e.kind; message= e.message; schema_majors= e.schema_majors}
  in
  let shared : Envelope.Core.t =
    {
      schema= reply.schema
    ; schema_version= reply.schema_version
    ; page= reply.page
    ; status= reply.status
    ; error= (match reply.error with Null -> Null | Value e -> Value (error e))
    }
  in
  read Envelope.Core.typ_of
    (Jsonrpc.to_string (Rpcmarshal.marshal Probe.typ_of_reply reply))
    shared

let type_profile max_instance_per_gi : Geometry.type_profile =
  {type_id= 1561; gi_profile_id= Value 47; max_instance_per_gi}

(* Each sample is written with V1_0's types, so it must carry V1_0's version *)
let carries_v1_0 name carried =
  ( name ^ " carries V1_0's schema_version"
  , `Quick
  , fun () -> Alcotest.(check (pair int int)) name schema_version carried
  )

let samples =
  [
    carries_v1_0 "partitions" partitions.schema_version
  ; carries_v1_0 "none carved" none_carved.schema_version
  ; carries_v1_0 "a partial reply" vgpus_unreadable.schema_version
  ; carries_v1_0 "a lock timeout" lock_timeout.schema_version
  ; carries_v1_0 "--version" (version []).schema_version
  ; carries_v1_0 "a usage error" usage_error.schema_version
  ]

(* The names of the members a value is written with, sorted *)
let members_written = function
  | Rpc.Dict members ->
      List.sort String.compare (List.map fst members)
  | rpc ->
      Alcotest.failf "written as %s, not as an object" (Rpc.to_string rpc)

let creatable_not_written_by_default () =
  Alcotest.(check (list string))
    "members written" []
    (members_written (Rpcmarshal.marshal Probe.typ_of_data (Probe.make_data ())))

(* The creatable section exactly as it goes on the wire, written by hand. The
   helper that writes this reply is another program, so these member names
   and nulls are a protocol that the types cannot check: a renamed field, a
   [@key], or a missing list written as [] instead of null still compiles and
   still reads back unchanged, and only this literal fails then. Members are
   compared sorted by name, so their order does not matter *)
let creatable_on_the_wire () =
  Alcotest.(check string)
    "creatable section"
    (json
       (Jsonrpc.of_string
          {|{"status": "ok", "kind": null,
             "gpu_instances": [
               {"gi_id": 3, "status": "ok", "kind": null,
                "library_call": null,
                "vgpu_types": [{"type_id": 1561}, {"type_id": 1562}]},
               {"gi_id": 4, "status": "error", "kind": "internal",
                "library_call": null, "vgpu_types": null}]}|}
       )
    )
    (json
       (Rpcmarshal.marshal Creatable.typ_of
          (creatable_read [entry_read; entry_internal])
       )
    )

let sections_not_written_by_default () =
  Alcotest.(check (list string))
    "members written"
    ["page_encodings"; "schema_versions"; "verbs"]
    (members_written
       (Rpcmarshal.marshal Version.typ_of_data (version_data None))
    )

(* The newest check, written once and never edited when a snapshot is
   added: it follows every section of 1.0 into the newest snapshot. It
   names the newest snapshot on purpose: it is a check, not a view. A
   snapshot that adds sections adds the same check for them *)
module Newest = Gpumon_interface.Partition_helper

(* Every section of 1.0, read from the typ of its sections, so that a
   section added to 1.0 is in the newest check with no edit here *)
let sections_of_1_0 () =
  match Sections.typ_of_known with
  | Rpc.Types.Variant {variants; _} ->
      List.map
        (fun (Rpc.Types.BoxedTag {Rpc.Types.tname; _}) ->
          ok (Rpcmarshal.unmarshal Sections.typ_of_known (Rpc.String tname))
        )
        variants
  | Rpc.Types.Basic _ | Rpc.Types.Struct _ | Rpc.Types.Abstract _ ->
      Alcotest.fail "the typ of the sections of 1.0 is not a variant"

(* Checked when this compiles: the newest Version lists sections in the
   newest snapshot's names *)
let _version_lists_newest_sections (sections : Newest.Sections.t list option)
    (data : Newest.Version.data) : Newest.Version.data =
  {data with sections}

let newest_name section =
  (Newest.Sections.token (Newest.Sections.of_v1_0 section) :> string)

let newest_reads_1_0_names () =
  List.iter
    (fun section ->
      match Newest.Sections.of_token (Sections.token section) with
      | Newest.Member.Known read ->
          Alcotest.(check string)
            "section" (newest_name section)
            (Newest.Sections.token read :> string)
      | Newest.Member.Unknown name ->
          Alcotest.failf "read back as an unknown section: %s" name
    )
    (sections_of_1_0 ())

let values =
  [
    ("enabled is a known mode", reads Mig_mode.Mode.typ_of "enabled" ~known:true)
  ; ( "Enabled is an unknown mode"
    , reads Mig_mode.Mode.typ_of "Enabled" ~known:false
    )
  ; ( "caller_timeout, a reader's own reason, is an unknown kind"
    , reads Kind.typ_of "caller_timeout" ~known:false
    )
  ; ( "insufficient_resources, a later command's kind, is an unknown kind"
    , reads Kind.typ_of "insufficient_resources" ~known:false
    )
  ; ( "a library call is written with its wire names and null"
    , library_call_on_the_wire
    )
  ; ( "max_instance_per_gi left out reads None"
    , read Geometry.typ_of_type_profile {|{"type_id":1561,"gi_profile_id":47}|}
        (type_profile None)
    )
  ; ( "max_instance_per_gi null reads Some Null"
    , read Geometry.typ_of_type_profile
        {|{"type_id":1561,"gi_profile_id":47,"max_instance_per_gi":null}|}
        (type_profile (Some Null))
    )
  ; ( "max_instance_per_gi 1 reads Some (Value 1)"
    , read Geometry.typ_of_type_profile
        {|{"type_id":1561,"gi_profile_id":47,"max_instance_per_gi":1}|}
        (type_profile (Some (Value 1)))
    )
  ; ("Envelope.Core reads the page a reply carries", core_reads partitions)
  ; ( "Envelope.Core reads the schema majors an error carries"
    , core_reads
        {
          lock_timeout with
          page= None
        ; error=
            Value
              {
                (reply_error Kind.Schema_unsupported "major 2 only" Null) with
                schema_majors= Some [2]
              }
        }
    )
  ; ( "a maximum duration that is not an integer reads None"
    , read Version.typ_of_verb
        {|{"command":"--version","status":"supported","max_duration_ms":3.5}|}
        (verb "--version")
    )
  ; ( "probe data built without the creatable argument writes no creatable \
       member"
    , creatable_not_written_by_default
    )
  ; ( "a creatable section is written with its wire names and nulls"
    , creatable_on_the_wire
    )
  ; ( "a --version reply's sections read back as written"
    , read Version.typ_of_data
        {|{"schema_versions": [[0, 0]], "verbs": [],
           "page_encodings": ["identity"], "sections": ["creatable"]}|}
        (version_data (Some [Known Sections.Creatable]))
    )
  ; ( "--version data built without the sections argument has none"
    , sections_not_written_by_default
    )
  ; ( "the newest snapshot reads every section name of 1.0 back as that section"
    , newest_reads_1_0_names
    )
  ]

let tests =
  !C.implementation
  @ !T.implementation
  @ List.map (case Probe.typ_of_reply) cases
  @ List.map (case Version.typ_of_reply) version_cases
  @ samples
  @ List.map (fun (name, test) -> (name, `Quick, test)) values
