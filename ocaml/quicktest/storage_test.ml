
type sr_info =
  { sr: API.ref_SR
  ; allowed_operations: API.storage_operations_set
  ; capabilities: string list
  }

let get_sr_info rpc session_id sr =
  let get_sr_features session_id sR =
    (* Even though the SM backend may expose a VDI_CREATE capability attempts
       to actually create a VDI will fail in (eg) the tools SR and any that
       happen to be R/O NFS exports *)
    let avoid_vdi_create session_id sr =
      let other_config = Client.Client.SR.get_other_config rpc session_id sr in
      let is_tools_sr = Client.Client.SR.get_is_tools_sr rpc session_id sr in
      let is_iso_sr = Client.Client.SR.get_content_type rpc session_id sr = "iso" in
      let special_key = "quicktest-no-VDI_CREATE" in
      let is_marked = List.mem_assoc special_key other_config && List.assoc special_key other_config = "true" in
      is_tools_sr || is_iso_sr || is_marked
    in

    let caps =
      let ty = Client.Client.SR.get_type rpc session_id sR in
      let sm = Client.Client.SM.get_all_records rpc session_id in
      match List.filter (fun (_, r) -> r.API.sM_type = ty) sm with
      | [] -> Alcotest.failf "Could not find SM plugin for SR type %s" ty
      | [ _, plugin ] -> plugin.API.sM_capabilities
      | _ :: _ -> Alcotest.failf "Multiple SM plugins found for SR type %s" ty
    in
    let avoid_vdi_create = avoid_vdi_create session_id sR in
    let caps =
      if avoid_vdi_create then
        List.filter
          (fun cap -> not (List.mem cap Sr_capabilities.[ vdi_create; vdi_delete ]))
          caps
      else
        caps
    in
    let ops = Client.Client.SR.get_allowed_operations ~session_id ~rpc:rpc ~self:sR in
    let ops =
      if avoid_vdi_create then
        List.filter
          (fun cap -> not (List.mem cap [`vdi_create; `vdi_destroy]))
          ops
      else
        ops
    in
    (ops, caps)
  in

  let allowed_operations, capabilities = get_sr_features session_id sr in
  { sr
  ; allowed_operations
  ; capabilities
  }

module VDI = struct

  (* This naming is used to identify VDIs to destroy later on *)
  let test_vdi_name_label = "quicktest-vdi_16cd61e7-3f42-4ea7-a1d8-5e9d3f64f25d"
  let test_vdi_name_description = "VDI for storage quicktest"

  let make rpc session_id ?(virtual_size=4194304L) sR =
    Client.Client.VDI.create
      ~sR
      ~session_id
      ~rpc
      ~name_label:test_vdi_name_label
      ~name_description:test_vdi_name_description
      ~_type:`user
      ~sharable:false
      ~read_only:false
      ~virtual_size
      ~xenstore_data:[]
      ~other_config:[]
      ~tags:[]
      ~sm_config:[]

  let with_destroyed session_id self f =
    let rpc = !Quicktest_common.rpc in
    Xapi_stdext_pervasives.Pervasiveext.finally
      f
      (fun () -> Client.Client.VDI.destroy ~rpc ~session_id ~self)

  let with_new session_id ?(virtual_size=4194304L) sr f =
    let rpc = !Quicktest_common.rpc in
    let self = make rpc session_id ~virtual_size sr in
    with_destroyed session_id self (fun () -> f self)

  let with_any session_id sr_info f =
    let rpc = !Quicktest_common.rpc in
    if List.mem `vdi_create sr_info.allowed_operations then begin
      with_new session_id sr_info.sr f
    end else begin
      let self = Client.Client.SR.get_VDIs ~rpc ~session_id ~self:sr_info.sr |> List.hd in
      f self
    end

  let check_fields = Quicktest_common.compare_fields "VDI"

  let test_update session_id self =
    let rpc = !Quicktest_common.rpc in
    let original_vdi = Client.Client.VDI.get_record ~rpc ~session_id ~self in
    Client.Client.VDI.update ~rpc ~session_id ~vdi:self;
    let new_vdi = Client.Client.VDI.get_record ~rpc ~session_id ~self in
    let expected =
      [ `Same, "cbt_enabled", (fun vdi -> vdi.API.vDI_cbt_enabled |> string_of_bool)
      ; `Same, "is_a_snapshot", (fun vdi -> vdi.API.vDI_is_a_snapshot |> string_of_bool)
      ; `Same, "location", (fun vdi -> vdi.API.vDI_location)
      ; `Same, "managed", (fun vdi -> vdi.API.vDI_managed |> string_of_bool)
      ; `Same, "name_description", (fun vdi -> vdi.API.vDI_name_description)
      ; `Same, "name_label", (fun vdi -> vdi.API.vDI_name_label)
      ; `Same, "snapshot_of", (fun vdi -> vdi.API.vDI_snapshot_of |> API.Ref.string_of)
      ; `Same, "snapshot_time", (fun vdi -> vdi.API.vDI_snapshot_time |> Xapi_stdext_date.Date.to_string)
      ; `Same, "virtual_size", (fun vdi -> vdi.API.vDI_location)
      ]
    in
    check_fields expected original_vdi new_vdi
end

module SR = struct
  let check_fields = Quicktest_common.compare_fields "SR"

  let test_update session_id self =
    let rpc = !Quicktest_common.rpc in
    let original_sr = Client.Client.SR.get_record ~rpc ~session_id ~self in
    Client.Client.SR.update ~rpc ~session_id ~sr:self;
    let new_sr = Client.Client.SR.get_record ~rpc ~session_id ~self in
    let expected =
      [ `Same, "name_description", (fun sr -> sr.API.sR_name_description)
      ; `Same, "name_label", (fun sr -> sr.API.sR_name_label)
      ]
    in
    check_fields expected original_sr new_sr
end

module Sr_filter = struct
  type t = (Rpc.call -> Rpc.response) -> API.ref_session -> sr_info list -> sr_info list

  let all _rpc _session_id sr_infos = sr_infos

  let random _rpc _session_id sr_infos =
    let index = Random.int @@ List.length sr_infos in
    [List.nth sr_infos index]

  let not_iso rpc session_id =
    List.filter (fun sr_info -> Client.Client.SR.get_content_type rpc session_id sr_info.sr <> "iso")

  let only_sr sr rpc session_id _sr_infos = [get_sr_info rpc session_id sr]

  let default_sr rpc session_id _sr_infos =
    let pool = Quicktest_common.get_pool session_id in
    let default = Client.Client.Pool.get_default_SR ~rpc ~session_id ~self:pool in
    [get_sr_info rpc session_id default]

  let with_smallest_disk_size rpc session_id sr_infos =
    (** Return the size of the smallest disk we can create in this SR. This wouldn't be necessary
        except the Netapp SR breaks with convention and returns errors rather than rounding up
        for small disks *)
    let find_smallest_disk_size session_id sr =
      let sizes = Sizes.[ 0L; 1L; 1L ** mib; 2L ** mib; 4L ** mib ] in
      let try_one size =
        try
          VDI.with_new session_id ~virtual_size:size sr (fun _vdi ->
              Some size)
        with _ -> None
      in
      let find_smallest =
        List.fold_left
          (fun state size -> if state = None then try_one size else state)
          None
          sizes
      in
      find_smallest
    in
    let sr_with_smallest_disk_size session_id srs =
      let minimum_sizes =
        srs
        |> List.map
          (fun sr_info ->
             sr_info,
             match find_smallest_disk_size session_id sr_info.sr with Some size -> size | None -> Sizes.(1L ** gib)
          )
      in
      List.fold_left
        (fun (chosen_sr, minimum_size) (sr, size) ->
           if size < minimum_size then ([sr], size) else (chosen_sr, minimum_size)
        )
        Sizes.([], 1L ** gib)
        minimum_sizes
      |> fst
    in
    sr_with_smallest_disk_size session_id sr_infos

  let with_any_vdi rpc session_id =
    List.filter (fun sr_info ->
        (List.mem `vdi_create sr_info.allowed_operations && List.mem `vdi_destroy sr_info.allowed_operations) ||
        (Client.Client.SR.get_VDIs ~rpc ~session_id ~self:sr_info.sr <> [])
      )

  let can_unplug rpc session_id =
    (* We filter out SRs that have any VDIs with VBDs. This is a safe
       overapproximation - we may be able to unplug some of the filtered out SRs *)
    List.filter
      (fun sr_info ->
         let vdis = Client.Client.SR.get_VDIs ~rpc ~session_id ~self:sr_info.sr in
         let vbds =
           List.map (fun vdi -> Client.Client.VDI.get_VBDs ~rpc ~session_id ~self:vdi) vdis
           |> List.concat
         in
         match vbds with
         | [] -> true
         | _::_ -> false
      )

  let allowed_operations ops _rpc _session_id sr_infos =
    List.filter (fun i -> Xapi_stdext_std.Listext.List.subset ops i.allowed_operations) sr_infos

  let has_capabilities caps _rpc _session_id sr_infos =
    List.filter (fun i -> Xapi_stdext_std.Listext.List.subset caps i.capabilities) sr_infos

  let not_type _type rpc session_id =
    List.filter (fun i -> Client.Client.SR.get_type ~rpc ~session_id ~self:i.sr <> _type)

  let (||>) a b = fun rpc session_id srs -> (a rpc session_id srs) |> (b rpc session_id)

  let filter rpc session_id f sr_infos = sr_infos |> f rpc session_id
end


type test_case = string * Alcotest.speed_level * (API.ref_session -> sr_info -> unit) * Sr_filter.t

(** Scan an SR and return the number of managed VDIs contained within *)
let count_vdis rpc session_id sr =
  Client.Client.SR.scan rpc session_id sr;
  let managed_vdis =
    Client.Client.SR.get_VDIs rpc session_id sr
    (* NB vhd backends may delete records beneath us *)
    |> Valid_ref_list.filter
      (fun vdi -> Client.Client.VDI.get_managed rpc session_id vdi)
  in
  List.length managed_vdis

let constant_vdi_number rpc session_id sr_info f =
  if List.mem `scan sr_info.allowed_operations then begin
    let before = count_vdis rpc session_id sr_info.sr in
    f session_id sr_info;
    let after = count_vdis rpc session_id sr_info.sr in
    Alcotest.(check int)
      "Should have same number of VDIs after the test as before"
      before
      after
  end

(** Creates a [Alcotest.test_case] from the given [storage_test_case] using the
    specified session ID and SR *)
let specialise rpc session_id storage_test sr_info =
  let (name, speed, test, _filter) = storage_test in
  let sr_name = Client.Client.SR.get_name_label ~session_id ~rpc ~self:sr_info.sr in
  let name = name ^ " on SR [" ^ sr_name ^ "]" in
  let test = fun () -> constant_vdi_number rpc session_id sr_info test in
  (name, speed, test)

(** Returns a list of [Alcotest.test_case]s that are specialized versions of
   this test for each SR that this test supports. *)
let expand rpc session_id sr_infos storage_test_case =
  let supported_srs : sr_info list =
    let (_name, _speed, _test, filter) = storage_test_case in
    Sr_filter.filter rpc session_id filter sr_infos
  in
  List.map (specialise rpc session_id storage_test_case) supported_srs

(** Returns a [Alcotest.test_case] for all possible (storage test, SR)
    combinations *)
let get_test_cases_for_srs rpc session_id storage_test_cases sr_infos =
  storage_test_cases
  |> List.map (expand rpc session_id sr_infos)
  |> List.flatten

let list_srs session_id filter =
  let sr_selection_filter =
    if !Quicktest_args.use_default_sr then Sr_filter.default_sr else Sr_filter.all
  in
  let rpc = !Quicktest_common.rpc in
  Client.Client.SR.get_all rpc session_id
  |> List.filter
    (fun sr ->
       Client.Client.SR.get_PBDs rpc session_id sr
       |> List.exists
         (fun pbd -> Client.Client.PBD.get_currently_attached rpc session_id pbd)
    )
  |> List.map (get_sr_info rpc session_id)
  |> sr_selection_filter rpc session_id
  |> filter rpc session_id

let get_test_cases session_id storage_test_cases =
  let rpc = !Quicktest_common.rpc in
  list_srs session_id Sr_filter.all |> (get_test_cases_for_srs rpc session_id storage_test_cases)
