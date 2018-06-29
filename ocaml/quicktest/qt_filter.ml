
module A = Quicktest_args

type 'a test_case = string * Alcotest.speed_level * 'a

type ('a, 'b) filter = 'a test_case list -> 'b test_case list

let for_each f test_cases = List.map f test_cases |> List.concat
(** Creates a filter from a function that processes only one test case,
    by applying this function to every test case in the list, and concatenating
    the results. *)

let session_id = ref API.Ref.null

let vdi_count = Hashtbl.create 4

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

(** Called before the quicktests start to save the original state.
    This data will be used by [finish] to check for any resource leaks. *)
let init () =
  session_id := Qt.init_session !A.rpc !A.username !A.password;
  Client.Client.SR.get_all_records ~rpc:!A.rpc ~session_id:!session_id
  |> List.iter (fun (ref, sr) ->
    if List.mem `scan sr.API.sR_allowed_operations then begin
      let before = count_vdis !A.rpc !session_id ref in
      Hashtbl.add vdi_count sr.API.sR_uuid before
    end)

(** Called at the end of the quicktests to check that no resources leaked
    during the test run *)
let finish () =
  Client.Client.SR.get_all_records ~rpc:!A.rpc ~session_id:!session_id
  |> List.iter (fun (ref, sr) ->
      match Hashtbl.find_opt vdi_count sr.API.sR_uuid with
      | Some before ->
          if List.mem `scan sr.API.sR_allowed_operations then begin
            let after = count_vdis !A.rpc !session_id ref in
            if after <> before then
              failwith (Printf.sprintf "VDIs leaked on SR %s: before=%d, after=%d" sr.API.sR_uuid before after)
          end
      | None -> ())

let cleanup () = Client.Client.Session.logout ~rpc:!A.rpc ~session_id:!session_id

let wrap f =
  init ();
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () -> f (); finish ())
    cleanup

let conn tcs = for_each (fun (name, speed, test) -> [(name, speed, test !A.rpc !session_id)]) tcs

module SR = struct
  type srs = unit -> Qt.sr_info list

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

      let sm =
        let ty = Client.Client.SR.get_type rpc session_id sR in
        let sms = Client.Client.SM.get_all_records rpc session_id in
        match List.filter (fun (_, r) -> r.API.sM_type = ty) sms with
        | [] -> Alcotest.failf "Could not find SM plugin for SR type %s" ty
        | [ _, plugin ] -> plugin
        | _ :: _ -> Alcotest.failf "Multiple SM plugins found for SR type %s" ty
      in

      let caps = sm.API.sM_capabilities in
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
      (ops, caps, sm.API.sM_required_api_version)
    in

    let allowed_operations, capabilities, required_sm_api_version = get_sr_features session_id sr in
    let open Qt in
    { sr
    ; allowed_operations
    ; capabilities
    ; required_sm_api_version
    }

  let list_selected_srs rpc session_id =
    Client.Client.SR.get_all rpc session_id
    |> List.filter
      (fun sr ->
         Client.Client.SR.get_PBDs rpc session_id sr
         |> List.exists
           (fun pbd -> Client.Client.PBD.get_currently_attached rpc session_id pbd)
      )
    |> List.map (get_sr_info rpc session_id)

  let only sr () = [get_sr_info !A.rpc !session_id sr]

  let all_srs = lazy (list_selected_srs !A.rpc !session_id)

  let all =
    if !A.use_default_sr then begin
      let pool = Qt.get_pool !A.rpc !session_id in
      only (Client.Client.Pool.get_default_SR ~rpc:!A.rpc ~session_id:!session_id ~self:pool)
    end else
      fun () -> Lazy.force all_srs

  let random srs () =
    let srs = srs () in
    let index = Random.int @@ List.length srs in
    [List.nth srs index]

  let sr_filter f srs () = List.filter f (srs ())

  let not_iso =
    sr_filter (fun sr_info -> Client.Client.SR.get_content_type !A.rpc !session_id sr_info.Qt.sr <> "iso")

  let is_empty = function [] -> true | _::_ -> false

  let with_any_vdi =
    sr_filter (fun sr_info ->
        (List.mem `vdi_create sr_info.Qt.allowed_operations && List.mem `vdi_destroy sr_info.Qt.allowed_operations) ||
        (not (is_empty (Client.Client.SR.get_VDIs ~rpc:!A.rpc ~session_id:!session_id ~self:sr_info.Qt.sr)))
      )

  let can_unplug =
    (* We filter out SRs that have any VDIs with VBDs. This is a safe
       overapproximation - we may be able to unplug some of the filtered out SRs *)
    sr_filter
      (fun sr_info ->
         let rpc = !A.rpc in
         let session_id = !session_id in
         let vdis = Client.Client.SR.get_VDIs ~rpc ~session_id ~self:sr_info.Qt.sr in
         let vbds =
           List.map (fun vdi -> Client.Client.VDI.get_VBDs ~rpc ~session_id ~self:vdi) vdis
           |> List.concat
         in
         is_empty vbds
      )

  let allowed_operations ops =
    sr_filter (fun i -> Xapi_stdext_std.Listext.List.subset ops i.Qt.allowed_operations)

  let has_capabilities caps =
    sr_filter (fun i -> Xapi_stdext_std.Listext.List.subset caps i.Qt.capabilities)

  let not_type _type =
    sr_filter (fun i -> Client.Client.SR.get_type ~rpc:!A.rpc ~session_id:!session_id ~self:i.Qt.sr <> _type)

  let is_smapiv1 sr_info = sr_info.Qt.required_sm_api_version < "3.0"

  let smapiv1 =
    sr_filter is_smapiv1

  let smapiv3 =
    sr_filter (fun i -> not (is_smapiv1 i))

  (** Creates a [Alcotest.test_case] from the given [storage_test_case] using the
      specified session ID and SR *)
  let specialise (name, speed, test) sr_info =
    let rpc = !A.rpc in
    let session_id = !session_id in
    let sr_name = Client.Client.SR.get_name_label ~session_id ~rpc ~self:sr_info.Qt.sr in
    let name = name ^ " on SR [" ^ sr_name ^ "]" in
    let test = test sr_info in
    (name, speed, test)

  let list_srs srs = srs ()

  let f srs tcs =
    for_each (fun test_case ->
      List.map (specialise test_case) (list_srs srs)
    ) tcs
end

let sr = SR.f
