open Db_filter_types
open Stdext.Listext

module D = Debug.Make(struct let name="cluster_stack_constraints" end)
open D

(* Check which cluster stack we can use based on the kinds of SRs that are attached *)
let required_cluster_stack ~__context =
  let constraints = List.map
      (fun (_, rc) -> rc.API.sM_type, rc.API.sM_required_cluster_stack)
      (Db.SM.get_all_records ~__context)
  in
  let active_cluster_stack =
    List.map
      (fun cluster -> Db.Cluster.get_cluster_stack ~__context ~self:cluster)
      (Db.Cluster.get_all ~__context)
    |> List.setify
  in
  (* Check which PBDs are attached on the master (assume this is running on the master) *)
  let localhost = Helpers.get_localhost ~__context in
  let pbds = Db.PBD.get_refs_where ~__context ~expr:(And (
      Eq (Field "host", Literal (Ref.string_of localhost)),
      Eq (Field "currently_attached", Literal "true")
    )) in
  (* Obtain constraints from the SR drivers. Each SR that has constraints
       * returns a list of alternative cluster stacks, any one of which will
       * work for the SR. *)
  let required_stacks = List.filter_map (fun pbd ->
      let sr = Db.PBD.get_SR ~__context ~self:pbd in
      let sr_type = Db.SR.get_type ~__context ~self:sr in
        match List.assoc sr_type constraints with
        | exception Not_found -> begin
          error "SR type not found in SM table.";
          failwith "SR type not found in SM table." end
        | [] -> None    (* No constraints *)
        | l ->  Some l  (* Any one of these will do *)
    ) pbds in
  let failwith_cluster_stack_conflict () =
      error "Conflicting cluster stack demands.";
      failwith "Conflicting cluster stack demands."
  in
  match required_stacks with
  | [] ->
    (* None of the attached SRs have constraints *)
    begin match active_cluster_stack with
    | [] -> None
    | [ stack ] -> Some stack
    | _ -> failwith_cluster_stack_conflict ()
    end
  | [stacks] ->
    (* There is one SR with constraints; pick the first alternative. *)
    Some (List.hd stacks)
  | hd :: tl ->
    (* There are multiple attached SRs with constraints. The intersection of
         * the sets of alternatives captures which cluster stacks are possible. *)
    match List.fold_left List.intersect hd tl with
    | [] ->
      (* This must be avoided by the PBD.plug code *)
      failwith_cluster_stack_conflict ()
    | stack :: _ ->
      (* Multiple options; just pick the first one. *)
      Some stack

(* Choose a cluster stack given the constraints. Use default stack if there are no constaints. *)
let choose_cluster_stack ~__context =
  match required_cluster_stack ~__context with
  | Some stack -> stack
  | None -> !Xapi_globs.cluster_stack_default

(* Check whether the given SR is compatible with the given cluster stack *)
let assert_sr_compatible ~__context ~cluster_stack ~sr =
  match Xha_scripts.get_supported_srs cluster_stack with
  | None -> ()  (* No constraints *)
  | Some srs ->
    let sr_type = Db.SR.get_type ~__context ~self:sr in
    if not (List.exists (fun x -> x = sr_type) srs) then
      raise (Api_errors.Server_error (Api_errors.incompatible_statefile_sr, [sr_type]))

(* Check whether we can attach the SR given the cluster stack that is currently in use *)
let assert_cluster_stack_compatible ~__context sr =
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_ha_enabled ~__context ~self:pool then begin
    let current_stack = Db.Pool.get_ha_cluster_stack ~__context ~self:pool in
    let sr_type = Db.SR.get_type ~__context ~self:sr in
    let sms = Db.SM.get_refs_where ~__context ~expr:(Eq (Field "type", Literal sr_type)) in
    match sms with
    | sm :: _ ->
      let constraints = Db.SM.get_required_cluster_stack ~__context ~self:sm in
      (match constraints with
       | [] -> ()  (* No constraints *)
       | alternatives ->
         if List.exists (fun x -> x = current_stack) alternatives then
           ()  (* Constraints satisfied *)
         else
           raise Api_errors.(Server_error
                    (incompatible_cluster_stack_active, [String.concat "," alternatives]))
      )
    | [] ->
      error "SR type not found in SM table.";
      failwith "SR type not found in SM table."
  end
