(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** Workload Balancing
 *  @group Workload Balancing
*)

open Stdext
open Printf
open Xstringext
open Threadext
module D = Debug.Make(struct let name = "workload_balancing" end)
open D

exception Xml_parse_failure of string

let request_mutex = Locking_helpers.Named_mutex.create "WLB"

let raise_url_invalid url =
  raise (Api_errors.Server_error (Api_errors.wlb_url_invalid, [url]))

let raise_malformed_response' meth reason response =
  raise (Api_errors.Server_error
           (Api_errors.wlb_malformed_response,
            [meth; reason; response]))

let raise_malformed_response meth reason response =
  raise_malformed_response' meth reason (Xml.to_string response)

let raise_not_initialized () =
  raise (Api_errors.Server_error (Api_errors.wlb_not_initialized, []))

let raise_disabled () =
  raise (Api_errors.Server_error (Api_errors.wlb_disabled, []))

let raise_timeout timeout =
  raise (Api_errors.Server_error
           (Api_errors.wlb_timeout, [string_of_float timeout]))

let raise_verify_error reason =
  raise (Api_errors.Server_error (Api_errors.ssl_verify_error, [reason]))

let raise_authentication_failed () =
  raise (Api_errors.Server_error (Api_errors.wlb_authentication_failed, []))

let raise_connection_refused () =
  raise (Api_errors.Server_error (Api_errors.wlb_connection_refused, []))

let raise_unknown_host () =
  raise (Api_errors.Server_error (Api_errors.wlb_unknown_host, []))

let raise_connection_reset () =
  raise (Api_errors.Server_error (Api_errors.wlb_connection_reset, []))

let raise_internal_error args =
  raise (Api_errors.Server_error (Api_errors.wlb_internal_error, args))

let split_host_port url =
  try
    if url.[0] = '[' then (* IPv6 *)
      begin
        let host_end = String.rindex url ']' in
        if url.[host_end + 1] <> ':' then
          raise_url_invalid url;
        let host = String.sub url 1 (host_end - 1) in
        let port = String.sub url (host_end + 2) (String.length url - host_end - 2) in
        (host, int_of_string port)
      end
    else
      match String.split_f (fun a -> a = ':') url with
      | [host; port] ->
        (host, int_of_string port)
      | _ ->
        raise_url_invalid url
  with
  | _ -> raise_url_invalid url

let wlb_host_port ~__context =
  let pool = Helpers.get_pool ~__context in
  let url = Db.Pool.get_wlb_url ~__context ~self:pool in
  split_host_port url

let assert_wlb_licensed ~__context =
  Pool_features.assert_enabled ~__context ~f:Features.WLB

let assert_wlb_initialized ~__context =
  let pool = Helpers.get_pool ~__context in
  if Db.Pool.get_wlb_url ~__context ~self:pool = ""
  then
    raise_not_initialized()

let assert_wlb_enabled ~__context =
  let pool = Helpers.get_pool ~__context in
  assert_wlb_licensed ~__context;
  assert_wlb_initialized ~__context;
  if not (Db.Pool.get_wlb_enabled ~__context ~self:pool)
  then
    raise_disabled()

(* when other calls use wlb to enhance their decision making process they need to know if it is available or whether they should use another algorithm *)
let check_wlb_enabled ~__context =
  try
    assert_wlb_enabled ~__context;
    true
  with
  | Api_errors.Server_error (err, _) ->
    debug "Wlb check failed: %s" err;
    false

let match_xml_tag x t =
  match x with
  | Xml.Element (tag, _, _) -> String.compare tag t == 0
  | Xml.PCData data -> false

let is_parent_to parent_element child_tag =
  match parent_element with
  | Xml.Element ( _, _, xml_elements) ->
    (List.exists (fun x -> match_xml_tag x child_tag) xml_elements)
  | _ -> false

let is_childless elem =
  match elem with
  | (Xml.Element ( _, _, [Xml.PCData _])) -> true
  | (Xml.Element ( _, _, children)) -> (List.length children == 0)
  |  Xml.PCData _ -> true

let unexpected_data meth tag xml =
  raise_malformed_response meth
    (sprintf "Found data in %s node, expected only parent nodes" tag)
    xml

(* Function walks down the xml tree matching nodes in a path defined by tag_names , returning the element at the end of the path.
   Throws Xml_parse_failure to be handled in context of the calling function *)
let rec descend_and_match tag_names xml =
  match tag_names, xml with
  | [], elem -> elem  (* have reached end of the list, with all correct matches so return this element*)
  | hd_tag::tail, Xml.Element ( _, _, [Xml.PCData data]) ->
    (*we have a leaf node, check that we are at the end of the path and return it *)
    raise_malformed_response "unknown"
      (sprintf "Method descend_and_match failed. Found leaf node with tag\
                %s, but expected path to continue with: %s->%s"
         hd_tag hd_tag (String.concat "->" tag_names)) xml
  | hd_tag::tail, Xml.Element ( _, _, xml_elements) ->
    (* take the tag off the head of the list and search the children of this element for it *)
    begin
      try
        descend_and_match tail
          (List.find (fun x -> match_xml_tag x hd_tag) xml_elements)
      with
      | Not_found ->
        raise (Xml_parse_failure
                 (sprintf "Descend_and_match failed. Node %s not found." hd_tag))
    end
  | _, Xml.PCData _ ->
    (* This should never happen as a leaf node is detected in an earlier match and returned *)
    raise_malformed_response' "unknown" "Method descend_and_match failed. Found orphan leaf node" ""

let data_from_leaf element =
  match element with
  | Xml.Element ( _, _, [Xml.PCData data]) ->data
  | Xml.Element ( _, _, _) ->  ""
  | _ ->
    raise_malformed_response "unknown" "Expected element to be leaf node"
      element

let path_to_exception =
  ["Body"; "Fault"; "Reason"; "Text"]
let path_to_inner meth =
  ["Body"; sprintf "%sResponse" meth; sprintf "%sResult" meth]

let pool_uuid_param ~__context =
  let pool = Helpers.get_pool ~__context in
  sprintf "<PoolUuid>%s</PoolUuid>" (Db.Pool.get_uuid ~__context ~self:pool)

let wlb_body meth params =
  Printf.sprintf
    "<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\">
  <s:Body>
    <%s xmlns=\"http://schemas.citrix.com/DWM\">
      <request xmlns:i=\"http://www.w3.org/2001/XMLSchema-instance\">
        %s
      </request>
    </%s>
  </s:Body>
</s:Envelope>" meth params meth

let wlb_request host meth body encoded_auth =
  let headers = [
    "SOAPAction", sprintf "\"http://schemas.citrix.com/DWM/IWorkloadBalance/%s\"" meth;
    "Content-Type", "text/xml; charset=utf-8";
    "Authorization", "Basic " ^ encoded_auth;
    "Host", host;
  ] in
  Xapi_http.http_request Http.Post ~headers ~body
    "/Citrix.Dwm.WorkloadBalance/Service" ~keep_alive:false

let filtered_headers headers =
  List.map (fun s ->
      if String.startswith "Authorization:" s then
        "Authorization: Basic <password>"
      else
        s)
    headers

let encoded_auth un pw =
  Base64.encode (Printf.sprintf "%s:%s" un pw)

let wlb_encoded_auth ~__context =
  let pool = Helpers.get_pool ~__context in
  let secret_ref = Db.Pool.get_wlb_password ~__context ~self:pool in
  encoded_auth
    (Db.Pool.get_wlb_username ~__context ~self:pool)
    (Db.Secret.get_value ~__context ~self:secret_ref)

let generate_safe_param tag_name tag_value =
  Xml.to_string (Xml.Element(tag_name, [], [Xml.PCData tag_value]))

(* if the call has failed we should try and retrieve the result code and any error messages*)
let parse_result_code meth xml_data response initial_error enable_log =
  let code =
    try
      data_from_leaf (descend_and_match ["ResultCode"] xml_data)
    with
    | Xml_parse_failure error ->
      raise_malformed_response' meth
        (sprintf "After failing to retrieve valid response, an error code\
                  could not be found. Some data is missing or corrupt.
Attempt retrieve valid response: (%s)
Attempt to retrieve error code: (%s)"
           initial_error error)
        (if enable_log
         then
           response
         else
           "Logging output disabled for this call.")
  in
  let message =
    try
      data_from_leaf (descend_and_match ["ErrorMessage"] xml_data)
    with
    | Xml_parse_failure msg -> ""
  in
  raise_internal_error [code; message]

let retrieve_inner_xml meth response enable_log=
  try
    descend_and_match (path_to_inner meth) response
  with
  | Xml_parse_failure error ->
    try
      raise_internal_error ["Exception:";
                            data_from_leaf (descend_and_match path_to_exception response)]
    with
    | Xml_parse_failure msg ->
      if enable_log
      then
        raise_malformed_response meth
          "Expected data is missing or corrupt. No exception found."
          response
      else
        raise_malformed_response' meth
          "Expected data is missing or corrupt. No exception found."
          "Logging output disabled for this call."

(* This function handles the actual network request and deals with any errors relating to the connection *)
let wlb_request ~__context ~host ~port ~auth ~meth ~params ~handler ~enable_log ~timeout_key ~timeout_default =
  let body = wlb_body meth params in
  let request = wlb_request host meth body auth in
  let pool = Helpers.get_pool ~__context in
  let verify_cert = Db.Pool.get_wlb_verify_cert ~__context ~self:pool in
  let pool_other_config = Db.Pool.get_other_config ~__context ~self:pool in
  let timeout =
    try
      if List.mem_assoc timeout_key pool_other_config then
        float_of_string (List.assoc timeout_key pool_other_config)
      else
        timeout_default
    with
    | _ ->
      timeout_default
  in
  if enable_log then
    debug "%s\n%s" (String.concat "\n" (filtered_headers (Http.Request.to_header_list request))) body;
  try
    Remote_requests.perform_request ~__context ~timeout ~verify_cert
      ~host ~port ~request ~handler ~enable_log
  with
  | Remote_requests.Timed_out ->
    raise_timeout timeout
  | Http_client.Http_request_rejected _ | Http_client.Http_error _ ->
    raise_authentication_failed ()
  | Xmlrpc_client.Connection_reset ->
    raise_connection_reset ()
  | Stunnel.Stunnel_verify_error reason ->
    raise_verify_error reason
  | Stunnel.Stunnel_error error_msg as exc->
    begin
      match error_msg with
      | "Connection refused" ->
        raise_connection_refused ()
      | "No route to host"
      | "No host resolved" ->
        raise_unknown_host ()
      | "Invalid argument" ->
        raise_url_invalid (sprintf "%s:%s" host (string_of_int port))
      | "" -> raise_connection_reset()
      | _ ->
        raise exc
    end
  | Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
    raise_connection_refused ()

let perform_wlb_request ?auth ?url ?enable_log ~meth ~params
    ~handle_response ~__context () =
  (* now assumes naming policy of xml repsonses is uniform Envelope->Body-> <x>Response-> <x>Result  where <x> is method name *)
  let enable_log =
    match enable_log with
    | Some b -> b
    | None -> true
  in
  let host, port =
    match url with
    | Some u -> split_host_port u
    | None -> wlb_host_port ~__context
  in
  let auth' =
    match auth with
    | Some x -> x
    | None -> wlb_encoded_auth ~__context
  in
  let result = ref None in
  (* this function attempts to parse the result into xml , and pass the 'result' section through to the handler*)
  let check_response response s =
    let response =
      try
        Xmlrpc_client.XML_protocol.read_response response s
      with
      | Xml.Error err ->
        raise_malformed_response' meth (Xml.error err) ""
    in
    debug "\n\n%s\n\n" (Xml.to_string response);
    let inner_xml = retrieve_inner_xml meth response enable_log in
    result := Some (
        let code =
          try
            data_from_leaf (descend_and_match ["ResultCode"] inner_xml)
          with
          | Xml_parse_failure error -> "0" (* If it failed trying to get ResultCode, assume the call was successful *)
        in
        if code <> "0" then
          (* Call failed, get error message and raise internal error *)
          let message =
            try
              data_from_leaf (descend_and_match ["ErrorMessage"] inner_xml)
            with
            | Xml_parse_failure error -> ""
          in raise_internal_error [code; message]
        else
          (* Call was successful, parse inner xml *)
          try
            handle_response inner_xml
          with
          | Xml_parse_failure error ->
            parse_result_code meth inner_xml (Xml.to_string response)
              error enable_log)
  in
  wlb_request ~__context ~host ~port ~auth:auth' ~meth ~params
    ~timeout_key:Xapi_globs.wlb_timeout
    ~timeout_default:Xapi_globs.default_wlb_timeout
    ~handler:check_response ~enable_log;
  match !result with
  | Some s -> s
  | None -> raise_internal_error []

let val_num i =
  try
    ignore (float_of_string i);
    i
  with
  | Failure _ ->
    raise_malformed_response' "unknown"
      "Could not parse star rating/recID/optID" "unknown"

let retrieve_vm_recommendations ~__context ~vm =
  assert_wlb_enabled ~__context;
  let params =
    sprintf "%s\n<VmUuid>%s</VmUuid>" (pool_uuid_param ~__context)
      (Db.VM.get_uuid ~__context ~self:vm)
  in
  let handle_response inner_xml =
    let extract_data place_recommendation =
      try
        let h = Db.Host.get_by_uuid ~__context
            ~uuid:(data_from_leaf
                     (descend_and_match ["HostUuid"] place_recommendation)) in
        if (is_parent_to place_recommendation "Stars")
        then
          (h, ["WLB";
               val_num (data_from_leaf (descend_and_match
                                          ["Stars"] place_recommendation));
               val_num (data_from_leaf (descend_and_match
                                          ["RecommendationId"] place_recommendation))])
        else
          (h, ["WLB"; "0.0";
               val_num (data_from_leaf (descend_and_match
                                          ["RecommendationId"] place_recommendation));
               data_from_leaf (descend_and_match
                                 ["ZeroScoreReason"] place_recommendation)])
      with
      | Xml_parse_failure error ->
        (* let this parse error carry on upwards,  perform_wlb_request will catch it and check the rest of the xml for an error code *)
        raise (Xml_parse_failure error)
      | Db_exn.Read_missing_uuid (_,_,_)
      | Db_exn.Too_many_values (_,_,_) ->
        raise_malformed_response' "VMGetRecommendations"
          "Invalid VM or host UUID" "unknown"
    in
    let recs = descend_and_match ["Recommendations"] inner_xml in
    if (is_childless recs)
    then
      []
    else
      match recs with
      | Xml.Element ( _, _, children) ->
        if ((List.length children) != List.length((Helpers.get_live_hosts ~__context)))
        then
          raise_malformed_response "VMGetRecommendations"
            "List of returned reccomendations is not equal to the number of hosts in pool"
            inner_xml
        else
          List.map (fun x -> extract_data x) children
      | _ -> assert false (*the is_childless should catch this *)
  in
  perform_wlb_request ~meth:"VMGetRecommendations" ~params ~handle_response
    ~__context ()

let init_wlb ~__context ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password =
  assert_wlb_licensed ~__context;
  let pool = Helpers.get_pool ~__context in
  let master = Db.Pool.get_master ~__context ~self:pool in
  let params = (sprintf "%s\n%s\n%s\n%s\n"
                  (generate_safe_param "Password" xenserver_password)
                  (pool_uuid_param ~__context)
                  (generate_safe_param "UserName" xenserver_username)
                  (generate_safe_param "XenServerUrl"
                     (let address_type = Record_util.primary_address_type_of_string (Xapi_inventory.lookup Xapi_inventory._management_address_type ~default:"ipv4") in
                      let master_address = Db.Host.get_address ~__context ~self:master in
                      if address_type = `IPv4 then
                        sprintf "http://%s:80/" master_address
                      else
                        (*This is an ipv6 address, put [] around the address so that WLB can properly parse the url*)
                        sprintf "http://[%s]:80/" master_address)))
  in
  let handle_response inner_xml =
    (*A succesful result has an ID inside the addxenserverresult *)
    (* delete if it already exists *)
    match (data_from_leaf (descend_and_match["Id"] inner_xml)) with
    | _ ->
      let old_secret_ref = Db.Pool.get_wlb_password ~__context ~self:pool in
      let wlb_secret_ref = Xapi_secret.create ~__context ~value:wlb_password ~other_config:[] in
      Db.Pool.set_wlb_username ~__context ~self:pool ~value:wlb_username;
      Db.Pool.set_wlb_password ~__context ~self:pool ~value:wlb_secret_ref;
      Db.Pool.set_wlb_url ~__context ~self:pool ~value:wlb_url;
      Pervasiveext.ignore_exn (fun _ -> Db.Secret.destroy ~__context ~self:old_secret_ref);
  in
  Locking_helpers.Named_mutex.execute request_mutex (perform_wlb_request ~enable_log:false
                                                       ~meth:"AddXenServer" ~params
                                                       ~auth:(encoded_auth wlb_username wlb_password) ~url:wlb_url
                                                       ~handle_response ~__context)

let decon_wlb ~__context =
  let clear_wlb_config ~__context ~pool =
    let secret_ref = Db.Pool.get_wlb_password ~__context ~self:pool in
    Db.Pool.set_wlb_username ~__context ~self:pool ~value:"";
    Db.Pool.set_wlb_password ~__context ~self:pool ~value:Ref.null;
    Db.Pool.set_wlb_url ~__context ~self:pool ~value:"";
    Db.Pool.set_wlb_enabled ~__context ~self:pool ~value:false;
    Db.Secret.destroy ~__context ~self:secret_ref
  in
  let pool = Helpers.get_pool ~__context in
  let handle_response inner_xml =
    (* A succesful result is empty. Check this before clearing config *)
    if (is_childless inner_xml)
    then
      clear_wlb_config ~__context ~pool
    else
      (*child elements are errors. Raise an  exception to force an error check *)
      raise (Xml_parse_failure "Expected blank result from a deconfigure")
  in
  if Db.Pool.get_wlb_url ~__context ~self:pool = ""
  then
    raise_not_initialized()
  else
    let params = pool_uuid_param ~__context in
    try Locking_helpers.Named_mutex.execute request_mutex (perform_wlb_request ~meth:"RemoveXenServer"
                                                             ~params ~handle_response ~__context) with
    (*Based on CA-60147,CA-93312 and CA-137044 - XAPI is designed to handle the error *)
    | _ -> clear_wlb_config ~__context ~pool

let send_wlb_config ~__context ~config =
  assert_wlb_licensed ~__context;
  assert_wlb_initialized ~__context;
  let rec xml_params kv_map =
    match kv_map with
    | (k,v):: tl ->
      Printf.sprintf
        "<b:KeyValueOfstringstring>%s%s</b:KeyValueOfstringstring>\n%s"
        (generate_safe_param "b:Key" k) (generate_safe_param "b:Value" v)
        (xml_params tl)
    | [] -> ""
  in
  let params = sprintf "
    <OptimizationParms \
xmlns:b=\"http://schemas.microsoft.com/2003/10/Serialization/Arrays\">
      %s
    </OptimizationParms>
    %s
" (xml_params config) (pool_uuid_param ~__context)
  in
  let handle_response inner_xml =
    (* A succesful result is empty. Check this now *)
    if (is_childless inner_xml)
    then
      ()
    else
      (*child elements are errors. Raise an  exception to force an error check *)
      raise (Xml_parse_failure
               "Expected blank result from a send_wlb_config")
  in
  perform_wlb_request ~meth:"SetXenPoolConfiguration" ~params ~handle_response
    ~__context ()

let retrieve_wlb_config ~__context =
  assert_wlb_licensed ~__context;
  assert_wlb_initialized ~__context;
  let params = pool_uuid_param ~__context in
  let handle_response inner_xml =
    let rec gen_map key_value_parents =
      match key_value_parents with
      | Xml.Element (_, _, _) as key_value_parent :: tl ->
        (data_from_leaf (descend_and_match ["Key"] key_value_parent),
         data_from_leaf (descend_and_match ["Value"] key_value_parent))
        :: gen_map tl
      | Xml.PCData _ :: tl ->
        unexpected_data "GetXenPoolConfiguration" "Configuration"
          inner_xml
      | [] -> []
    in
    match (descend_and_match ["OptimizationParms"] inner_xml) with
    | Xml.Element (_, _, children) ->
      gen_map children
    | _ ->
      raise_malformed_response "GetXenPoolConfiguration"
        "Expected children to OptimizationParms node" inner_xml
  in
  perform_wlb_request ~meth:"GetXenPoolConfiguration" ~params ~handle_response
    ~__context ()

let get_dom0_vm ~__context host =
  Db.Host.get_control_domain ~__context ~self:(Db.Host.get_by_uuid ~__context ~uuid:host)

let get_opt_recommendations ~__context =
  assert_wlb_enabled ~__context;
  let params = pool_uuid_param ~__context in
  let handle_response inner_xml =
    let rec gen_map key_value_parents =
      match key_value_parents with
      | Xml.Element (_, _, kvalues) :: tl ->
        List.map (
          fun elem -> match elem with
            | (Xml.Element (key, _, _)) as leaf ->
              (key, data_from_leaf leaf)
            | Xml.PCData _ ->
              unexpected_data "GetOptimizationRecommendations"
                "PoolOptimizationRecommendation"
                inner_xml
        ) kvalues :: gen_map tl
      | Xml.PCData _ :: tl ->
        unexpected_data "GetOptimizationRecommendations"
          "Recommendations node"
          inner_xml
      | [] -> [];
    in
    if (is_childless inner_xml)
    then
      ([],"") (*No recommendations to give. *)
    else
      match (descend_and_match ["Recommendations"] inner_xml) with
      | Xml.Element (_, _, children) ->
        (gen_map children,
         data_from_leaf (descend_and_match ["OptimizationId"] inner_xml))
      | _ -> debug "IS CHILDLESS"; assert false (*is_childless should prevent this case *)
  in
  let result_map =
    perform_wlb_request ~meth:"GetOptimizationRecommendations" ~params
      ~handle_response ~__context ()
  in

  let rec remap vm hostfrom hostto reason rec_id opt_id = function
    | (k, v) :: vs ->    debug "k:%s v:%s" k v;
      if k = "VmToMoveUuid" && v <> "" then
        remap (Some (Db.VM.get_by_uuid ~__context ~uuid:v)) hostfrom hostto
          reason rec_id opt_id vs
      else if k = "MoveToHostUuid" then
        remap vm hostfrom (Some v) reason rec_id opt_id vs
      else if k = "MoveFromHostUuid" then
        remap vm (Some v) hostto reason rec_id opt_id vs
      else if k = "Reason" then
        remap vm hostfrom hostto (Some v) rec_id opt_id vs
      else if k = "RecommendationId" then
        remap vm hostfrom hostto reason (Some v) opt_id vs
      else
        remap vm hostfrom hostto reason rec_id opt_id vs
    | [] ->
      begin
        match (vm, hostfrom, hostto, reason, rec_id, opt_id) with
        | (Some vm', _, Some hostto', Some reason', Some rec_id', opt_id') -> (vm', ["WLB"; hostto'; val_num (opt_id'); val_num (rec_id'); reason'])
        | (None, Some hostfrom',_, Some reason', Some rec_id', opt_id') -> ( (get_dom0_vm ~__context hostfrom'), ["WLB"; hostfrom'; val_num (opt_id'); val_num (rec_id'); reason'])
        | _ ->
          raise_malformed_response' "GetOptimizationRecommendations"
            "Missing VmToMoveUuid, RecID, MoveToHostUuid, or Reason" "unknown"
      end
  in
  try
    match result_map with (map, opt_id) ->
      List.map (fun kvs -> remap None None None None None opt_id kvs) map
  with
  | Db_exn.Read_missing_uuid (_,_,_)
  | Db_exn.Too_many_values (_,_,_) ->
    raise_malformed_response' "GetOptimizationRecommendations" "Invalid VM or host UUID" "unknown"



(* note that this call only returns a rec for each vm which can be migrated *)
let get_evacuation_recoms ~__context ~uuid =
  assert_wlb_enabled ~__context;
  let params =
    sprintf "<HostUuid>%s</HostUuid>\n%s" uuid (pool_uuid_param ~__context)
  in
  let handle_response inner_xml =
    let rec gen_map key_value_parents =
      match key_value_parents with
      | Xml.Element (_, _, kvalues) :: tl ->
        List.map (
          fun elem -> match elem with
            | (Xml.Element (key, _, _)) as leaf ->
              (key, data_from_leaf leaf)
            | Xml.PCData _ ->
              unexpected_data "HostGetRecommendations"
                "HostEvacuationRecommendation"
                inner_xml
        ) kvalues :: gen_map tl
      | Xml.PCData _ :: tl ->
        unexpected_data "HostGetRecommendations"
          "Recommendations"
          inner_xml
      | [] -> []
    in
    if (is_childless inner_xml)
    then
      []
    else
      match inner_xml with
      | (Xml.Element ( _, _, _)) ->
        begin
          match (descend_and_match ["Recommendations"] inner_xml) with
          | Xml.Element (_, _, children) ->
            gen_map children
          | _ -> [] (* just data, which we are treating as an empty response *)
        end
      | Xml.PCData _ -> unexpected_data "HostGetRecommendations"
                          "HostGetRecommendationsResult" inner_xml
  in
  let result_map =
    perform_wlb_request ~meth:"HostGetRecommendations" ~params
      ~handle_response ~__context ()
  in
  let rec remap vm host rec_id = function
    | (k, v) :: vs ->
      if k = "VmUuid" && v<> "" then
        remap (Some (Db.VM.get_by_uuid ~__context ~uuid:v)) host rec_id vs
      else if k = "HostUuid" then
        remap vm (Some v) rec_id vs
      else if k = "RecommendationId" then
        remap vm host (Some v) vs
      else
        remap vm host rec_id vs
    | [] ->
      begin
        match (vm, host, rec_id) with
        | (Some vm', Some host', Some rec_id') -> (vm', ["WLB"; host'; val_num (rec_id')])
        | (None, Some host', Some rec_id') -> ((get_dom0_vm ~__context host'), ["WLB"; host'; val_num (rec_id')])
        | _ ->
          raise_malformed_response' "HostGetRecommendations"
            "Missing VmUuid, RecID or HostUuid" "unknown"
      end
  in
  try
    List.map (fun kvs -> remap None None None kvs) result_map
  with
  | Db_exn.Read_missing_uuid (_,_,_)
  | Db_exn.Too_many_values (_,_,_) ->
    raise_malformed_response' "HostGetRecommendations"
      "Invalid VM or host UUID" "unknown"

let make_param = function
  | (n, v) ->
    Xml.Element("ReportParameter", [],
                [Xml.Element("ParameterName", [], [Xml.PCData n]);
                 Xml.Element("ParameterValue", [], [Xml.PCData v])])

let wlb_context_request meth params ~__context ~handler =
  assert_wlb_licensed ~__context;
  assert_wlb_initialized ~__context;
  let host, port = wlb_host_port ~__context in
  let auth = wlb_encoded_auth ~__context in
  wlb_request ~__context ~host ~port ~auth ~meth ~params ~handler
    ~enable_log:true
    ~timeout_key:Xapi_globs.wlb_reports_timeout
    ~timeout_default:Xapi_globs.default_wlb_reports_timeout

let wlb_report_request report params =
  let meth = "ExecuteReport" in
  let p =
    ((Xml.to_string (Xml.Element("ReportName", [], [Xml.PCData report]))) ^
     (Xml.to_string (Xml.Element("ReportParms", [],
                                 List.map make_param params))))
  in
  debug "%s" p;
  (meth, wlb_context_request meth p)

let wlb_diagnostics_request =
  let meth = "GetDiagnostics" in
  (meth, wlb_context_request meth "")
