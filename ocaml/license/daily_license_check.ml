module XenAPI = Client.Client

type result = Good | Expiring of string list | Expired of string list

let seconds_per_day = 3600. *. 24.

let seconds_per_30_days = 30. *. seconds_per_day

let days_to_expiry now expiry =
  (expiry /. seconds_per_day) -. (now /. seconds_per_day)

let get_hosts all_license_params threshold =
  List.fold_left
    (fun acc (name_label, license_params) ->
      let expiry = List.assoc "expiry" license_params in
      let expiry = Xapi_stdext_date.Date.(to_unix_time (of_iso8601 expiry)) in
      if expiry < threshold then
        name_label :: acc
      else
        acc
    )
    [] all_license_params

let check_license now pool_license_state all_license_params =
  let expiry = List.assoc "expiry" pool_license_state in
  let expiry = Xapi_stdext_date.Date.(to_unix_time (of_iso8601 expiry)) in
  let days = days_to_expiry now expiry in
  if days <= 0. then
    Expired (get_hosts all_license_params now)
  else if days <= 30. then
    Expiring (get_hosts all_license_params (now +. seconds_per_30_days))
  else
    Good

let get_info_from_db rpc session_id =
  let pool = List.hd (XenAPI.Pool.get_all ~rpc ~session_id) in
  let pool_license_state =
    XenAPI.Pool.get_license_state ~rpc ~session_id ~self:pool
  in
  let hosts = XenAPI.Host.get_all_records ~rpc ~session_id in
  let all_license_params =
    List.map
      (fun (_, host) -> (host.API.host_name_label, host.API.host_license_params))
      hosts
  in
  (pool, pool_license_state, all_license_params)

let execute rpc session_id pool result =
  let send_alert session_id pool msg body =
    let name, priority = msg in
    let obj_uuid = XenAPI.Pool.get_uuid ~rpc ~session_id ~self:pool in
    ignore
      (XenAPI.Message.create ~rpc ~session_id ~name ~priority ~cls:`Pool
         ~obj_uuid ~body
      )
  in
  match result with
  | Good ->
      ()
  | Expiring hosts ->
      let body =
        Printf.sprintf
          "The licenses of the following hosts are about to expire: %s"
          (String.concat ", " hosts)
      in
      send_alert session_id pool Api_messages.license_expires_soon body
  | Expired hosts ->
      let body =
        Printf.sprintf "The licenses of the following hosts have expired: %s"
          (String.concat ", " hosts)
      in
      send_alert session_id pool Api_messages.license_expired body
