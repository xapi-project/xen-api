module XenAPI = Client.Client

type result = Good | Expiring of string list | Expired of string list

let seconds_per_day = 3600. *. 24.

let seconds_per_30_days = 30. *. seconds_per_day

let days_to_expiry now expiry =
  (expiry /. seconds_per_day) -. (now /. seconds_per_day)

let get_expiry_date license =
  List.assoc_opt "expiry" license
  |> Fun.flip Option.bind (fun e -> if e = "never" then None else Some e)
  |> Option.map Xapi_stdext_date.Date.of_iso8601
  |> Option.map Xapi_stdext_date.Date.to_unix_time

let get_hosts all_license_params threshold =
  List.fold_left
    (fun acc (name_label, license_params) ->
      match get_expiry_date license_params with
      | Some expiry when expiry < threshold ->
          name_label :: acc
      | _ ->
          acc
    )
    [] all_license_params

let check_license now pool_license_state all_license_params =
  match get_expiry_date pool_license_state with
  | Some expiry ->
      let days = days_to_expiry now expiry in
      if days <= 0. then
        Expired (get_hosts all_license_params now)
      else if days <= 30. then
        Expiring (get_hosts all_license_params (now +. seconds_per_30_days))
      else
        Good
  | None ->
      Good

let get_info_from_db rpc session =
  let pool = List.hd (XenAPI.Pool.get_all rpc session) in
  let pool_license_state = XenAPI.Pool.get_license_state rpc session pool in
  let hosts = XenAPI.Host.get_all_records rpc session in
  let all_license_params =
    List.map
      (fun (_, host) -> (host.API.host_name_label, host.API.host_license_params))
      hosts
  in
  (pool, pool_license_state, all_license_params)

let execute rpc session pool result =
  let send_alert session pool msg body =
    let name, priority = msg in
    let pool_uuid = XenAPI.Pool.get_uuid rpc session pool in
    ignore (XenAPI.Message.create rpc session name priority `Pool pool_uuid body)
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
      send_alert session pool Api_messages.license_expires_soon body
  | Expired hosts ->
      let body =
        Printf.sprintf "The licenses of the following hosts have expired: %s"
          (String.concat ", " hosts)
      in
      send_alert session pool Api_messages.license_expired body
