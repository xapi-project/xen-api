module XenAPI = Client.Client
module Date = Xapi_stdext_date.Date

type result = Good | Expiring of string list | Expired of string list

let a_month_after date =
  let days_30 = Ptime.Span.unsafe_of_d_ps (30, 0L) in
  Date.to_ptime date
  |> (fun d -> Ptime.add_span d days_30)
  |> Option.fold ~none:date ~some:Date.of_ptime

let days_to_expiry ~expiry now =
  Ptime.diff (Date.to_ptime expiry) (Date.to_ptime now) |> Ptime.Span.to_d_ps
  |> fun (days, picosec) ->
  let with_fraction = if days < 0 then Fun.id else fun d -> d + 1 in
  if picosec = 0L then days else with_fraction days

let get_expiry_date pool_license =
  List.assoc_opt "expiry" pool_license
  |> Fun.flip Option.bind (fun e -> if e = "never" then None else Some e)
  |> Option.map Xapi_stdext_date.Date.of_iso8601

let get_hosts all_license_params threshold =
  List.filter_map
    (fun (name_label, license_params) ->
      let ( let* ) = Option.bind in
      let* expiry = get_expiry_date license_params in
      if Date.is_earlier expiry ~than:threshold then
        Some name_label
      else
        None
    )
    all_license_params

let check_license now pool_license_state all_license_params =
  match get_expiry_date pool_license_state with
  | Some expiry ->
      let days = days_to_expiry ~expiry now in
      if days <= 0 then
        Expired (get_hosts all_license_params now)
      else if days <= 30 then
        Expiring (get_hosts all_license_params (a_month_after now))
      else
        Good
  | None ->
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
