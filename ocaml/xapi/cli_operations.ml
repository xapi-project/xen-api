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
(**
 * @group Command-Line Interface (CLI)
*)

open Cli_protocol
open Cli_util
open Cli_cmdtable
open Stdext
open Xstringext
open Pervasiveext
open Listext
open Fun

module D=Debug.Make(struct let name="cli" end)
open D

open Records

let failwith str = raise (Cli_util.Cli_failure str)
exception ExitWithError of int

let bool_of_string param string =
  let s = String.lowercase_ascii string in
  match s with
    "true" -> true
  | "t" -> true
  | "1" -> true
  | "false" -> false
  | "f" -> false
  | "0" -> false
  | _ -> failwith ("Failed to parse parameter '"^param^"': expecting 'true' or 'false'")

let get_bool_param params ?(default = false) param =
  if List.mem_assoc param params
  then bool_of_string param (List.assoc param params)
  else default

let get_float_param params param ~default =
  if List.mem_assoc param params
  then try float_of_string (List.assoc param params) with Not_found -> default
  else default

let get_param params param ~default =
  if List.mem_assoc param params
  then List.assoc param params
  else default

open Client

let progress_bar printer task_record =
  let progress = task_record.API.task_progress in
  let hashes = String.make (int_of_float (progress *. 70.)) '#' in
  let animation = "|/-\\" in
  let char = animation.[int_of_float (progress *. 100.) mod (String.length animation)] in
  let line = Printf.sprintf "\r %3d%% %c %s" (int_of_float (progress *. 100.)) char hashes in
  Cli_printer.PStderr line |> printer

let wait_with_progress_bar printer rpc session_id task =
  Cli_util.track (progress_bar printer) rpc session_id task;
  Cli_printer.PStderr "\n" |> printer;
  Cli_util.result_from_task rpc session_id task

let wait printer rpc session_id task =
  Cli_util.track (fun _ -> ()) rpc session_id task;
  Cli_util.result_from_task rpc session_id task

let waiter printer rpc session_id params task =
  finally
    (fun () ->
       (if List.mem_assoc "progress" params
        then wait_with_progress_bar
        else wait) printer rpc session_id task)
    (fun () ->
       Client.Task.destroy rpc session_id task
    )

(* Return the list of k=v pairs for maps *)
let read_map_params name params =
  let len = String.length name + 1 in (* include ':' *)
  let filter_params = List.filter (fun (p,_) -> (String.startswith name p) && (String.length p > len)) params in
  List.map (fun (k,v) -> String.sub k len (String.length k - len),v) filter_params
let read_set_params name params = List.map fst (read_map_params name params)

let get_chunks fd =
  let buffer = Buffer.create 10240 in
  let rec f () =
    match unmarshal fd with
    | Blob (Chunk len) ->
      debug "Reading a chunk of %ld bytes" len;
      let data = Unixext.really_read_string fd (Int32.to_int len) in
      Buffer.add_string buffer data;
      f()
    | Blob End ->
      Buffer.contents buffer
    | _ ->
      failwith "Thin CLI protocol error"
  in
  f()

let get_client_file fd filename =
  marshal fd (Command (Load filename));
  match unmarshal fd with
  | Response OK ->
    Some (get_chunks fd)
  | Response Failed ->
    None
  | _ ->
    failwith "Thin CLI protocol error"


let diagnostic_compact printer rpc session_id params =
  Gc.compact ()

let diagnostic_gc_stats printer rpc session_id params =
  let stat = Gc.stat () in
  let table =
    ["minor_words",string_of_float stat.Gc.minor_words;
     "promoted_words",string_of_float stat.Gc.promoted_words;
     "major_words",string_of_float stat.Gc.major_words;
     "minor_collections",string_of_int stat.Gc.minor_collections;
     "major_collections",string_of_int stat.Gc.major_collections;
     "heap_words",string_of_int stat.Gc.heap_words;
     "heap_chunks",string_of_int stat.Gc.heap_chunks;
     "live_words",string_of_int stat.Gc.live_words;
     "live_blocks",string_of_int stat.Gc.live_blocks;
     "free_words",string_of_int stat.Gc.free_words;
     "free_blocks",string_of_int stat.Gc.free_blocks;
     "largest_free",string_of_int stat.Gc.largest_free;
     "fragments",string_of_int stat.Gc.fragments;
     "compactions",string_of_int stat.Gc.compactions;
     "top_heap_words",string_of_int stat.Gc.top_heap_words;
    ] in
  printer (Cli_printer.PTable [table])

let diagnostic_timing_stats printer rpc session_id params =
  let table_of_host host =
    [ "host-uuid", Client.Host.get_uuid rpc session_id host;
      "host-name-label", Client.Host.get_name_label rpc session_id host ] @
    (try
       Client.Host.get_diagnostic_timing_stats rpc session_id host
     with e ->
       [ "Error", Api_errors.to_string e ]) in
  let all = List.map table_of_host (Client.Host.get_all rpc session_id) in

  printer (Cli_printer.PTable all)

let diagnostic_net_stats printer rpc session_id params =
  let all = Http_svr.Server.all_stats Xapi_http.server in
  let meth (m, _, _) =
    not (List.mem_assoc "method" params)
    || (String.lowercase_ascii(Http.string_of_method_t m) = String.lowercase_ascii (List.assoc "method" params)) in
  let uri (_, u, _) =
    not (List.mem_assoc "uri" params)
    || (String.lowercase_ascii u = String.lowercase_ascii (List.assoc "uri" params)) in
  let has_param x = not(List.mem_assoc "params" params) || (List.mem x (String.split ',' (List.assoc "params" params))) in
  let all = List.filter meth (List.filter uri all) in
  let rows = List.map
      (fun (m, uri, stats) ->
         let m' = if has_param "method" then [ Http.string_of_method_t m ] else [] in
         let uri' = if has_param "uri" then [ uri ] else [] in
         let requests' = if has_param "requests" then [ string_of_int stats.Http_svr.Stats.n_requests ] else [] in
         let connections' = if has_param "connections" then [ string_of_int stats.Http_svr.Stats.n_connections ] else [] in
         let framed' = if has_param "framed" then [ string_of_int stats.Http_svr.Stats.n_framed ] else [] in
         m' @ uri' @ requests' @ connections' @ framed'
      ) all in
  let widths = Table.compute_col_widths rows in
  let sll = List.map (List.map2 Table.right widths) rows in
  List.iter (fun line -> printer (Cli_printer.PMsg (String.concat " | " line))) sll

let diagnostic_db_stats printer rpc session_id params =
  let (n,avgtime,min,max) = Db_lock.report () in
  let (writes,reads,creates,drops,tasks,threads) = Stats.summarise_db_calls () in
  printer (Cli_printer.PMsg (Printf.sprintf "DB lock stats: n=%d avgtime=%f min=%f max=%f" n avgtime min max));
  printer (Cli_printer.PMsg "Reads:");
  printer (Cli_printer.PList reads);
  printer (Cli_printer.PMsg "Writes:");
  printer (Cli_printer.PList writes);
  printer (Cli_printer.PMsg "Creates:");
  printer (Cli_printer.PList creates);
  printer (Cli_printer.PMsg "Drops:");
  printer (Cli_printer.PList drops);
  printer (Cli_printer.PMsg "Tasks:");
  printer (Cli_printer.PTable (List.map (fun (name,ops)-> ("task",name)::ops) (List.sort (fun (t1,ops1) (t2,ops2)-> compare (List.length ops2) (List.length ops1)) tasks)));
  printer (Cli_printer.PMsg "Threads:");
  printer (Cli_printer.PTable (List.map (fun (id,ops)-> ("thread",string_of_int id)::ops) threads))

let diagnostic_db_log printer rpc session_id params =
  Stats.log_stats := true;
  printer (Cli_printer.PMsg "Database/task statistics gathering enabled. Warning, this never releases memory! Restart xapi to reset.")

type host_license = {
  hostname: string;
  uuid: string;
  rstr: Features.feature list;
  edition: string;
  edition_short: string;
  expiry: float;
}
let host_license_of_r host_r editions =
  let params = host_r.API.host_license_params in
  let rstr = Features.of_assoc_list params in
  let expiry =
    if List.mem_assoc "expiry" params then
      Date.to_float (Date.of_string (List.assoc "expiry" params))
    else
      0.
  in
  let edition_str = host_r.API.host_edition in
  let unavailable_edition_str = "N/A" in
  let edition_short =
    editions
    |> List.filter_map
      V6_interface.(fun ed -> if ed.title = edition_str
                     then Some ed.code
                     else None)
    |> (function
        | a::_ -> a
        | _ -> unavailable_edition_str
      ) in
  {
    hostname = host_r.API.host_hostname;
    uuid = host_r.API.host_uuid;
    rstr = rstr;
    edition = edition_str;
    edition_short = edition_short;
    expiry = expiry;
  }

let diagnostic_license_status printer rpc session_id params =
  let hosts = Client.Host.get_all_records rpc session_id in
  let heading = [ "Hostname"; "UUID"; "Features"; "Code"; "Free"; "Expiry"; "Days left" ] in
  let editions = V6_client.get_editions "diagnostic_license_status" in

  let valid, invalid = List.partition (fun (_, host_r) -> try ignore(host_license_of_r host_r editions); true with _ -> false) hosts in
  let host_licenses = List.map (fun (_, host_r) -> host_license_of_r host_r editions) valid in
  (* Sort licenses into nearest-expiry first then free *)
  let host_licenses = List.sort (fun a b ->
      let a_expiry = a.expiry and b_expiry = b.expiry in
      let a_free = a.edition = "free"
      and b_free = b.edition = "free" in
      if a_expiry < b_expiry then -1
      else
      if a_expiry > b_expiry then 1
      else
      if a_free && not b_free then -1
      else
      if not a_free && b_free then 1
      else 0) host_licenses in
  let now = Unix.gettimeofday () in
  let hosts = List.map (fun h -> [ h.hostname;
                                   String.sub h.uuid 0 8;
                                   Features.to_compact_string h.rstr;
                                   h.edition_short;
                                   string_of_bool (h.edition = "free");
                                   Date.to_string (Date.of_float h.expiry);
                                   Printf.sprintf "%.1f" ((h.expiry -. now) /. (24. *. 60. *. 60.));
                                 ]) host_licenses in
  let invalid_hosts = List.map (fun (_, host_r) -> [ host_r.API.host_hostname;
                                                     String.sub host_r.API.host_uuid 0 8;
                                                     "-"; "-"; "-"; "-"; "-" ]) invalid in
  let __context = Context.make "diagnostic_license_status" in
  let pool = Helpers.get_pool ~__context in
  let pool_features = Features.of_assoc_list (Db.Pool.get_restrictions ~__context ~self:pool) in
  let pool_free = List.fold_left (||) false (List.map (fun h -> h.edition = "free") host_licenses) in
  let divider = [ "-"; "-"; "-"; "-"; "-"; "-"; "-" ] in
  let pool = [ "-"; "-"; Features.to_compact_string pool_features; "-"; string_of_bool pool_free; "-"; "-" ] in
  let table = heading :: divider :: hosts @ invalid_hosts @ [ divider; pool ] in

  (* Compute the required column widths *)
  let rec transpose x =
    if List.filter (fun x -> x <> []) x = []
    then []
    else
      let heads = List.map List.hd x in
      let tails = List.map List.tl x in
      heads :: (transpose tails) in
  let map f x = List.map (List.map f) x in
  let column_sizes = List.map (List.fold_left max 0) (transpose (map String.length table)) in

  List.iter
    (fun row ->
       printer (Cli_printer.PMsg (String.concat " " (List.map (fun (data, len) -> data ^ (String.make (len - String.length data) ' ')) (List.combine row column_sizes))))
    ) table

let get_hosts_by_name_or_id rpc session_id name =
  let hosts = Client.Host.get_all_records_where rpc session_id "true" in
  let allrecs = List.map (fun (host,host_r) -> let r = host_record rpc session_id host in r.setrefrec (host,host_r); r) hosts in
  let hosts = List.filter
      (fun x -> (safe_get_field (field_lookup x.fields "name-label") = name
                 || (safe_get_field (field_lookup x.fields "uuid") = name))) allrecs in
  hosts

let get_host_by_name_or_id rpc session_id name =
  let hosts = get_hosts_by_name_or_id rpc session_id name in
  if List.length hosts = 0 then (failwith ("Host "^name^" not found"));
  List.nth hosts 0

let get_host_from_session rpc session_id =
  Client.Session.get_this_host rpc session_id session_id

(* Create a VBD record in database and attempt to hotplug it, ignoring hotplug errors *)
let create_vbd_and_plug_with_other_config rpc session_id vm vdi device_name bootable rw cd unpluggable qtype qparams other_config =
  let vbd = Client.VBD.create ~rpc ~session_id ~vM:vm ~vDI:vdi ~userdevice:device_name ~bootable ~mode:rw
      ~_type:cd ~unpluggable ~empty:false ~qos_algorithm_type:qtype ~qos_algorithm_params:qparams ~other_config in
  try Client.VBD.plug rpc session_id vbd
  with Api_errors.Server_error(_, _) as e ->
    debug "VBD created but not hotplugged: %s" (Api_errors.to_string e)

let create_vbd_and_plug rpc session_id vm vdi device_name bootable rw cd unpluggable qtype qparams =
  create_vbd_and_plug_with_other_config rpc session_id vm vdi device_name bootable rw cd unpluggable qtype qparams []

let create_owner_vbd_and_plug rpc session_id vm vdi device_name bootable rw cd unpluggable qtype qparams =
  create_vbd_and_plug_with_other_config rpc session_id vm vdi device_name bootable rw cd unpluggable qtype qparams [Xapi_globs.owner_key,""]


(* ---------------------------------------------------------------------
   CLI Operation Implementation
   --------------------------------------------------------------------- *)

(* NB: all logging is now via syslog. No manual log controls are here. *)
let log_set_output printer _ session_id params =
  ()

let log_get_keys printer _ session_id params =
  ()

let log_get printer _ session_id params =
  ()

let log_reopen printer _ session_id params =
  ()

let string_of_task_status task = match task.API.task_status with
  | `pending ->
    Printf.sprintf "%d %% complete "
      (int_of_float (task.API.task_progress *. 100.))
  | `success ->
    "Completed"
  | `failure ->
    "Failed"
  | `cancelling ->
    "Cancelling"
  | `cancelled ->
    "Cancelled"

(*let task_list printer rpc session_id params =
  let internal = try (List.assoc "internal" params)="true" with _ -> false in
  let task_records = get_task_records rpc session_id in
  let recs =
  List.map
  (fun (ref,task) ->
  let common =
  ["NAME",task.API.task_name_label;
  "uuid",task.API.task_uuid;
  "descr",task.API.task_name_description;
  "status", (string_of_task_status task);
  ] in
  (* If in show-internal mode, list the locks too *)
  let locks = if internal
  then List.map (fun lock -> "lock", lock)
  (Client.Task.get_locks rpc session_id ref)
  else [] in
  common @ locks)
  task_records
  in
  printer (Cli_printer.PTable recs)
*)

let user_password_change _ rpc session_id params =
  let old_pwd = List.assoc_default "old" params ""
  (* "new" must be in params here, since it is a required parameter. *)
  and new_pwd = List.assoc "new" params in
  Client.Session.change_password rpc session_id old_pwd new_pwd

(** Low level CLI interface **)

(* Have a record for each class of the API, then accessor functions for it in a consistent way *)
(* e.g. vm-create vm-destroy vm-param-list vm-param-set vm-param-get vm-param-add vm-param-remove *)
(* create creates an instance of a class with a minimal set of required fields set on the cmd line *)
(* and the rest as optional params - e.g. *)
(* xe vm-create name-label=mynewvm name-description="a nice new vm" *)
(* and returns the uuid of the created object *)
(* vm-destroy takes the uuid and destroys the object *)
(* vm-param-list takes the uuid and lists either a default set of parameters, or those passed *)

let alltrue l =
  List.fold_left (&&) true l

let get_set_names rlist =
  let sets = List.filter (fun r -> r.get_set <> None) rlist in
  List.map (fun r -> r.name) sets

let get_map_names rlist =
  let maps = List.filter (fun r -> r.get_map <> None) rlist in
  List.map (fun r -> r.name) maps

let safe_get_field x =
  try x.get ()
  with
  | Api_errors.Server_error(s,_) as e-> if s=Api_errors.handle_invalid then "<invalid reference>" else raise e
  | e -> raise e

type fieldtype = Normal | Set of string | Map of string

let get_field_type fieldname record =
  if List.exists (fun field -> field.name=fieldname) record
  then Normal
  else
    begin
      (* New 'normal' behaviour is to split map name from key by the separator ':' *)
      if String.contains fieldname ':' then
        begin
          let i = String.index fieldname ':' in
          let real_fieldname = String.sub fieldname 0 i in
          try
            let field = List.find (fun field -> field.name=real_fieldname) record in
            if field.get_set <> None
            then Set field.name
            else if field.get_map <> None
            then Map field.name
            else failwith ("Field '"^(field.name)^"' is not a set or map")
          with
            Not_found -> failwith ("Unknown field '"^fieldname^"'")
        end
      else
        (* Old behaviour is to match like this: param-name-key=value *)
        begin
          (* Find all the maps, then sort in length order, longest first *)
          let mapfields = List.filter (fun field -> field.get_map <> None) record in
          let mapfields = List.sort (fun a b -> compare (String.length b.name) (String.length a.name)) mapfields in
          try
            (* Find the first (longest) matching field *)
            let field = List.find (fun field -> String.startswith (field.name^"-") fieldname) mapfields in
            Map field.name
          with
            Not_found ->
            let setfields = List.filter (fun field -> field.get_set <> None) record in
            let setfields = List.sort (fun a b -> compare (String.length b.name) (String.length a.name)) setfields in
            try
              let field = List.find (fun field -> String.startswith (field.name^"-") fieldname) setfields in
              Set field.name
            with
              _ -> failwith ("Unknown field '"^fieldname^"'")
        end
    end

let filter_records_on_set_param records (k,v) s =
  (* On entry here, s is the name of the parameter, and k will be of the form s[:-]contains *)
  let n = String.length s in
  let contains = String.sub k (n + 1) (String.length k - n - 1) in
  if contains<>"contains" then failwith "Invalid syntax for set filtering (should be set-param:contains=key)";
  let filterfn record =
    let field = field_lookup record.fields s in
    let get_set = match field.get_set with
      | Some x -> x
      | None -> (failwith (Printf.sprintf "Records broken (field %s)" s))
    in
    try
      let set = get_set () in
      let set, v =
        if field.case_insensitive
        then List.map String.lowercase_ascii set, String.lowercase_ascii v
        else set, v in
      List.exists (fun member -> v=member) set
    with
      _ -> false
  in
  List.filter filterfn records

let filter_records_on_map_param records (k,v) s =
  (* On entry here, s is the name of the parameter, and k will be of the form s[:-]key *)
  let n = String.length s in
  let key = String.sub k (n + 1) (String.length k - n - 1) in
  let filterfn record =
    let field = field_lookup record.fields s in
    let get_map = match field.get_map with
      | Some x -> x
      | None -> failwith (Printf.sprintf "Records broken (field %s)" s)
    in
    try
      let map = get_map () in
      let map, key, v =
        if field.case_insensitive
        then List.map (fun (k, v) -> String.lowercase_ascii k, v) map, String.lowercase_ascii key, String.lowercase_ascii v
        else map, key, v in
      List.mem_assoc key map && List.assoc key map = v
    with
      _ -> false
  in
  List.filter filterfn records

let filter_records_on_normal_param records (k,v) =
  let filterfn record =
    let field = field_lookup record.fields k in
    let value = safe_get_field field in
    if field.case_insensitive
    then String.lowercase_ascii value = String.lowercase_ascii v
    else value=v
  in
  List.filter filterfn records

let filter_records_on_fields records (k,v) =
  (* Ignore empty lists *)
  if records = [] then [] else begin

    (* We can only tell what types fields are by looking at a record itself. *)
    (* We use the first one *)
    let firstrec = List.hd records in

    (* Switch on the type of the field *)
    match get_field_type k firstrec.fields with
      Normal -> filter_records_on_normal_param records (k,v)
    | Map s -> filter_records_on_map_param records (k,v) s
    | Set s -> filter_records_on_set_param records (k,v) s
  end


let stdparams = ["server";"password";"port";"username"; "minimal"; "force"; "multiple"; "all"; "message-priority"; "trace"]

(* This goes through the list of parameters, extracting any of the form map-name-key=value   *)
(* where map-name is the name of a map in the class. These will be used to set the key-value *)
(* pair in the map. Returns a list of params that didn't fit this form *)


let choose_params params defaults =
  if List.mem_assoc "params" params
  then
    let ps = List.assoc "params" params in
    (if ps="all" then [] else String.split_f (fun c -> c = ',') ps)
  else defaults

let select_fields params records default_params =
  let params = choose_params params default_params in
  if params=[] then (List.map (fun record -> record.fields) records) else
    (List.map (fun record -> List.filter (fun field -> List.mem field.name params) record.fields) records)

let print_field x =
  let append =
    if x.get_set <> None then
      (* Set *)
      if x.add_to_set = None then
        " (SRO)"
      else
        " (SRW)"
    else if x.get_map <> None then
      (* map *)
      if x.add_to_map = None then
        " (MRO)"
      else
        " (MRW)"
    else if x.set = None then
      " ( RO)"
    else
      " ( RW)"
  in
  let result = safe_get_field x in
  (x.name ^ append ^ (if x.deprecated then " [DEPRECATED]" else ""), result)

type printer = Cli_printer.print_fn
type rpc = (Rpc.call -> Rpc.response)
type params = (string * string) list

(* Check the params for "database:vdi-uuid=" - if this parameter is present, *)
(* open the database on the specified VDI and use the resulting session_id. *)
(* If the parameter is not present, use the original session_id. *)
let with_specified_database rpc session_id params f =
  let database_params = read_map_params "database" params in
  let use_db_vdi = List.mem_assoc "vdi-uuid" database_params in
  let use_db_file = List.mem_assoc "filename" database_params in
  if use_db_vdi && use_db_file then
    failwith "xapi can query a DB vdi or a DB file, but not both.";
  let session_id =
    if use_db_vdi then begin
      let database_vdi_uuid = List.assoc "vdi-uuid" database_params in
      let database_vdi = Client.VDI.get_by_uuid ~rpc ~session_id ~uuid:database_vdi_uuid in
      Client.VDI.open_database ~rpc ~session_id ~self:database_vdi
    end else if use_db_file then begin
      let database_file = List.assoc "filename" database_params in
      Client.Session.create_from_db_file ~rpc ~session_id ~filename:database_file
    end else
      session_id
  in
  finally
    (fun () -> f session_id)
    (fun () -> if use_db_vdi || use_db_file then Client.Session.logout ~rpc ~session_id)

let make_param_funs getall getallrecs getbyuuid record class_name def_filters def_list_params rpc session_id =
  let get_record2 rpc session_id x =
    let r = record rpc session_id x in
    r.fields
  in

  let get_record rpc session_id uuid =
    get_record2 rpc session_id (getbyuuid ~rpc ~session_id ~uuid)
  in

  let list printer rpc session_id params : unit =
    with_specified_database rpc session_id params
      (fun session_id ->
         let all = getallrecs ~rpc ~session_id ~expr:"true" in
         let all_recs = List.map (fun (r,x) -> let record = record rpc session_id r in record.setrefrec (r,x); record) all in

         (* Filter on everything on the cmd line except params=... *)
         let filter_params = List.filter (fun (p,_) -> not (List.mem p ("params"::stdparams))) params in
         (* Filter out all params beginning with "database:" *)
         let filter_params = List.filter (fun (p,_) -> not (String.startswith "database:" p)) filter_params in
         (* Add in the default filters *)
         let filter_params = def_filters @ filter_params in
         (* Filter all the records *)
         let records = List.fold_left filter_records_on_fields all_recs filter_params in

         let print_all = get_bool_param params "all" in

         let print_params = select_fields params (if print_all then all_recs else records) def_list_params in
         let print_params = List.map (fun fields -> List.filter (fun field -> not field.hidden) fields) print_params in
         let print_params = List.map (fun fields -> List.map (fun field -> if field.expensive then makeexpensivefield field else field) fields) print_params in

         printer (Cli_printer.PTable (List.map (List.map print_field) print_params))
      )
  in

  let p_list printer rpc session_id params : unit =
    with_specified_database rpc session_id params
      (fun session_id ->
         let record = get_record rpc session_id (List.assoc "uuid" params) in
         let record = List.filter (fun field -> not field.hidden) record in
         printer (Cli_printer.PTable [List.map print_field record])
      )
  in

  let p_get printer rpc session_id params : unit =
    with_specified_database rpc session_id params
      (fun session_id ->
         let record = get_record rpc session_id (List.assoc "uuid" params) in
         let param = List.assoc "param-name" params in
         let x = field_lookup record param in
         let std () =
           printer (Cli_printer.PList [ safe_get_field x])
         in
         if List.mem_assoc "param-key" params then
           let key = List.assoc "param-key" params in
           match x.get_map with
             Some f ->
             let result =
               try List.assoc key (f ()) with _ -> failwith (Printf.sprintf "Key %s not found in map" key) in
             printer (Cli_printer.PList [result])
           | None -> std ()
         else std ()
      )
  in

  let p_set (printer : printer) rpc session_id params =
    let record = get_record rpc session_id (List.assoc "uuid" params) in
    let set_params = List.filter (fun (p,_) -> not (List.mem p ("uuid"::stdparams))) params in
    (* Hashtable set_map_table contains key as set_map function
       and associated value as list of (key, value) pairs to set a map field *)
    let set_map_table: (((string * string) list -> unit), (string * string) list) Hashtbl.t = Hashtbl.create 10 in

    let set_field (k,v) =
      let field_type = get_field_type k record in
      match field_type with
      | Map s ->
        let field = field_lookup record s in
        let n = String.length s in
        let key = String.sub k (n + 1) (String.length k - n - 1) in
        let get_map = match field.get_map with
          | Some x -> x
          | None -> failwith (Printf.sprintf "Broken Records (field %s)" s)
        in begin
          (* If set_in_map is present, use it instead of using remove_from_map followed by add_to_map. *)
          (* If set_map is present then accumulate all (key, value) pairs into set_map_table *)
          match field.set_in_map, field.set_map  with
          | Some set_in_map, None -> set_in_map key v
          | None, Some set_map ->
            let existing_params = try Hashtbl.find set_map_table set_map with Not_found -> [] in
            Hashtbl.replace set_map_table set_map ((key, v) :: existing_params)
          | None, None ->
            let add_to_map = match field.add_to_map with Some f -> f | None -> failwith ("Map field '"^s^"' is read-only.") in
            let remove_from_map = match field.remove_from_map with Some f -> f | None -> failwith (Printf.sprintf "Records broken (field %s)" s) in
            let map = get_map () in
            if List.mem_assoc key map then remove_from_map key;
            add_to_map key v
          | Some _, Some _ -> failwith (Printf.sprintf "Broken Records (field %s)" s)
        end
      | Set s -> failwith "Cannot param-set on set fields"
      | Normal ->
        let field=field_lookup record k in
        let set = match field.set, field.add_to_map with
          | Some f, _    -> f
          | None, Some f -> failwith ("Field '"^k^"' is a map or set. use the 'name:key=value' syntax.")
          | None, None   -> failwith ("Field '"^k^"' is read-only.") in
        try
          set v
        with
        (* XXX: -- warning 52 -- this might break with new ocaml compilers *)
          (Failure "int_of_string") -> failwith ("Parameter "^k^" must be an integer")
        | (Failure "float_of_string") -> failwith ("Parameter "^k^" must be a floating-point number")
        | (Invalid_argument "bool_of_string") -> failwith ("Parameter "^k^" must be a boolean (true or false)")
        | e -> raise e
    in
    List.iter set_field set_params;
    Hashtbl.iter (fun func params -> func params) set_map_table
  in

  let p_add (printer : printer) rpc session_id params =
    let record = get_record rpc session_id (List.assoc "uuid" params) in
    let param_name = List.assoc "param-name" params in
    let filter_params = List.filter (fun (p,_) -> not (List.mem p ("uuid"::"param-name"::"param-key"::stdparams))) params in
    match field_lookup record param_name with
    | { add_to_set = Some f } ->
      if List.mem_assoc "param-key" params then
        let key = List.assoc "param-key" params in
        f key
      else
        failwith "When adding a key to a set, use the syntax: *-param-add param-name=<name> param-key=<key>"
    | { add_to_map = Some f } -> List.iter (fun (k,x) -> f k x) filter_params
    | { get_set = Some _; add_to_set=None }
    | { get_map = Some _; add_to_map=None } ->
      failwith "Parameter is read-only"
    | _ -> failwith "Can only add to parameters of type Set or Map"
  in

  let p_remove (printer : printer) rpc session_id params =
    let record = get_record rpc session_id (List.assoc "uuid" params) in
    let param_name = List.assoc "param-name" params in
    let param_key = List.assoc "param-key" params in
    match field_lookup record param_name with
    | { get_set = Some g; remove_from_set = Some f } -> if List.mem param_key (g ()) then f param_key else failwith (Printf.sprintf "Key %s is not in the set" param_key)
    | { get_map = Some g; remove_from_map = Some f } -> if List.mem_assoc param_key (g ()) then f param_key else failwith (Printf.sprintf "Key %s is not in map" param_key)
    | { get_set = Some _; remove_from_set = None }
    | { get_map = Some _; remove_from_map = None } -> failwith "Cannot remove parameters from read-only map"
    | _ -> failwith "Can only remove from parameters of type Set or Map"
  in

  let p_clear (printer : printer) rpc session_id params =
    let record = get_record rpc session_id (List.assoc "uuid" params) in
    let param_name = List.assoc "param-name" params in
    match field_lookup record param_name with
    | { get_set = Some f; remove_from_set = Some g } -> List.iter g (f ())
    | { get_map = Some f; remove_from_map = Some g } -> List.iter g (List.map fst (f ()))
    | { set = Some f } -> (try f "" with _ -> failwith "Cannot clear this parameter")
    | _ -> failwith "Can only clear RW parameters"
  in

  let gen_frontend (rpc:rpc) (session_id:API.ref_session) =
    let make_cmdtable_data (opname, reqd, optn, help, impl, std) =
      (opname,{reqd=reqd; optn=optn; help=help; implementation=No_fd impl; flags=if std then [Standard] else []})
    in
    try
      let all = List.filter (fun x -> not x.hidden) (record rpc session_id (Ref.null)).fields in
      let all_optn = List.map (fun r -> r.name) all in
      let settable = List.map (fun r -> r.name) (List.filter (fun r -> r.set <> None) all) in
      let settable = settable @ (List.map (fun r -> r.name ^ ":") (List.filter (fun r -> r.add_to_map <> None || r.set_in_map <> None || r.set_map <> None) all)) in
      let addable = List.map (fun r -> r.name) (List.filter (fun r -> r.add_to_set <> None || r.add_to_map <> None) all) in
      let clearable = List.map (fun r -> r.name) (List.filter (fun r -> r.set <> None || r.get_set <> None || r.get_map <> None) all) in
      (* We need the names of the set and map filters *)
      let sm_param_names =
        let sets = List.filter (fun field -> field.get_set <> None) all in
        List.map (fun field -> field.name^":contains") sets
      in
      let cli_name n = class_name^"-"^n in
      let plural = if class_name="patch" then "patches" else class_name^"s" in
      let ops = [(cli_name "list",[],"params"::"database:"::all_optn@sm_param_names, "Lists all the "^plural^", filtering on the optional arguments. To filter on map parameters, use the syntax 'map-param:key=value'",list,(class_name="vm" || class_name="network" || class_name="sr"));
                 (cli_name "param-list",["uuid"],["database:"],"Lists all the parameters of the object specified by the uuid.",p_list,false);
                 (cli_name "param-get",["uuid";"param-name"],["param-key";"database:"],"Gets the parameter specified of the object. If the parameter is a map of key=value pairs, use 'param-key=<key>' to get the value associated with a particular key.",p_get,false)] in
      let ops = if List.length settable > 0 then
          (cli_name "param-set",["uuid";],settable,"Sets the parameter specified. If param-value is not given, the parameter is set to a null value. To set a (key,value) pair in a map parameter, use the syntax 'map-param:key=value'.",p_set,false)::ops
        else ops in
      let ops = if List.length addable > 0 then
          ops @  [(cli_name "param-add",["uuid";"param-name"],["param-key"],"Adds to a set or map parameter. If the parameter is a set, use param-key=<key to add>. If the parameter is a map, pass the values to add as 'key=value' pairs.",p_add,false);
                  (cli_name "param-remove",["uuid";"param-name";"param-key"],[],"Removes a member or a key,value pair from a set/map respectively.",p_remove,false)]
        else ops in
      let ops = if List.length clearable > 0 then
          ops @ [(cli_name "param-clear",["uuid";"param-name"],[],"Clears the specified parameter (param-name can be "^(String.concat "," clearable)^").",p_clear,false)]
        else ops in
      List.map make_cmdtable_data ops
    with _ -> []
  in
  gen_frontend rpc session_id

(* the fields to show when doing `xe <class>-list`, whereas
   `xe <class>-param-list uuid=...` shows all the non-hidden fields of a record *)
let gen_cmds rpc session_id =
  let mk = make_param_funs in
  List.concat
    [ Client.Pool.(mk get_all get_all_records_where get_by_uuid pool_record "pool" [] ["uuid";"name-label";"name-description";"master";"default-SR"] rpc session_id)
    ; Client.PIF.(mk get_all get_all_records_where get_by_uuid pif_record "pif" [] ["uuid";"device";"VLAN";"mac";"network-uuid"; "currently-attached"] rpc session_id)
    ; Client.Bond.(mk get_all get_all_records_where get_by_uuid bond_record "bond" [] ["uuid";"master";"slaves"] rpc session_id)
    ; Client.VLAN.(mk get_all get_all_records_where get_by_uuid vlan_record "vlan" [] ["uuid";"tagged-PIF";"untagged-PIF"; "tag"] rpc session_id)
    ; Client.Tunnel.(mk get_all get_all_records_where get_by_uuid tunnel_record "tunnel" [] ["uuid";"transport-PIF";"access-PIF";"status"] rpc session_id)
    ; Client.VIF.(mk get_all get_all_records_where get_by_uuid vif_record "vif" [] ["uuid";"device";"vm-uuid";"network-uuid"] rpc session_id)
    ; Client.Network.(mk get_all get_all_records_where get_by_uuid net_record "network" [] ["uuid";"name-label";"name-description";"bridge"] rpc session_id)
    ; Client.Console.(mk get_all get_all_records_where get_by_uuid console_record "console" [] ["uuid";"vm-uuid";"vm-name-label";"protocol";"location"] rpc session_id)
    ; Client.VM.(mk get_all get_all_records_where get_by_uuid vm_record "vm" [("is-a-template","false")] ["name-label";"uuid";"power-state"] rpc session_id)
    ; Client.VM.(mk get_all get_all_records_where get_by_uuid vm_record "template" [("is-a-template","true");("is-a-snapshot","false")] ["name-label";"name-description";"uuid"] rpc session_id)
    ; Client.VM.(mk get_all get_all_records_where get_by_uuid vm_record "snapshot" [("is-a-snapshot","true")] ["name-label";"name-description";"uuid";"snapshot_of"; "snapshot_time"; "is-vmss-snapshot"] rpc session_id)
    ; Client.Host.(mk get_all get_all_records_where get_by_uuid host_record "host" [] ["uuid";"name-label";"name-description"] rpc session_id)
    ; Client.Host_cpu.(mk get_all get_all_records_where get_by_uuid host_cpu_record "host-cpu" [] ["uuid";"number";"vendor";"speed";"utilisation"] rpc session_id)
    ; Client.Host_crashdump.(mk get_all get_all_records_where get_by_uuid host_crashdump_record "host-crashdump" [] ["uuid";"host";"timestamp";"size"] rpc session_id)
    ; Client.Pool_patch.(mk get_all get_all_records_where get_by_uuid pool_patch_record "patch" [] ["uuid"; "name-label"; "name-description"; "size"; "hosts"; "after-apply-guidance"] rpc session_id)
    ; Client.Pool_update.(mk get_all get_all_records_where get_by_uuid pool_update_record "update" [] ["uuid"; "name-label"; "name-description"; "version"; "installation-size"; "hosts"; "after-apply-guidance"] rpc session_id)
    ; Client.VDI.(mk get_all get_all_records_where get_by_uuid vdi_record "vdi" [] ["uuid";"name-label";"name-description";"virtual-size";"read-only";"sharable";"sr-uuid"] rpc session_id)
    ; Client.VBD.(mk get_all get_all_records_where get_by_uuid vbd_record "vbd" [] ["uuid";"vm-uuid";"vm-name-label";"vdi-uuid";"device"; "empty"] rpc session_id)
    ; Client.SR.(mk get_all get_all_records_where get_by_uuid sr_record "sr" [] ["uuid";"name-label";"name-description";"host";"type";"content-type"] rpc session_id)
    ; Client.SM.(mk get_all get_all_records_where get_by_uuid sm_record "sm" [] ["uuid";"type"; "name-label";"name-description";"vendor"; "copyright"; "configuration"] rpc session_id)
    ; Client.PBD.(mk get_all get_all_records_where get_by_uuid pbd_record "pbd" [] ["uuid";"host-uuid";"sr-uuid";"device-config";"currently-attached"] rpc session_id)
    ; Client.Task.(mk get_all get_all_records_where get_by_uuid task_record "task" [] ["uuid";"name-label";"name-description";"status";"progress"] rpc session_id)
    ; Client.Subject.(mk get_all get_all_records_where get_by_uuid subject_record "subject" [] ["uuid";"subject-identifier";"other-config";"roles"] rpc session_id)
    ; Client.Role.(mk get_all (fun ~rpc ~session_id ~expr -> get_all_records_where ~rpc ~session_id ~expr:Xapi_role.expr_no_permissions)
                     get_by_uuid role_record "role" [] ["uuid";"name";"description";"subroles"] rpc session_id)
    ; Client.VMSS.(mk get_all get_all_records_where get_by_uuid vmss_record "vmss" [] ["uuid";"name-label";"name-description";"enabled";"type";"retained-snapshots";"frequency";"schedule";"last-run-time";"VMs"] rpc session_id)
    (* ; Client.Blob.(mk get_all get_all_records_where get_by_uuid blob_record "blob" [] ["uuid";"mime-type"] rpc session_id)
       		 *)
    ; Client.Message.(mk get_all get_all_records_where get_by_uuid message_record "message" [] [] rpc session_id)
    ; Client.Secret.(mk get_all get_all_records_where get_by_uuid secret_record "secret" [] [] rpc session_id)
    ; Client.VM_appliance.(mk get_all get_all_records_where get_by_uuid vm_appliance_record "appliance" [] [] rpc session_id)
    ; Client.PGPU.(mk get_all get_all_records_where get_by_uuid pgpu_record "pgpu" [] ["uuid";"vendor-name";"device-name";"gpu-group-uuid"] rpc session_id)
    ; Client.GPU_group.(mk get_all get_all_records_where get_by_uuid gpu_group_record "gpu-group" [] ["uuid";"name-label";"name-description"] rpc session_id)
    ; Client.VGPU.(mk get_all get_all_records_where get_by_uuid vgpu_record "vgpu" [] ["uuid";"vm-uuid";"device";"gpu-group-uuid"] rpc session_id)
    ; Client.VGPU_type.(mk get_all get_all_records_where get_by_uuid vgpu_type_record "vgpu-type" [] ["uuid";"vendor-name";"model-name";"max-resolution";"max-heads"] rpc session_id)
    ; Client.DR_task.(mk get_all get_all_records_where get_by_uuid dr_task_record "drtask" [] [] rpc session_id)
    (*; Client.Alert.(mk get_all get_all_records_where get_by_uuid alert_record "alert" [] ["uuid";"message";"level";"timestamp";"system";"task"] rpc session_id)
      		 *)
    ; Client.PVS_site.(mk get_all get_all_records_where get_by_uuid pvs_site_record "pvs-site" [] ["uuid"; "name-label"; "name-description"; "pvs-uuid"; "pvs-server-uuids"] rpc session_id)
    ; Client.PVS_server.(mk get_all get_all_records_where get_by_uuid pvs_server_record "pvs-server" [] ["uuid"; "addresses"; "pvs-site-uuid"] rpc session_id)
    ; Client.PVS_proxy.(mk get_all get_all_records_where get_by_uuid pvs_proxy_record "pvs-proxy" [] ["uuid"; "vif-uuid"; "pvs-site-uuid"; "currently-attached"; "cache-sr-uuid"] rpc session_id)
    ; Client.PVS_cache_storage.(mk get_all get_all_records_where get_by_uuid pvs_cache_storage_record "pvs-cache-storage" [] ["uuid"; "host-uuid"; "sr-uuid"; "pvs-site-uuid"; "size"] rpc session_id)
    ; Client.Feature.(mk get_all get_all_records_where get_by_uuid feature_record "feature" []
                        ["uuid"; "name-label"; "name-description"; "enabled"; "experimental"; "version"; "host-uuid"] rpc session_id)
    ; Client.SDN_controller.(mk get_all get_all_records_where get_by_uuid sdn_controller_record "sdn-controller" [] ["uuid"; "protocol"; "address"; "port"] rpc session_id)
    ; Client.PUSB.(mk get_all get_all_records_where get_by_uuid pusb_record "pusb" [] ["uuid"; "path"; "product-id"; "product-desc"; "vendor-id"; "vendor-desc"; "serial"; "version";"description"] rpc session_id)
    ; Client.USB_group.(mk get_all get_all_records_where get_by_uuid usb_group_record "usb-group" [] ["uuid";"name-label";"name-description"] rpc session_id)
    ; Client.VUSB.(mk get_all get_all_records_where get_by_uuid vusb_record "vusb" [] ["uuid";"vm-uuid"; "usb-group-uuid"] rpc session_id)
    ; Client.Cluster.(mk get_all get_all_records_where get_by_uuid cluster_record "cluster" [] ["uuid";"cluster_hosts";"network";"cluster_token";"cluster_stack";"allowed_operations";"current_operations";"pool_auto_join";"cluster_config";"other_config"] rpc session_id)
    ; Client.Cluster_host.(mk get_all get_all_records_where get_by_uuid cluster_host_record "cluster-host" [] ["uuid";"cluster";"host";"enabled";"allowed_operations";"current_operations";"other_config"] rpc session_id)
    ]

(* NB, might want to put these back in at some point
 * let zurich_params_gone =
 *   ["distribution";"distribution_vsn";"os";"boot_params"]
 *
 * let zurich_param_map =
 *   [("name","name-label");
 *    ("description","name-description");
 *    ("vcpus","vcpus-number");
 *    ("memory_set","memory-dynamic-max");]
*)

let message_create printer rpc session_id params =
  let body = List.assoc "body" params in
  let priority = try Int64.of_string (List.assoc "priority" params) with _ -> failwith "Priority field should be an integer" in
  let name = List.assoc "name" params in
  let uuid,cls =
    if (List.mem_assoc "vm-uuid" params) then
      List.assoc "vm-uuid" params, `VM
    else if (List.mem_assoc "pool-uuid" params) then
      List.assoc "pool-uuid" params, `Pool
    else if (List.mem_assoc "sr-uuid" params) then
      List.assoc "sr-uuid" params, `SR
    else if (List.mem_assoc "host-uuid" params) then
      List.assoc "host-uuid" params, `Host
    else
      raise (Cli_util.Cli_failure "Need one of: vm-uuid, host-uuid, sr-uuid or pool-uuid")
  in
  ignore(Client.Message.create rpc session_id name priority cls uuid body)

let message_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let message = Client.Message.get_by_uuid rpc session_id uuid in
  Client.Message.destroy rpc session_id message

(* Pool operations *)

let get_pool_with_default rpc session_id params key =
  if List.mem_assoc key params then
    (* User provided a pool uuid. *)
    let pool_uuid = List.assoc key params in
    Client.Pool.get_by_uuid rpc session_id pool_uuid
  else
    (* User didn't provide a pool uuid: let's fetch the default pool. *)
    List.hd (Client.Pool.get_all rpc session_id)

let pool_enable_binary_storage printer rpc session_id params =
  Client.Pool.enable_binary_storage rpc session_id

let pool_disable_binary_storage printer rpc session_id params =
  Client.Pool.disable_binary_storage rpc session_id

let pool_ha_enable printer rpc session_id params =
  let config = read_map_params "ha-config" params in
  let uuids = if List.mem_assoc "heartbeat-sr-uuids" params then String.split ',' (List.assoc "heartbeat-sr-uuids" params) else [] in
  let srs = List.map (fun uuid -> Client.SR.get_by_uuid rpc session_id uuid) uuids in
  Client.Pool.enable_ha rpc session_id srs config
let pool_ha_disable printer rpc session_id params =
  Client.Pool.disable_ha rpc session_id
let pool_ha_prevent_restarts_for printer rpc session_id params =
  let seconds = Int64.of_string (List.assoc "seconds" params) in
  Client.Pool.ha_prevent_restarts_for rpc session_id seconds
let pool_ha_compute_max_host_failures_to_tolerate printer rpc session_id params =
  let n = Client.Pool.ha_compute_max_host_failures_to_tolerate rpc session_id in
  printer (Cli_printer.PList [ Int64.to_string n ])
let pool_ha_compute_hypothetical_max_host_failures_to_tolerate printer rpc session_id params =
  (* Walk through the params in order constructing a VM -> restart_priority map *)
  let vms = List.map snd (List.filter (fun (k, _) -> k = "vm-uuid") params)
  and pri = List.map snd (List.filter (fun (k, _) -> k = "restart-priority") params) in
  if List.length vms <> (List.length pri) then failwith "Call requires an equal number of vm-uuid and restart-priority arguments";
  let vms = List.map (fun uuid -> Client.VM.get_by_uuid rpc session_id uuid) vms in
  let n = Client.Pool.ha_compute_hypothetical_max_host_failures_to_tolerate rpc session_id (List.combine vms pri) in
  printer (Cli_printer.PList [ Int64.to_string n ])

let pool_ha_compute_vm_failover_plan printer rpc session_id params =
  let host_uuids = String.split ',' (List.assoc "host-uuids" params) in
  let hosts = List.map (fun uuid -> Client.Host.get_by_uuid rpc session_id uuid) host_uuids in
  (* For now select all VMs resident on the given hosts *)
  let vms = List.concat (List.map (fun host -> Client.Host.get_resident_VMs rpc session_id host) hosts) in
  let vms = List.filter (fun vm -> not(Client.VM.get_is_control_domain rpc session_id vm)) vms in
  let plan = Client.Pool.ha_compute_vm_failover_plan rpc session_id hosts vms in
  let table = List.map (fun (vm, result) ->
      Printf.sprintf "%s (%s)" (Client.VM.get_uuid rpc session_id vm) (Client.VM.get_name_label rpc session_id vm),
      if List.mem_assoc "host" result then begin
        let host = Ref.of_string (List.assoc "host" result) in
        Printf.sprintf "%s (%s)" (Client.Host.get_uuid rpc session_id host) (Client.Host.get_name_label rpc session_id host)
      end else if List.mem_assoc "error_code" result then begin
        List.assoc "error_code" result
      end else "UNKNOWN") plan in
  printer (Cli_printer.PTable [ ("VM", "Destination Host or Error") :: table ])
let host_ha_xapi_healthcheck fd printer rpc session_id params =
  try
    let result = Client.Host.ha_xapi_healthcheck rpc session_id in
    if not(result) then begin
      marshal fd (Command (PrintStderr "Host.ha_xapi_healthcheck reports false\n"));
      raise (ExitWithError 2) (* comms failure exits with error 1 in the thin CLI itself *)
    end;
    marshal fd (Command (Print "xapi is healthy."))
  with e ->
    marshal fd (Command (PrintStderr (Printf.sprintf "Host.ha_xapi_healthcheck threw exception: %s\n" (ExnHelper.string_of_exn e))));
    raise (ExitWithError 3)

let pool_sync_database printer rpc session_id params =
  Client.Pool.sync_database rpc session_id

let pool_designate_new_master printer rpc session_id params =
  let host_uuid=List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  Client.Pool.designate_new_master rpc session_id host

let pool_management_reconfigure printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "network-uuid" params) in
  Client.Pool.management_reconfigure rpc session_id network

let pool_join printer rpc session_id params =
  try
    let force = get_bool_param params "force" in
    if force then
      Client.Pool.join_force ~rpc ~session_id
        ~master_address:(List.assoc "master-address" params)
        ~master_username:(List.assoc "master-username" params)
        ~master_password:(List.assoc "master-password" params)
    else
      Client.Pool.join ~rpc ~session_id
        ~master_address:(List.assoc "master-address" params)
        ~master_username:(List.assoc "master-username" params)
        ~master_password:(List.assoc "master-password" params);
    printer (Cli_printer.PList ["Host agent will restart and attempt to join pool in "^(string_of_float !Xapi_globs.fuse_time)^" seconds..."])
  with
  | Api_errors.Server_error(code, params) when code=Api_errors.pool_joining_host_connection_failed ->
    printer (Cli_printer.PList ["Host cannot contact destination host: connection refused.";
                                "Check destination host has services running and accessible from this host."])


let pool_eject fd printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host=Client.Host.get_by_uuid rpc session_id host_uuid in
  let force = get_bool_param params "force" in

  let go () =
    Client.Pool.eject ~rpc ~session_id ~host;
    printer (Cli_printer.PList ["Specified host will attempt to restart as a master of a new pool in "^(string_of_float !Xapi_globs.fuse_time)^" seconds..."]) in

  if force
  then go ()
  else begin

    (* Best-effort attempt to warn the user that VDIs in local SRs are going to be lost. *)
    let warnings =
      try
        (* Find local SRs *)
        let pbds = Client.Host.get_PBDs rpc session_id host in
        (* Find the subset of SRs which cannot be seen from other hosts *)
        let srs = List.concat
            (List.map
               (fun pbd ->
                  try
                    let sr = Client.PBD.get_SR rpc session_id pbd in
                    let other_pbds = Client.SR.get_PBDs rpc session_id sr in
                    let other_hosts = List.map (fun pbd -> Client.PBD.get_host rpc session_id pbd) other_pbds in
                    let other_hosts_than_me = List.filter (fun other -> other <> host) other_hosts in
                    if other_hosts_than_me = []
                    then [ sr ] else []
                  with _ -> []) pbds) in
        let warnings = ref [] in
        List.iter
          (fun sr ->
             try
               let vdis = Client.SR.get_VDIs rpc session_id sr in
               List.iter
                 (fun vdi ->
                    try
                      let uuid = Client.VDI.get_uuid rpc session_id vdi
                      and name_label = Client.VDI.get_name_label rpc session_id vdi in
                      warnings := Printf.sprintf "VDI: %s (%s)" uuid name_label :: !warnings
                    with _ -> ()
                 ) vdis
             with _ -> ()
          ) srs;
        !warnings
      with _ -> []
    in

    marshal fd (Command (Print "WARNING: Ejecting a host from the pool will reinitialise that host's local SRs."));
    marshal fd (Command (Print "WARNING: Any data contained with the local SRs will be lost."));
    if warnings <> [] then begin
      marshal fd (Command (Print "The following VDI objects will be destroyed:"));
      List.iter (fun msg -> marshal fd (Command (Print msg))) warnings
    end;
    if user_says_yes fd
    then go ()
  end


let pool_emergency_reset_master printer rpc session_id params =
  let master_address = List.assoc "master-address" params in
  Client.Pool.emergency_reset_master ~rpc ~session_id ~master_address;
  printer (Cli_printer.PList ["Host agent will restart and become slave of "^master_address^" in "^(string_of_float !Xapi_globs.fuse_time)^" seconds..."])

let pool_emergency_transition_to_master printer rpc session_id params =
  let force = get_bool_param params "force" in
  if not (Pool_role.is_master ()) || force then begin
    Client.Pool.emergency_transition_to_master ~rpc ~session_id;
    printer (Cli_printer.PList ["Host agent will restart and transition to master in "^(string_of_float !Xapi_globs.fuse_time)^" seconds..."])
  end
  else
    printer (Cli_printer.PList ["Host agent is already master. Use '--force' to execute the operation anyway."])

let pool_recover_slaves printer rpc session_id params =
  let hosts = Client.Pool.recover_slaves ~rpc ~session_id in
  let host_uuids = List.map (fun href -> Client.Host.get_uuid rpc session_id href) hosts in
  printer (Cli_printer.PList host_uuids)

let pool_initialize_wlb printer rpc session_id params =
  let wlb_url = List.assoc "wlb_url" params in
  let wlb_username = List.assoc "wlb_username" params in
  let wlb_password = List.assoc "wlb_password" params in
  let xenserver_username = List.assoc "xenserver_username" params in
  let xenserver_password = List.assoc "xenserver_password" params in
  Client.Pool.initialize_wlb ~rpc ~session_id ~wlb_url ~wlb_username ~wlb_password ~xenserver_username ~xenserver_password

let pool_deconfigure_wlb printer rpc session_id params =
  Client.Pool.deconfigure_wlb ~rpc ~session_id

let pool_send_wlb_configuration printer rpc session_id params =
  let len = String.length "config:" in
  let filter_params = List.filter (fun (p,_) -> (String.startswith "config" p) && (String.length p > len)) params in
  let config = List.map (fun (k,v) -> String.sub k len (String.length k - len),v) filter_params in
  Client.Pool.send_wlb_configuration ~rpc ~session_id ~config

let pool_retrieve_wlb_configuration printer rpc session_id params =
  printer (Cli_printer.PTable [(Client.Pool.retrieve_wlb_configuration ~rpc ~session_id)])

let pool_retrieve_wlb_recommendations printer rpc session_id params =
  let table t =
    List.map (fun (vm, recom) -> (Printf.sprintf "%s (%s)" (Client.VM.get_uuid rpc session_id vm) (Client.VM.get_name_label rpc session_id vm), String.concat " " recom)) t
  in
  printer (Cli_printer.PTable ([("VM", "Host, OptID, RecID, Reason") :: table (Client.Pool.retrieve_wlb_recommendations ~rpc ~session_id)]))

let pool_send_test_post printer rpc session_id params =
  let host = List.assoc "dest-host" params in
  let port = Int64.of_string (List.assoc "dest-port" params) in
  let body = List.assoc "body" params in
  printer (Cli_printer.PMsg
             (Client.Pool.send_test_post ~rpc ~session_id ~host ~port ~body))

let pool_certificate_install fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  match get_client_file fd filename with
  | Some cert ->
    Client.Pool.certificate_install ~rpc ~session_id
      ~name:(Filename.basename filename) ~cert
  | None ->
    marshal fd (Command (PrintStderr "Failed to read certificate\n"));
    raise (ExitWithError 1)

let pool_certificate_uninstall printer rpc session_id params =
  let name = List.assoc "name" params in
  Client.Pool.certificate_uninstall ~rpc ~session_id ~name

let pool_certificate_list printer rpc session_id params =
  printer (Cli_printer.PList
             (Client.Pool.certificate_list ~rpc ~session_id))

let pool_crl_install fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  match get_client_file fd filename with
  | Some cert ->
    Client.Pool.crl_install ~rpc ~session_id
      ~name:(Filename.basename filename) ~cert
  | None ->
    marshal fd (Command (PrintStderr "Failed to read CRL\n"));
    raise (ExitWithError 1)

let pool_crl_uninstall printer rpc session_id params =
  let name = List.assoc "name" params in
  Client.Pool.crl_uninstall ~rpc ~session_id ~name

let pool_crl_list printer rpc session_id params =
  printer (Cli_printer.PList
             (Client.Pool.crl_list ~rpc ~session_id))

let pool_certificate_sync printer rpc session_id params =
  Client.Pool.certificate_sync ~rpc ~session_id

let pool_enable_redo_log printer rpc session_id params =
  let sr = Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params) in
  Client.Pool.enable_redo_log ~rpc ~session_id ~sr

let pool_disable_redo_log printer rpc session_id params =
  Client.Pool.disable_redo_log ~rpc ~session_id

let pool_set_vswitch_controller printer rpc session_id params =
  let address = List.assoc "address" params in
  Client.Pool.set_vswitch_controller ~rpc ~session_id ~address

let pool_enable_ssl_legacy printer rpc session_id params =
  let self = get_pool_with_default rpc session_id params "uuid" in
  Client.Pool.enable_ssl_legacy ~rpc ~session_id ~self

let pool_disable_ssl_legacy printer rpc session_id params =
  let self = get_pool_with_default rpc session_id params "uuid" in
  Client.Pool.disable_ssl_legacy ~rpc ~session_id ~self

let vdi_type_of_string = function
  | "system" -> `system
  | "user" -> `user
  | "suspend" -> `suspend
  | "crashdump" -> `crashdump
  | x -> failwith (Printf.sprintf "Unknown vdi type: %s" x)

let vdi_create printer rpc session_id params =
  let sR = Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params) in
  let name_label=List.assoc "name-label" params in
  let virtual_size = Record_util.bytes_of_string "virtual-size" (List.assoc "virtual-size" params) in
  let ty =
    if List.mem_assoc "type" params
    then vdi_type_of_string (List.assoc "type" params)
    else `user in
  let sharable = get_bool_param params "sharable" in
  let sm_config=read_map_params "sm-config" params in
  let tags=read_set_params "tags" params in

  let vdi = Client.VDI.create ~rpc ~session_id ~name_label ~name_description:"" ~sR ~virtual_size ~_type:ty
      ~sharable ~read_only:false ~xenstore_data:[] ~other_config:[] ~sm_config ~tags in
  let vdi_uuid = Client.VDI.get_uuid rpc session_id vdi in
  printer (Cli_printer.PList [vdi_uuid])

let vdi_introduce printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let sR = Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params) in
  (* CA-13140: Some of the backends set their own name-labels, and the VDI introduce will
     	   not override them if we pass in the empty string.  *)
  let name_label = try List.assoc "name-label" params with _ -> "" in
  let name_description = if List.mem_assoc "name-description" params then List.assoc "name-description" params else "" in
  let _type = vdi_type_of_string (List.assoc "type" params) in
  let sharable = get_bool_param params "sharable" in
  let read_only = get_bool_param params "read-only" in
  (* NB call is new so backwards compat other-config- not required *)
  let other_config = read_map_params "other-config" params in
  let xenstore_data = read_map_params "xenstore-data" params in
  let sm_config = read_map_params "sm-config" params in
  let location = List.assoc "location" params in
  let managed = get_bool_param params "managed" in
  let virtual_size = 0L and physical_utilisation = 0L in
  let metadata_of_pool = Ref.null in
  let is_a_snapshot = false in
  let snapshot_time = Date.never in
  let snapshot_of = Ref.null in
  let vdi = Client.VDI.introduce ~rpc ~session_id ~uuid ~name_label ~name_description
      ~sR ~_type ~sharable ~read_only ~other_config ~location ~xenstore_data ~sm_config
      ~managed ~virtual_size ~physical_utilisation ~metadata_of_pool ~is_a_snapshot
      ~snapshot_time ~snapshot_of in
  (* round-trip catches partial application errors *)
  let vdi_uuid = Client.VDI.get_uuid ~rpc ~session_id ~self:vdi in
  printer (Cli_printer.PList [ vdi_uuid ])


let vdi_resize printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let new_size = Record_util.bytes_of_string "disk-size" (List.assoc "disk-size" params) in
  let online = List.mem_assoc "online" params && (List.assoc "online" params = "true") in
  if online
  then Client.VDI.resize_online rpc session_id vdi new_size
  else Client.VDI.resize rpc session_id vdi new_size

let vdi_generate_config printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
  printer (Cli_printer.PList [ Client.VDI.generate_config rpc session_id host vdi ])

let vdi_copy printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let base_vdi =
    if List.mem_assoc "base-vdi-uuid" params
    then Client.VDI.get_by_uuid rpc session_id (List.assoc "base-vdi-uuid" params)
    else Ref.null in
  let sr, into = match List.mem_assoc "sr-uuid" params, List.mem_assoc "into-vdi-uuid" params with
    | false, false
    | true, true ->
      failwith "Please specify one but not both of: a destination sr-uuid (I will create a fresh VDI); or a destination into-vdi-uuid (I will copy the blocks into this VDI)"
    | true, false ->
      Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params), Ref.null
    | false, true ->
      Ref.null, Client.VDI.get_by_uuid rpc session_id (List.assoc "into-vdi-uuid" params) in
  let newvdi = Client.VDI.copy rpc session_id vdi sr base_vdi into in
  let newuuid = Client.VDI.get_uuid rpc session_id newvdi in
  printer (Cli_printer.PList [newuuid])

let vdi_pool_migrate printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params)
  and sr = Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params)
  and options = [] (* no options implemented yet *)
  in
  let newvdi = Client.VDI.pool_migrate rpc session_id vdi sr options in
  let newuuid = Client.VDI.get_uuid rpc session_id newvdi in
  printer (Cli_printer.PList [newuuid])

let vdi_clone printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let driver_params = read_map_params "driver-params" params in
  let name_label = try Some (List.assoc "new-name-label" params) with Not_found -> None in
  let name_description = try Some (List.assoc "new-name-description" params) with Not_found -> None in
  let newvdi = Client.VDI.clone rpc session_id vdi driver_params in
  maybe (fun x -> Client.VDI.set_name_label rpc session_id newvdi x) name_label;
  maybe (fun x -> Client.VDI.set_name_description rpc session_id newvdi x) name_description;
  let newuuid = Client.VDI.get_uuid rpc session_id newvdi in
  printer (Cli_printer.PList [newuuid])

let vdi_snapshot printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let driver_params = read_map_params "driver-params" params in
  let newvdi = Client.VDI.snapshot rpc session_id vdi driver_params in
  let newuuid = Client.VDI.get_uuid rpc session_id newvdi in
  printer (Cli_printer.PList [newuuid])

let vdi_destroy printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.destroy rpc session_id vdi

let vdi_forget printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.forget rpc session_id vdi

let vdi_update printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.update rpc session_id vdi

let vdi_unlock printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  if not(List.mem_assoc "force" params)
  then failwith "This operation is extremely dangerous and may cause data loss. This operation must be forced (use --force).";
  Client.VDI.force_unlock rpc session_id vdi

let vdi_enable_cbt printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.enable_cbt rpc session_id vdi

let vdi_disable_cbt printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.disable_cbt rpc session_id vdi

let vdi_data_destroy printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.VDI.data_destroy rpc session_id vdi

let vdi_list_changed_blocks socket _ rpc session_id params =
  let vdi_from = Client.VDI.get_by_uuid rpc session_id (List.assoc "vdi-from-uuid" params) in
  let vdi_to = Client.VDI.get_by_uuid rpc session_id (List.assoc "vdi-to-uuid" params) in
  let bitmap = Client.VDI.list_changed_blocks ~rpc ~session_id ~vdi_from ~vdi_to in
  marshal socket (Command (Print bitmap))

let diagnostic_vdi_status printer rpc session_id params =
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let vdi_r = vdi_record rpc session_id vdi in
  let vdi_fields = List.filter
      (fun x -> List.mem x.name [ "uuid"; "name-label"; "sr-uuid"; "mode"; "read-only"; "sharable"; "storage-lock" ]) vdi_r.fields in
  printer (Cli_printer.PTable [List.map print_field vdi_fields]);
  let all_vbds = Client.VDI.get_VBDs rpc session_id vdi in
  let all_vbd_records = List.map (vbd_record rpc session_id) all_vbds in
  let active_records = List.filter (fun x -> (field_lookup (x.fields) "currently-attached").get() = "true") all_vbd_records in
  let inactive_records = List.set_difference all_vbd_records active_records in
  let show_vbds records =
    List.iter (fun vbd_record ->
        let fields = List.filter (fun x -> List.mem x.name [ "uuid"; "userdevice"; "device"; "empty"; "mode"; "type"; "storage-lock" ]) vbd_record.fields in
        printer (Cli_printer.PTable [List.map print_field fields])) records in
  if active_records = []
  then printer (Cli_printer.PList [ "no active VBDs." ])
  else begin
    printer (Cli_printer.PList [ "active VBDs:" ]);
    show_vbds active_records;
  end;
  if inactive_records <> [] then begin
    printer (Cli_printer.PList [ "inactive VBDs:" ]);
    show_vbds inactive_records
  end

(* Print a table of hosts, reporting whether a VM can start on each host and if not, why not! *)
let print_assert_exception e =
  let rec get_arg n xs =
    match n,xs with
      1,x::_ -> x
    | n,_::xs -> get_arg (n-1) xs
    | _ -> "<server did not provide reference>" in
  match e with
    Api_errors.Server_error(code, params) when code=Api_errors.vm_requires_sr ->
    "VM requires access to SR: "^(Cli_util.ref_convert (get_arg 2 params))
  | Api_errors.Server_error(code, params) when code=Api_errors.host_disabled ->
    "Host disabled (use 'xe host-enable' to re-enable)"
  | Api_errors.Server_error(code, params) when code=Api_errors.host_not_live ->
    "Host down"
  | Api_errors.Server_error(code, params) when code=Api_errors.host_not_enough_free_memory ->
    Printf.sprintf "Not enough free memory"
  | Api_errors.Server_error(code, params) when code=Api_errors.vm_requires_net ->
    "VM requires access to network: "^(Cli_util.ref_convert (get_arg 2 params))
  | Api_errors.Server_error(code, params) when code=Api_errors.host_cannot_attach_network ->
    "Host cannot attach to network: "^(Cli_util.ref_convert (get_arg 2 params))
  | Api_errors.Server_error(code, params) when code=Api_errors.vm_hvm_required ->
    "HVM not supported"
  | Api_errors.Server_error(code, [key; v] ) when code=Api_errors.invalid_value ->
    Printf.sprintf "Field has invalid value: %s = %s" key v

  (* Used by VM.assert_agile: *)
  | Api_errors.Server_error(code, [ sr ]) when code=Api_errors.ha_constraint_violation_sr_not_shared ->
    Printf.sprintf "VM requires access to non-shared SR: %s. SR must both be marked as shared and a properly configured PBD must be plugged-in on every host" (Cli_util.ref_convert sr)
  | Api_errors.Server_error(code, [ net]) when code = Api_errors.ha_constraint_violation_network_not_shared ->
    Printf.sprintf "VM requires access to non-shared Network: %s. Network must either be entirely virtual or there must be a PIF connecting to this Network on every host." (Cli_util.ref_convert net)

  | e -> Printexc.to_string e

let print_vm_host_report printer rpc session_id vm_ref =
  let hosts = Client.Host.get_all rpc session_id in
  let table = List.map (fun host -> Client.Host.get_name_label rpc session_id host,
                                    try Client.VM.assert_can_boot_here rpc session_id vm_ref host; "OK"
                                    with e -> "Cannot start here ["^(print_assert_exception e)^"]") hosts in
  printer (Cli_printer.PTable [table])

let diagnostic_vm_status printer rpc session_id params =
  let vm = Client.VM.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let vm_r = vm_record rpc session_id vm in
  let vm_fields = List.filter
      (fun x -> List.mem x.name [ "uuid"; "name-label"; "power-state"; "possible-hosts"]) vm_r.fields in

  printer (Cli_printer.PTable [List.map print_field vm_fields]);
  printer (Cli_printer.PList [ "Checking to see whether disks are attachable" ]);
  let show_vbds records =
    List.iter (fun vbd_record ->
        let fields = List.filter (fun x -> List.mem x.name [ "uuid"; "userdevice"; "device"; "vdi-uuid"; "empty"; "mode"; "type"; "storage-lock"; "attachable" ]) vbd_record.fields in
        printer (Cli_printer.PTable [List.map print_field fields])) records in
  let all_vbds = Client.VM.get_VBDs rpc session_id vm in
  let all_vbd_records = List.map (vbd_record rpc session_id) all_vbds in
  show_vbds all_vbd_records;
  printer (Cli_printer.PList [ "Checking to see whether VM can boot on each host" ]);
  print_vm_host_report printer rpc session_id vm;
  printer (Cli_printer.PList [
      try Client.VM.assert_agile rpc session_id vm; "VM is agile."
      with e -> "VM is not agile because: " ^ (print_assert_exception e)  ])

(* VBD create destroy list param-list param-get param-set param-add param-remove *)

let vbd_create printer rpc session_id params =
  let vM=Client.VM.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "vm-uuid" params) in
  let empty = not(List.mem_assoc "vdi-uuid" params) in
  let vDI =
    if empty
    then Ref.null
    else Client.VDI.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "vdi-uuid" params) in
  let bootable = get_bool_param params "bootable" in
  let mode =
    if List.mem_assoc "mode" params
    then match String.lowercase_ascii (List.assoc "mode" params) with
      | "ro" -> `RO | "rw" -> `RW
      | x -> failwith (Printf.sprintf "Unknown mode: %s (should be \"ro\" or \"rw\"" x)
    else `RW in
  let _type =
    if List.mem_assoc "type" params
    then match String.lowercase_ascii (List.assoc "type" params) with
      | "cd" -> `CD | "disk" -> `Disk
      | x -> failwith (Printf.sprintf "Unknown type: %s (should be \"cd\" or \"disk\"" x)
    else `Disk in
  let unpluggable = get_bool_param params ~default:true "unpluggable" in
  if _type=`Disk && empty then failwith "Empty VBDs can only be made for type=CD";
  let vbd=Client.VBD.create ~rpc ~session_id ~vM ~vDI ~userdevice:(List.assoc "device" params)
      ~bootable
      ~mode
      ~_type
      ~unpluggable
      ~empty
      ~qos_algorithm_type:""
      ~qos_algorithm_params:[] ~other_config:[] in
  let vbd_uuid=Client.VBD.get_uuid rpc session_id vbd in
  printer (Cli_printer.PList [vbd_uuid])

let vbd_destroy printer rpc session_id params =
  let self = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  Client.VBD.destroy ~rpc ~session_id ~self

let vbd_eject printer rpc session_id params =
  let self = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  Client.VBD.eject rpc session_id self

let vbd_insert printer rpc session_id params =
  let self = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  let vdi_uuid = List.assoc "vdi-uuid" params in
  let vdi = Client.VDI.get_by_uuid rpc session_id vdi_uuid in
  Client.VBD.insert rpc session_id self vdi

let vbd_plug printer rpc session_id params =
  let vbd = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  Client.VBD.plug rpc session_id vbd

let vbd_unplug printer rpc session_id params =
  let vbd = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  let timeout =
    if List.mem_assoc "timeout" params then
      (try float_of_string (List.assoc "timeout" params) with _ -> failwith "Failed to parse parameter 'timeout': expecting a float")
    else 0. in
  let force = get_bool_param params "force" in
  let start = Unix.gettimeofday () in
  try
    (if force then Client.VBD.unplug_force else Client.VBD.unplug) rpc session_id vbd
  with Api_errors.Server_error(code, _) as e when code = Api_errors.device_detach_rejected ->
    (* enter polling mode *)
    let unplugged = ref false in
    while not(!unplugged) && (Unix.gettimeofday () -. start < timeout) do
      Thread.delay 5.;
      unplugged := not(Client.VBD.get_currently_attached rpc session_id vbd)
    done;
    if not(!unplugged) then raise e

let vbd_pause printer rpc session_id params =
  let vbd = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  let token = Client.VBD.pause rpc session_id vbd in
  printer (Cli_printer.PList [token])

let vbd_unpause printer rpc session_id params =
  let vbd = Client.VBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  let token = List.assoc "token" params in
  Client.VBD.unpause rpc session_id vbd token

(* SR scan *)
let sr_scan printer rpc session_id params =
  let sr_uuid = List.assoc "uuid" params in
  let sr_ref = Client.SR.get_by_uuid rpc session_id sr_uuid in
  Client.SR.scan rpc session_id sr_ref

let parse_host_uuid ?(default_master=true) rpc session_id params =
  if List.mem_assoc "host-uuid" params then
    let host_uuid=List.assoc "host-uuid" params in
    Client.Host.get_by_uuid rpc session_id host_uuid
  else begin
    let hosts = Client.Host.get_all rpc session_id in
    let standalone = List.length hosts = 1 in
    if standalone || default_master
    then
      let pool = List.hd (Client.Pool.get_all rpc session_id) in
      Client.Pool.get_master rpc session_id pool
    else failwith "Required parameter not found: host-uuid"
  end

let parse_device_config params =
  (* Ack! We're supposed to use the format device-config:key=value but we need to match device-config-key=value for *)
  (* backwards compatability *)
  let len = String.length "device-config:" in
  let filter_params = List.filter (fun (p,_) -> (String.startswith "device-config" p) && (String.length p > len)) params in
  List.map (fun (k,v) -> String.sub k len (String.length k - len),v) filter_params

(* SR create destroy list param-list param-get param-set param-add param-remove *)

let sr_create fd printer rpc session_id params =
  let name_label=List.assoc "name-label" params in
  let shared = get_bool_param params "shared" in
  let host = parse_host_uuid ~default_master:shared rpc session_id params in
  let physical_size=
    try
      Record_util.bytes_of_string "physical-size" (List.assoc "physical-size" params)
    with _ -> 0L in
  let _type=List.assoc "type" params in
  let content_type = List.assoc_default "content-type" params "" in
  let device_config = parse_device_config params in
  (* If the device-config parameter is of the form k-filename=v, then we assume the
     	   key is 'k' and the value is stored in a file named 'v' *)
  let suffix = "-filename" in
  let device_config = List.map (fun (k,v) ->
      if String.endswith suffix k then begin
        let k = String.sub k 0 (String.length k - (String.length suffix)) in
        match get_client_file fd v with
        | Some v -> k,v
        | None ->
          marshal fd (Command(PrintStderr (Printf.sprintf "File not found: %s" v)));
          failwith "File not found"
      end else (k, v)
    ) device_config in
  let sm_config = read_map_params "sm-config" params in
  let sr=Client.SR.create ~rpc ~session_id ~host ~device_config ~name_label
      ~name_description:""
      ~physical_size ~_type ~content_type ~shared:shared ~sm_config in
  let sr_uuid=Client.SR.get_uuid ~rpc ~session_id ~self:sr in
  marshal fd (Command (Print sr_uuid))

let sr_introduce printer rpc session_id params =
  let name_label=List.assoc "name-label" params in
  let _type=List.assoc "type" params in
  let content_type = List.assoc_default "content-type" params "" in
  let uuid = List.assoc "uuid" params in
  let shared = get_bool_param params "shared" in
  let sm_config = read_map_params "sm-config" params in
  let _ = Client.SR.introduce ~rpc ~session_id ~uuid ~name_label ~name_description:"" ~_type ~content_type ~shared ~sm_config in
  printer (Cli_printer.PList [uuid])

let sr_probe printer rpc session_id params =
  let host = parse_host_uuid rpc session_id params in
  let _type = List.assoc "type" params in
  let device_config = parse_device_config params in
  let sm_config = read_map_params "sm-config" params in
  let txt = Client.SR.probe ~rpc ~session_id ~host ~_type ~device_config ~sm_config in
  try
    (* If it's the new format, try to print it more nicely *)
    let open Storage_interface in
    match probe_result_of_rpc (Xmlrpc.of_string txt) with
    | Raw x -> printer (Cli_printer.PList [ x ])
    | Probe x ->
      let sr (uri, x) = [
        "uri", uri;
        "name-label", x.name_label;
        "name-description", x.name_description;
        "total-space", Int64.to_string x.total_space;
        "free-space", Int64.to_string x.free_space;
      ] in
      if x.srs <> []
      then printer (Cli_printer.PMsg "The following SRs were found:");
      printer (Cli_printer.PTable (List.map sr x.srs));
      if x.uris <> []
      then printer (Cli_printer.PMsg "The following URIs may contain SRs:");
      printer (Cli_printer.PList x.uris)
  with _ ->
    printer (Cli_printer.PList [txt])

let sr_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let sr = Client.SR.get_by_uuid rpc session_id uuid in
  Client.SR.destroy rpc session_id sr

let sr_forget printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let sr = Client.SR.get_by_uuid rpc session_id uuid in
  Client.SR.forget rpc session_id sr

let sr_update printer rpc session_id params =
  let sr = Client.SR.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.SR.update rpc session_id sr

let sr_enable_database_replication printer rpc session_id params =
  let sr = Client.SR.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.SR.enable_database_replication rpc session_id sr

let sr_disable_database_replication printer rpc session_id params =
  let sr = Client.SR.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.SR.disable_database_replication rpc session_id sr

(* PIF destroy* list param-list param-get param-set param-add param-remove *)

let pbd_create printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let sr_uuid = List.assoc "sr-uuid" params in

  (* Ack! We're supposed to use the format device-config:key=value but we need to match device-config-key=value for *)
  (* backwards compatability *)
  let len = String.length "device-config:" in
  let filter_params = List.filter (fun (p,_) -> (String.startswith "device-config" p) && (String.length p > len)) params in
  let device_config = List.map (fun (k,v) -> String.sub k len (String.length k - len),v) filter_params in

  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let sr = Client.SR.get_by_uuid rpc session_id sr_uuid in
  let pbd = Client.PBD.create rpc session_id host sr device_config [] in
  let uuid = Client.PBD.get_uuid rpc session_id pbd in
  printer (Cli_printer.PList [uuid])

let pbd_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let pbd = Client.PBD.get_by_uuid rpc session_id uuid in
  Client.PBD.destroy rpc session_id pbd

let pbd_plug printer rpc session_id params =
  let pbd = Client.PBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  Client.PBD.plug rpc session_id pbd

let pbd_unplug printer rpc session_id params =
  let pbd = Client.PBD.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
  Client.PBD.unplug rpc session_id pbd

let vif_create printer rpc session_id params =
  let device = List.assoc "device" params in
  let network_uuid = List.assoc "network-uuid" params in
  let vm_uuid=List.assoc "vm-uuid" params in
  let mac=List.assoc_default "mac" params "" in
  let mac=if mac="random" then (Record_util.random_mac_local ()) else mac in
  let vm=Client.VM.get_by_uuid rpc session_id vm_uuid in
  let network=Client.Network.get_by_uuid rpc session_id network_uuid in
  let mtu = Client.Network.get_MTU rpc session_id network in
  let vif = Client.VIF.create rpc session_id device network vm mac mtu [] "" [] `network_default [] [] in
  let uuid = Client.VIF.get_uuid rpc session_id vif in
  printer (Cli_printer.PList [uuid])

let vif_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vif = Client.VIF.get_by_uuid rpc session_id uuid in
  Client.VIF.destroy rpc session_id vif

let vif_plug printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vif = Client.VIF.get_by_uuid rpc session_id uuid in
  Client.VIF.plug rpc session_id vif

let vif_unplug printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vif = Client.VIF.get_by_uuid rpc session_id uuid in
  let force = get_bool_param params "force" in
  (if force then Client.VIF.unplug_force else Client.VIF.unplug) rpc session_id vif

let vif_configure_ipv4 printer rpc session_id params =
  let vif = Client.VIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let mode = Record_util.vif_ipv4_configuration_mode_of_string (List.assoc "mode" params) in
  let address = List.assoc_default "address" params "" in
  let gateway = List.assoc_default "gateway" params "" in
  if mode = `Static && address = "" then failwith "Required parameter not found: address";
  Client.VIF.configure_ipv4 rpc session_id vif mode address gateway

let vif_configure_ipv6 printer rpc session_id params =
  let vif = Client.VIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let mode = Record_util.vif_ipv6_configuration_mode_of_string (List.assoc "mode" params) in
  let address = List.assoc_default "address" params "" in
  let gateway = List.assoc_default "gateway" params "" in
  if mode = `Static && address = "" then failwith "Required parameter not found: address";
  Client.VIF.configure_ipv6 rpc session_id vif mode address gateway

let vif_move printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let network_uuid = List.assoc "network-uuid" params in
  let vif = Client.VIF.get_by_uuid rpc session_id uuid in
  let network = Client.Network.get_by_uuid rpc session_id network_uuid in
  Client.VIF.move rpc session_id vif network

let net_create printer rpc session_id params =
  let network = List.assoc "name-label" params in
  let descr = List.assoc_default "name-description" params "" in
  let mtu = if List.mem_assoc "MTU" params then Int64.of_string (List.assoc "MTU" params) else 1500L in
  let bridge = List.assoc_default "bridge" params "" in
  let managed = get_bool_param params ~default:true "managed" in
  let net = Client.Network.create rpc session_id network descr mtu [] bridge managed [] in
  let uuid = Client.Network.get_uuid rpc session_id net in
  printer (Cli_printer.PList [uuid])

let net_destroy printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  ignore(Client.Network.destroy rpc session_id network)

let net_attach printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
  let () = Client.Network.attach rpc session_id network host in ()

let vm_create printer rpc session_id params =
  let name_label=List.assoc "name-label" params in
  let name_description=List.assoc_default "name-description" params "" in
  let ( ** ) = Int64.mul in
  let mib = 1024L ** 1024L in
  let memory_max = 256L ** mib in
  let memory_min = 128L ** mib in
  let vm = Client.VM.create ~rpc ~session_id ~name_label ~name_description ~user_version:0L ~is_a_template:false
      ~blocked_operations:[]
      ~affinity:Ref.null
      ~memory_target:memory_max
      ~memory_static_max:memory_max
      ~memory_dynamic_max:memory_max
      ~memory_dynamic_min:memory_min
      ~memory_static_min:memory_min
      ~vCPUs_params:[] ~vCPUs_max:1L ~vCPUs_at_startup:1L
      ~actions_after_shutdown:`destroy ~actions_after_reboot:`restart ~actions_after_crash:`destroy ~pV_bootloader:""
      ~pV_kernel:"" ~pV_ramdisk:"" ~pV_args:"" ~pV_bootloader_args:"" ~pV_legacy_args:"" ~hVM_boot_policy:""
      ~hVM_boot_params:[] ~hVM_shadow_multiplier:1. ~platform:[] ~pCI_bus:"" ~other_config:[] ~xenstore_data:[] ~recommendations:"" ~ha_always_run:false ~ha_restart_priority:""
      ~tags:[] ~protection_policy:Ref.null ~is_snapshot_from_vmpp:false
      ~snapshot_schedule:Ref.null ~is_vmss_snapshot:false
      ~appliance:Ref.null
      ~start_delay:0L
      ~shutdown_delay:0L
      ~order:0L
      ~suspend_SR:Ref.null
      ~version:0L
      ~generation_id:""
      ~hardware_platform_version:0L
      ~has_vendor_device:false ~reference_label:""
  in
  let uuid=Client.VM.get_uuid rpc session_id vm in
  printer (Cli_printer.PList [uuid])

let vm_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vm = Client.VM.get_by_uuid rpc session_id uuid in
  if (Client.VM.get_is_control_domain rpc session_id vm) then
    raise (Api_errors.Server_error (Api_errors.operation_not_allowed,
                                    ["You cannot destroy a control domain via the CLI"]))
  else
    Client.VM.destroy rpc session_id vm


(* Event *)
(*
  let dodiff fd orig_values tbl reference =
  let (r,orig_tbl) = try List.find (fun (x,x_rec) -> x=reference) orig_values with _ -> ("",[]) in
  let changed = List.filter (fun (n,v) -> try v <> List.assoc n orig_tbl with _ -> true) tbl in
  List.iter (fun (n,v) -> marshal fd (Command (Print (Printf.sprintf "%s: %s\n" n v)))) changed

  let diagnostic_event_deltas fd printer rpc session_id params =
  let classes=[List.assoc "class" params] in
  let orig = match List.hd classes with
  | "vm" -> List.map (fun vm -> (Ref.string_of vm),(vm_record rpc session_id vm).fields) (Client.VM.get_all rpc session_id)
  | "vdi" -> List.map (fun vdi -> (Ref.string_of vdi),(vdi_record rpc session_id vdi).fields) (Client.VDI.get_all rpc session_id)
  | "sr" -> List.map (fun sr -> (Ref.string_of sr),(sr_record rpc session_id sr).fields) (Client.SR.get_all rpc session_id)
  | _ -> []
  in
  let orig_values = List.map (fun (r,x) -> (r,List.map (fun r -> (r.name,safe_get_field r)) x)) orig in
  Client.Event.register ~rpc ~session_id ~classes;
  while true do
  let events = Event_types.events_of_xmlrpc (Client.Event.next ~rpc ~session_id) in
  marshal fd (Command (Print (Printf.sprintf "Got %d event(s)!\n" (List.length events))));
  let doevent event =
  let tbl = match Event_helper.record_of_event event with
(*	| Event_helper.VM x -> let record = vm_record rpc session_id (Ref.of_string event.Event_types.reference) in record.set_ref x; tbl
  | Event_helper.VDI x -> let record = vdi_record rpc session_id (Ref.of_string event.Event_types.reference) in f x; tbl
  | Event_helper.SR x -> let record = sr_record rpc session_id (Ref.of_string event.Event_types.reference) in f x; tbl*)
  | _ -> failwith "bah!"
  in
  let record = List.map (fun r -> (r.name,safe_get_field r)) tbl in
  let reference = event.Event_types.reference in
  dodiff fd orig_values record reference
  in
  List.iter doevent events
  done
 *)

exception Finished

let event_wait_gen rpc session_id classname record_matches =
  (* Immediately register *)
  let classes = [classname] in
  Client.Event.register ~rpc ~session_id ~classes;

  debug "Registered for events";

  (* Check to see if the condition is already satisfied - get all objects of whatever class specified... *)
  let poll () =
    let current_tbls =
      match classname with
      | "vm" -> List.map (fun x -> (vm_record rpc session_id x).fields) (Client.VM.get_all rpc session_id)
      | "vdi" -> List.map (fun x -> (vdi_record rpc session_id x).fields) (Client.VDI.get_all rpc session_id)
      | "sr" -> List.map (fun x -> (sr_record rpc session_id x).fields) (Client.SR.get_all rpc session_id)
      | "host" -> List.map (fun x -> (host_record rpc session_id x).fields) (Client.Host.get_all rpc session_id)
      | "network" -> List.map (fun x -> (net_record rpc session_id x).fields) (Client.Network.get_all rpc session_id)
      | "vif" -> List.map (fun x -> (vif_record rpc session_id x).fields) (Client.VIF.get_all rpc session_id)
      | "pif" -> List.map (fun x -> (pif_record rpc session_id x).fields) (Client.PIF.get_all rpc session_id)
      | "vbd" -> List.map (fun x -> (vbd_record rpc session_id x).fields) (Client.VBD.get_all rpc session_id)
      | "pbd" -> List.map (fun x -> (pbd_record rpc session_id x).fields) (Client.PBD.get_all rpc session_id)
      | "pool" -> List.map (fun x -> (pool_record rpc session_id x).fields) (Client.Pool.get_all rpc session_id)
      | "task" -> List.map (fun x -> (task_record rpc session_id x).fields) (Client.Task.get_all rpc session_id)
      | "subject" -> List.map (fun x -> (subject_record rpc session_id x).fields) (Client.Subject.get_all rpc session_id)
      | "role" -> List.map (fun x -> (role_record rpc session_id x).fields) (Client.Role.get_all rpc session_id)
      | "secret" -> List.map (fun x -> (secret_record rpc session_id x).fields) (Client.Secret.get_all rpc session_id)
      | "vmss" -> List.map (fun x -> (vmss_record rpc session_id x).fields) (Client.VMSS.get_all rpc session_id)
      (*				| "alert" -> List.map (fun x -> (alert_record rpc session_id x).fields) (Client.Alert.get_all rpc session_id) *)
      | _ -> failwith ("Cli listening for class '"^classname^"' not currently implemented")
    in

    debug "Getting all records";
    (* Records of every object of the class specified *)
    let all_recs = List.map (List.map (fun r -> (r.name,(fun () -> safe_get_field r)))) current_tbls in

    debug "Got %d records" (List.length all_recs);

    (* true if anything matches now *)
    let find_any_match recs =
      let ls = List.map record_matches recs in
      (List.length (List.filter (fun x -> x) ls)) > 0
    in
    find_any_match all_recs
  in

  finally
    (fun () ->
       if not(poll ()) then
         try
           while true do
             try
               let events = Event_types.events_of_rpc (Client.Event.next ~rpc ~session_id) in
               let doevent event =
                 let tbl = match Event_helper.record_of_event event with
                   | Event_helper.VM (r,Some x) -> let record = vm_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.VDI (r,Some x) -> let record = vdi_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.SR (r,Some x) -> let record = sr_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.Host (r,Some x) -> let record = host_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.Network (r,Some x) -> let record = net_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.VIF (r,Some x) -> let record = vif_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.PIF (r,Some x) -> let record = pif_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.VBD (r,Some x) -> let record = vbd_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.PBD (r,Some x) -> let record = pbd_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.Pool (r,Some x) -> let record = pool_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.Task (r,Some x) -> let record = task_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.VMSS (r,Some x) -> let record = vmss_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | Event_helper.Secret (r,Some x) -> let record = secret_record rpc session_id r in record.setrefrec (r,x); record.fields
                   | _ -> failwith ("Cli listening for class '"^classname^"' not currently implemented")
                 in
                 let record = List.map (fun r -> (r.name,fun () -> safe_get_field r)) tbl in
                 if record_matches record then raise Finished
               in
               List.iter doevent (List.filter (fun e -> e.Event_types.snapshot <> None) events)
             with Api_errors.Server_error(code, _) when code = Api_errors.events_lost ->
               debug "Got EVENTS_LOST; reregistering";
               Client.Event.unregister ~rpc ~session_id ~classes;
               Client.Event.register ~rpc ~session_id ~classes;
               if poll() then raise Finished
           done
         with Finished -> ()
    ) (fun () -> Client.Event.unregister ~rpc ~session_id ~classes) (* We're done. Unregister and finish *)


let event_wait printer rpc session_id params =
  let classname=List.assoc "class" params in
  let filter_params = List.filter (fun (p,_) -> not (List.mem p ("class"::stdparams))) params in

  (* Each filter_params is a key value pair:
     	   (key, value) if the user entered "key=value"
     	   (key, "/=" value) if the user entered "key=/=value"
     	   We now parse these into a slightly nicer form *)

  let filter_params = List.map
      (fun (key, value) ->
         if String.startswith "/=" value then begin
           let key' = key in
           let value' = String.sub value 2 (String.length value - 2) in
           `NotEquals, key', value'
         end else begin
           `Equals, key, value
         end) filter_params in

  (* This returns true if the record matches the cmd line constraints *)
  let record_matches record =
    let matches = List.map (fun (operator, p,v) ->
        if not(List.mem_assoc p record)
        then failwith (Printf.sprintf "key missing: %s" p);
        let v' = List.assoc p record () in match operator with
        | `NotEquals -> v <> v'
        | `Equals -> v = v') filter_params in
    alltrue matches
  in
  event_wait_gen rpc session_id classname record_matches



(* TASK list *)



(* Convenience functions *)
let select_vms ?(include_control_vms = false) ?(include_template_vms = false) rpc session_id params ignore_params =
  (* Make sure we don't select a template or control domain by mistake *)
  let params = if not include_control_vms  then ("is-control-domain", "false") :: params else params in
  let params = if not include_template_vms then ("is-a-template"    , "false") :: params else params in
  let vm_name_or_ref = try Some (
      (* Escape every quote character *)
      List.assoc "vm" params |> String.replace "\"" "\\\""
    ) with _ -> None in
  let params, where_clause = match vm_name_or_ref with
    | None -> params, "true"
    | Some v -> (
        (* try matching vm=<name or uuid> *)
        List.remove_assoc "vm" params,
        Printf.sprintf "(field \"uuid\"=\"%s\") or (field \"name__label\"=\"%s\")" v v
      )
  in
  let vms = Client.VM.get_all_records_where rpc session_id where_clause in
  let all_recs = List.map (fun (vm,vm_r) -> let r = vm_record rpc session_id vm in r.setrefrec (vm,vm_r); r) vms in
  (* Filter on everything on the cmd line except params=... *)
  let filter_params = List.filter (fun (p,_) ->
      let p' =
        try
          let i = String.index p ':' in
          String.sub p 0 i
        with Not_found -> p
      in
      not (List.mem p' (stdparams @ ignore_params))
    ) params in
  (* Filter all the records *)
  List.fold_left filter_records_on_fields all_recs filter_params


let select_hosts rpc session_id params ignore_params =
  let host_name_or_ref = try Some (
      (* Escape every quote character *)
      List.assoc "host" params |> Stdext.Xstringext.String.replace "\"" "\\\""
    ) with _ -> None in
  let params, where_clause = match host_name_or_ref with
    | None -> params, "true"
    | Some v -> (
        (* try matching host=<name or uuid> *)
        List.remove_assoc "host" params,
        Printf.sprintf "(field \"uuid\"=\"%s\") or (field \"hostname\"=\"%s\") or (field \"name__label\"=\"%s\")" v v v
      )
  in
  let hosts = Client.Host.get_all_records_where rpc session_id where_clause in
  let all_recs = List.map (fun (host,host_r) -> let r = host_record rpc session_id host in r.setrefrec (host,host_r); r) hosts in
  let filter_params = List.filter (fun (p,_) ->
      let stem=List.hd (String.split ':' p) in not (List.mem stem (stdparams @ ignore_params))) params in
  (* Filter all the records *)
  List.fold_left filter_records_on_fields all_recs filter_params


let select_vm_geneva rpc session_id params =
  if List.mem_assoc "vm-name" params then
    begin
      let vmname = List.assoc "vm-name" params in
      let vms = Client.VM.get_all rpc session_id in
      let vm = List.filter (fun vm -> Client.VM.get_name_label rpc session_id vm = vmname) vms in
      if List.length vm = 0 then
        failwith ("VM with name '"^vmname^"' not found")
      else if List.length vm > 1 then
        failwith ("Multiple VMs with name '"^vmname^"' found")
      else
        vm_record rpc session_id (List.hd vm)
    end
  else if List.mem_assoc "vm-id" params then
    begin
      let vmid = List.assoc "vm-id" params in
      try
        vm_record rpc session_id (Client.VM.get_by_uuid rpc session_id vmid)
      with
        e -> failwith ("Failed to find VM with id '"^vmid^"'")
    end
  else
    (failwith ("Must select a VM using either vm-name or vm-id: params="
               ^(String.concat "," (List.map (fun (a,b) -> a^"="^b) params))))

let select_srs rpc session_id params ignore_params =
  let sr_name_or_ref = try Some (
      (* Escape every quote character *)
      List.assoc "sr" params |> Stdext.Xstringext.String.replace "\"" "\\\""
    ) with _ -> None in
  let params, where_clause = match sr_name_or_ref with
    | None -> params, "true"
    | Some v -> (
        (* try matching sr=<name or uuid> *)
        List.remove_assoc "sr" params,
        Printf.sprintf "(field \"uuid\"=\"%s\") or (field \"name__label\"=\"%s\")" v v
      )
  in
  let srs = Client.SR.get_all_records_where rpc session_id where_clause in
  let all_recs = List.map (fun (sr,sr_r) -> let r = sr_record rpc session_id sr in r.setrefrec (sr,sr_r); r) srs in
  let filter_params = List.filter (fun (p,_) ->
      let stem=List.hd (String.split ':' p) in not (List.mem stem (stdparams @ ignore_params))) params in
  (* Filter all the records *)
  List.fold_left filter_records_on_fields all_recs filter_params


exception Multiple_failure of (string * string) list

let format_message msg =
  Printf.sprintf "Message: time=%s priority=%Ld name='%s'" (Date.to_string msg.API.message_timestamp)
    (msg.API.message_priority) (msg.API.message_name)

let wrap_op printer pri rpc session_id op e =
  let now = (Unix.gettimeofday ()) in
  let result = op e in
  let msgs = try Client.Message.get ~rpc ~session_id ~cls:`VM ~obj_uuid:(safe_get_field (field_lookup e.fields "uuid")) ~since:(Date.of_float now) with _ -> [] in
  List.iter (fun (ref,msg) ->
      if msg.API.message_priority < pri
      then printer (Cli_printer.PStderr (format_message msg ^ "\n"))) msgs;
  result

let do_multiple op set =
  let fails = ref [] in
  let append_fail e msg =
    let uuid = safe_get_field (field_lookup e.fields "uuid") in
    fails := (uuid, msg) :: !fails
  in
  (* do every operations and record every failure *)
  let ret = List.map (fun e ->
      try
        Some (op e);
      with
      | Api_errors.Server_error(code, params) as exn -> (
          match Cli_util.get_server_error code params with
          | None           -> append_fail e (ExnHelper.string_of_exn exn)
          | Some (msg, ps) -> append_fail e (msg ^ "\n" ^ (String.concat "\n" ps))
        ); None
      | exn -> append_fail e (ExnHelper.string_of_exn exn); None
    ) set in

  let success = List.fold_left (fun acc e -> match e with None -> acc | Some x -> x :: acc) [] ret in
  if !fails <> [] then raise (Multiple_failure (!fails));
  success

let do_vm_op ?(include_control_vms = false) ?(include_template_vms = false)
    printer rpc session_id op params ?(multiple=true) ignore_params =
  let msg_prio = try Int64.of_string (List.assoc "message-priority" params) with _ -> 5L in
  let op = wrap_op printer msg_prio rpc session_id op in
  try
    let vms = select_vms ~include_control_vms ~include_template_vms rpc session_id params ignore_params in
    match List.length vms with
    | 0 -> failwith "No matching VMs found"
    | 1 -> [ op (List.hd vms) ]
    | _ ->
      if multiple && get_bool_param params "multiple" then
        do_multiple op vms
      else
        failwith
          (if not multiple
           then "Multiple matches VMs found. Operation can only be performed on one VM at a time"
           else "Multiple matches VMs found. --multiple required to complete the operation")
  with
  | Records.CLI_failed_to_find_param name ->
    failwith ("Parameter '"^name^"' is not a field of the VM class. Failed to select VM for operation.")

let do_host_op rpc session_id op params ?(multiple=true) ignore_params =
  let hosts = select_hosts rpc session_id params ignore_params in
  match List.length hosts with
  | 0 -> failwith "No matching hosts found"
  | 1 -> [ op 1 (List.hd hosts) ]
  | _ ->
    if multiple && get_bool_param params "multiple" then
      do_multiple (op (List.length hosts)) hosts
    else
      failwith
        (if not multiple
         then "Multiple matching hosts found. Operation can only be performed on one host at a time"
         else "Multiple matching hosts found. --multiple required to complete the operation")

let do_sr_op rpc session_id op params ?(multiple=true) ignore_params =
  let srs = select_srs rpc session_id params ignore_params in
  match List.length srs with
  | 0 -> failwith "No matching hosts found"
  | 1 -> [ op (List.hd srs) ]
  | _ ->
    if multiple && get_bool_param params "multiple" then
      do_multiple op srs
    else
      failwith
        (if not multiple
         then "Multiple matching SRs found. Operation can only be performed on one SR at a time"
         else "Multiple matching SRs found. --multiple required to complete the operation")

(* Execute f; if we get a no_hosts_available error then print a vm diagnostic table and reraise exception *)
let hook_no_hosts_available printer rpc session_id vm f =
  try
    f ()
  with
    (Api_errors.Server_error(code,params) as e) ->
    if code=Api_errors.no_hosts_available then
      begin
        printer (Cli_printer.PList ["There are no suitable hosts to start this VM on.";
                                    "The following table provides per-host reasons for why the VM could not be started:";""]);
        print_vm_host_report printer rpc session_id vm;
      end;
    raise e

let vm_compute_memory_overhead printer rpc session_id params =
  ignore
    (do_vm_op ~include_control_vms:true printer rpc session_id
       (fun vm ->
          let memory_overhead = Client.VM.compute_memory_overhead
              rpc session_id (vm.getref ()) in
          printer (Cli_printer.PMsg (Int64.to_string memory_overhead))
       )
       params []
    )

let vm_memory_dynamic_range_set printer rpc session_id params =
  let min = Record_util.bytes_of_string "min" (List.assoc "min" params)
  and max = Record_util.bytes_of_string "max" (List.assoc "max" params) in
  ignore
    (do_vm_op ~include_control_vms:true ~include_template_vms:true
       printer rpc session_id
       (fun vm ->
          Client.VM.set_memory_dynamic_range rpc session_id
            (vm.getref ()) min max)
       params ["min"; "max"])

let vm_memory_static_range_set printer rpc session_id params =
  let min = Record_util.bytes_of_string "min" (List.assoc "min" params)
  and max = Record_util.bytes_of_string "max" (List.assoc "max" params) in
  ignore
    (do_vm_op ~include_control_vms:true ~include_template_vms:true
       printer rpc session_id
       (fun vm ->
          Client.VM.set_memory_static_range rpc session_id
            (vm.getref ()) min max)
       params ["min"; "max"])

let vm_memory_limits_set printer rpc session_id params =
  let extract key =
    Record_util.bytes_of_string key (List.assoc key params) in
  let static_min = extract "static-min"
  and static_max = extract "static-max"
  and dynamic_min = extract "dynamic-min"
  and dynamic_max = extract "dynamic-max" in
  ignore
    (do_vm_op ~include_control_vms:true ~include_template_vms:true
       printer rpc session_id
       (fun vm ->
          Client.VM.set_memory_limits rpc session_id (vm.getref ())
            static_min static_max dynamic_min dynamic_max)
       params ["static-min"; "static-max"; "dynamic-min"; "dynamic-max"])

let vm_memory_set printer rpc session_id params =
  let value = Record_util.bytes_of_string "memory" (List.assoc "memory" params) in
  ignore
    (do_vm_op ~include_control_vms:true ~include_template_vms:true
       printer rpc session_id
       (fun vm ->
          Client.VM.set_memory rpc session_id (vm.getref ()) value)
       params ["memory"])

let vm_memory_target_set printer rpc session_id params =
  let target = Record_util.bytes_of_string "target"
      (List.assoc "target" params) in
  ignore (do_vm_op ~include_control_vms:true printer rpc session_id
            (fun vm ->
               Client.VM.set_memory_dynamic_range rpc session_id
                 (vm.getref ()) target target) params ["target"]
         )

let vm_memory_target_wait printer rpc session_id params =
  ignore (do_vm_op ~include_control_vms:true printer rpc session_id
            (fun vm ->
               let vm=vm.getref () in
               Client.VM.wait_memory_target_live rpc session_id vm) params [])

let vm_call_plugin fd printer rpc session_id params =
  let vm_uuid = List.assoc "vm-uuid" params in
  let vm = Client.VM.get_by_uuid rpc session_id vm_uuid in
  let plugin = List.assoc "plugin" params in
  let fn = List.assoc "fn" params in
  let args = read_map_params "args" params in
  (* Syntax interpretation: args:key:file=filename equals args:key=filename_content *)
  let convert ((k,v) as p) =
    match String.split ~limit:2 ':' k with
    | key :: "file" :: [] ->
      begin
        match get_client_file fd v with
        | Some s -> (key, s)
        | None ->
          marshal fd (Command (PrintStderr (Printf.sprintf "Failed to read file %s\n" v)));
          raise (ExitWithError 1)
      end
    | _ -> p in
  let args = List.map convert args in
  let result = Client.VM.call_plugin rpc session_id vm plugin fn args in
  printer (Cli_printer.PList [ result ])

let data_source_to_kvs ds =
  ["name_label",ds.API.data_source_name_label;
   "name_description",ds.API.data_source_name_description;
   "enabled",string_of_bool ds.API.data_source_enabled;
   "standard",string_of_bool ds.API.data_source_standard;
   "min",string_of_float ds.API.data_source_min;
   "max",string_of_float ds.API.data_source_max;
   "units",ds.API.data_source_units;
  ]

let vm_data_source_list printer rpc session_id params =
  ignore(do_vm_op ~include_control_vms:true printer rpc session_id ~multiple:false
           (fun vm ->
              let vm=vm.getref () in
              let dss =Client.VM.get_data_sources rpc session_id vm in
              let output = List.map data_source_to_kvs dss in
              printer (Cli_printer.PTable output)) params [])

let vm_data_source_record printer rpc session_id params =
  ignore(do_vm_op ~include_control_vms:true printer rpc session_id ~multiple:false
           (fun vm ->
              let vm=vm.getref () in
              let ds=List.assoc "data-source" params in
              Client.VM.record_data_source rpc session_id vm ds) params ["data-source"])

let vm_data_source_query printer rpc session_id params =
  ignore(do_vm_op ~include_control_vms:true printer rpc session_id ~multiple:false
           (fun vm ->
              let vm=vm.getref () in
              let ds=List.assoc "data-source" params in
              let value = Client.VM.query_data_source rpc session_id vm ds in
              printer (Cli_printer.PList [Printf.sprintf "%f" value])) params ["data-source"])

let vm_data_source_forget printer rpc session_id params =
  ignore(do_vm_op ~include_control_vms:true printer rpc session_id ~multiple:false
           (fun vm ->
              let vm=vm.getref () in
              let ds=List.assoc "data-source" params in
              Client.VM.forget_data_source_archives rpc session_id vm ds) params ["data-source"])

(* APIs to collect SR level RRDs *)
let sr_data_source_list printer rpc session_id params =
  ignore(do_sr_op rpc session_id ~multiple:false
           (fun sr ->
              let sr=sr.getref () in
              let dss = Client.SR.get_data_sources rpc session_id sr in
              let output = List.map data_source_to_kvs dss in
              printer (Cli_printer.PTable output)) params [])

let sr_data_source_record printer rpc session_id params =
  ignore(do_sr_op rpc session_id ~multiple:false
           (fun sr ->
              let sr=sr.getref () in
              let ds=List.assoc "data-source" params in
              Client.SR.record_data_source rpc session_id sr ds) params ["data-source"])

let sr_data_source_query printer rpc session_id params =
  ignore(do_sr_op rpc session_id ~multiple:false
           (fun sr ->
              let sr=sr.getref () in
              let ds=List.assoc "data-source" params in
              let value = Client.SR.query_data_source rpc session_id sr ds in
              printer (Cli_printer.PList [Printf.sprintf "%f" value])) params ["data-source"])

let sr_data_source_forget printer rpc session_id params =
  ignore(do_sr_op rpc session_id ~multiple:false
           (fun sr ->
              let sr=sr.getref () in
              let ds=List.assoc "data-source" params in
              Client.SR.forget_data_source_archives rpc session_id sr ds) params ["data-source"])

let host_data_source_list printer rpc session_id params =
  ignore(do_host_op rpc session_id ~multiple:false
           (fun _ host ->
              let host=host.getref () in
              let dss =Client.Host.get_data_sources rpc session_id host in
              let output = List.map data_source_to_kvs dss in
              printer (Cli_printer.PTable output)) params [])

let host_data_source_record printer rpc session_id params =
  ignore(do_host_op rpc session_id ~multiple:false
           (fun _ host ->
              let host=host.getref () in
              let ds=List.assoc "data-source" params in
              Client.Host.record_data_source rpc session_id host ds) params ["data-source"])

let host_data_source_query printer rpc session_id params =
  ignore(do_host_op rpc session_id ~multiple:false
           (fun _ host ->
              let host=host.getref () in
              let ds=List.assoc "data-source" params in
              let value = Client.Host.query_data_source rpc session_id host ds in
              printer (Cli_printer.PList [Printf.sprintf "%f" value])) params ["data-source"])

let host_data_source_forget printer rpc session_id params =
  ignore(do_host_op rpc session_id ~multiple:false
           (fun _ host ->
              let host=host.getref () in
              let ds=List.assoc "data-source" params in
              Client.Host.forget_data_source_archives rpc session_id host ds) params ["data-source"])


let host_compute_free_memory printer rpc session_id params =
  ignore (do_host_op rpc session_id ~multiple:false (
      fun _ host ->
        let host = host.getref () in
        let free_memory = Client.Host.compute_free_memory rpc session_id host in
        printer (Cli_printer.PMsg (Int64.to_string free_memory))
    ) params [])

let host_compute_memory_overhead printer rpc session_id params =
  ignore
    (do_host_op rpc session_id ~multiple:false
       (fun _ host ->
          let host = host.getref () in
          let memory_overhead = Client.Host.compute_memory_overhead
              rpc session_id host in
          printer (Cli_printer.PMsg (Int64.to_string memory_overhead))
       )
       params []
    )

let host_get_server_certificate printer rpc session_id params =
  ignore (do_host_op rpc session_id ~multiple:false
            (fun _ host ->
               let host = host.getref () in
               printer
                 (Cli_printer.PMsg
                    (Client.Host.get_server_certificate rpc session_id host)))
            params [])

let host_get_sm_diagnostics printer rpc session_id params =
  ignore (do_host_op rpc session_id ~multiple:false
            (fun _ host ->
               let host = host.getref () in
               printer
                 (Cli_printer.PMsg
                    (Client.Host.get_sm_diagnostics rpc session_id host)))
            params [])

let host_get_thread_diagnostics printer rpc session_id params =
  ignore (do_host_op rpc session_id ~multiple:false
            (fun _ host ->
               let host = host.getref () in
               printer
                 (Cli_printer.PMsg
                    (Client.Host.get_thread_diagnostics rpc session_id host)))
            params [])

let host_sm_dp_destroy printer rpc session_id params =
  let dp = List.assoc "dp" params in
  let allow_leak = get_bool_param params "allow-leak" in
  ignore (do_host_op rpc session_id ~multiple:false
            (fun _ host ->
               let host = host.getref () in
               Client.Host.sm_dp_destroy rpc session_id host dp allow_leak)
            params ["dp"; "allow-leak"])

let vm_memory_shadow_multiplier_set printer rpc session_id params =
  let multiplier = (try float_of_string (List.assoc "multiplier" params) with _ -> failwith "Failed to parse parameter 'multiplier': expecting a float") in
  let (_: unit list) = do_vm_op printer rpc session_id
      (fun vm ->
         let vm = vm.getref () in
         Client.VM.set_shadow_multiplier_live rpc session_id vm multiplier) params ["multiplier"] in
  ()

let vm_query_services printer rpc session_id params =
  ignore(do_vm_op printer rpc session_id
           (fun vm ->
              let vm=vm.getref () in
              let record = Client.VM.query_services rpc session_id vm in
              printer (Cli_printer.PTable [ ("Type", "Name") :: record ])
           ) params [])

let vm_start printer rpc session_id params =
  let force = get_bool_param params "force" in
  let paused = get_bool_param params "paused" in
  ignore(do_vm_op ~include_control_vms:true printer rpc session_id
           (fun vm ->
              let vm=vm.getref () in
              let task =
                if List.mem_assoc "on" params
                then
                  let host = get_host_by_name_or_id rpc session_id (List.assoc "on" params) in
                  Client.Async.VM.start_on rpc session_id vm (host.getref()) paused force
                else
                  Client.Async.VM.start rpc session_id vm paused force in
              hook_no_hosts_available printer rpc session_id vm
                (fun () ->
                   waiter printer rpc session_id params task
                )
           ) params ["on"; "paused"; "progress"])

let vm_suspend printer rpc session_id params =
  ignore(do_vm_op printer rpc session_id (fun vm ->
      let task = Client.Async.VM.suspend rpc session_id (vm.getref ()) in
      waiter printer rpc session_id params task
    ) params ["progress"])

let vm_resume printer rpc session_id params =
  let force = get_bool_param params "force" in
  ignore(do_vm_op printer rpc session_id
           (fun vm ->
              if List.mem_assoc "on" params then
                let host = get_host_by_name_or_id rpc session_id (List.assoc "on" params) in
                let task = Client.Async.VM.resume_on rpc session_id (vm.getref()) (host.getref()) false force in
                waiter printer rpc session_id params task
              else
                let vm=vm.getref() in
                hook_no_hosts_available printer rpc session_id vm
                  (fun ()->
                     let task = Client.Async.VM.resume rpc session_id vm false force in
                     waiter printer rpc session_id params task
                  )
           ) params ["on"; "progress"])

let vm_pause printer rpc session_id params =
  ignore(do_vm_op printer rpc session_id (fun vm -> Client.VM.pause rpc session_id (vm.getref ())) params [])

let vm_unpause printer rpc session_id params =
  ignore(do_vm_op printer rpc session_id (fun vm -> Client.VM.unpause rpc session_id (vm.getref ())) params [])

(* A helper function for VM install *)
let is_recommended recommendations_xml fieldname =
  let rec seek_recommendation i =
    if Xmlm.eoi i then false
    else match Xmlm.input i with
      | `El_start ((ns, tag), attrs)
        when tag = "restriction" && (List.mem ((ns, "field"), fieldname) attrs)
        -> List.mem ((ns, "value"), "true") attrs
      | _ -> seek_recommendation i
  in
  let i = Xmlm.make_input (`String (0, recommendations_xml)) in
  try
    seek_recommendation i
  with Xmlm.Error ((line, col), err) ->
    debug "Invalid VM.recommendations xml at line %d, column %d: %s" line col (Xmlm.error_message err);
    false

let vm_install_real printer rpc session_id template name description params =

  let sr_ref =
    if Client.VM.get_is_a_snapshot rpc session_id template then
      if false
      || (List.mem_assoc "sr-name-label" params
          || List.mem_assoc "sr-uuid" params) then
        failwith "Do not use the sr-name-label or sr-uuid argument when installing from a snapshot. By default, it will install each new disk on the same SR as the corresponding snapshot disks."
      else Some Ref.null
    else None in

  let sr_ref = match sr_ref with
    | Some _ -> sr_ref
    | None ->
      if List.mem_assoc "sr-uuid" params then
        let uuid = List.assoc "sr-uuid" params in
        Some (Client.SR.get_by_uuid rpc session_id uuid)
      else None in

  let sr_ref =
    if List.mem_assoc "sr-name-label" params then
      let name = List.assoc "sr-name-label" params in
      match Client.SR.get_by_name_label rpc session_id name with
      | [] -> failwith "No SR with that name-label found"
      | sr_list -> match sr_ref with
        | Some sr ->
          if List.mem sr sr_list then sr_ref
          else failwith "SR specified via sr-uuid doesn't have the name specified via sr-name-label"
        | None ->
          if List.length sr_list > 1 then
            failwith "Multiple SRs with that name-label found"
          else Some (List.hd sr_list)
    else sr_ref in

  let suspend_sr_ref = match sr_ref with
    | Some sr ->
      if Cli_util.is_valid_ref session_id sr then
        (* sr-uuid and/or sr-name-label was specified - use this as the suspend_SR *)
        sr
      else
        (* Template is a snapshot - copy the suspend_SR from the template *)
        Client.VM.get_suspend_SR rpc session_id template
    | None ->
      (* Not a snapshot and no sr-uuid or sr-name-label specified - copy the suspend_SR from the template *)
      Client.VM.get_suspend_SR rpc session_id template in

  (* It's fine that we still don't have a SR information till this step, we'll do
     	   a VM.clone instead of VM.copy. However we need to figure out sr_uuid for
     	   provisioning disks if any. *)
  let sr_uuid = match sr_ref with
    | Some r when r <> Ref.null -> Client.SR.get_uuid rpc session_id r
    | _ ->
      match get_default_sr_uuid rpc session_id with
      | Some uuid -> uuid
      | None ->
        match Xapi_templates.get_template_record rpc session_id template
        with
        | None | Some {Xapi_templates.disks = []} -> Ref.string_of Ref.null
        | _ -> failwith "Failed to find a valid default SR for the \
                         Pool. Please provide an sr-name-label or sr-uuid parameter." in

  let new_vm =
    match sr_ref with
    | Some r when r <> Ref.null ->
      Client.VM.copy rpc session_id template name r
    | _ ->
      Client.VM.clone rpc session_id template name in

  try
    Client.VM.set_name_description rpc session_id new_vm description;
    Client.VM.set_suspend_SR rpc session_id new_vm suspend_sr_ref;
    rewrite_provisioning_xml rpc session_id new_vm sr_uuid;
    let recommendations = Client.VM.get_recommendations rpc session_id template in
    let licerr = Api_errors.Server_error(Api_errors.license_restriction, [Features.name_of_feature Features.PCI_device_for_auto_update]) in
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    let policy_vendor_device_is_ok = not (Client.Pool.get_policy_no_vendor_device rpc session_id pool) in
    let want_dev = (is_recommended recommendations "has-vendor-device") && policy_vendor_device_is_ok in
    (
      try Client.VM.set_has_vendor_device rpc session_id new_vm want_dev
      with e when e = licerr ->
        let msg = Printf.sprintf "Note: the VM template recommends setting has-vendor-device=true (to provide the option of obtaining PV drivers through Windows Update), but a suitable licence has not been deployed for this host. Ignoring this recommendation and continuing with installation of VM %S..."
            (Client.VM.get_name_label rpc session_id new_vm) in
        warn "%s" msg;
        Cli_printer.PStderr (msg^"\n") |> printer
    );
    Client.VM.provision rpc session_id new_vm;
    (* Client.VM.start rpc session_id new_vm false true; *)  (* stop install starting VMs *)

    (* copy BIOS strings if needed *)
    if List.mem_assoc "copy-bios-strings-from" params then begin
      let host = Client.Host.get_by_uuid rpc session_id (List.assoc "copy-bios-strings-from" params) in
      Client.VM.copy_bios_strings rpc session_id new_vm host
    end;
    let vm_uuid = Client.VM.get_uuid rpc session_id new_vm in
    printer (Cli_printer.PList [vm_uuid])
  with e ->
    (try Client.VM.destroy rpc session_id new_vm with _ -> ());
    raise e


(* The process of finding the VM in this case is special-cased since we want to call the
 * params 'template-name', like a foreign key, sort of *)
let vm_install printer rpc session_id params =
  (* Filter on everything on the cmd line except params=... *)
  let template =
    if List.mem_assoc "template-uuid" params
    then
      try
        Client.VM.get_by_uuid rpc session_id (List.assoc "template-uuid" params)
      with _ -> failwith "Cannot find template"
    else
      begin
        let filter_params = [("is-a-template", "true"); ("is-control-domain", "false")] in
        let vms = Client.VM.get_all_records_where rpc session_id "true" in
        let all_recs = List.map (fun (vm,vm_r) -> let r = vm_record rpc session_id vm in r.setrefrec (vm,vm_r); r) vms in
        let find_by_name name =
          let templates = List.fold_left filter_records_on_fields all_recs (("name-label",name)::filter_params) in
          match List.length templates with
            0 -> failwith "No templates matched"
          | 1 -> (List.hd templates).getref ()
          | _ -> failwith "More than one matching template found"
        in

        if (List.mem_assoc "template-name-label" params) || (List.mem_assoc "template-name" params)
        then
          let template_name =
            if List.mem_assoc "template-name-label" params
            then (List.assoc "template-name-label" params)
            else (List.assoc "template-name" params) in
          find_by_name template_name
        else if List.mem_assoc "template" params
        then
          try
            Client.VM.get_by_uuid rpc session_id (List.assoc "template" params)
          with _ ->
            find_by_name (List.assoc "template" params)
        else
          failwith "Template must be specified by parameter 'template-uuid', 'template-name', 'template-name-label' or 'template'"
      end
  in

  if not (Client.VM.get_is_a_template rpc session_id template) then failwith "Can only install from templates";
  let new_name = List.assoc "new-name-label" params in
  let new_description = "Installed via xe CLI" in (* Client.VM.get_name_description rpc session_id template in *)
  vm_install_real printer rpc session_id template new_name new_description params

let console fd printer rpc session_id params =
  let c = match select_vms ~include_control_vms:true rpc session_id params [] with
    | [ vm_r ] ->
      let vm = vm_r.getref () in
      let cs = Client.VM.get_consoles rpc session_id vm in
      begin
        try
          List.find (fun c -> Client.Console.get_protocol rpc session_id c = `vt100) cs
        with Not_found ->
          marshal fd (Command (PrintStderr "No text console available\n"));
          raise (ExitWithError 1)
      end
    | [] ->
      marshal fd (Command (PrintStderr "No VM found\n"));
      raise (ExitWithError 1)
    | _ :: _ ->
      marshal fd (Command (PrintStderr "Multiple VMs found: please narrow your request to one VM.\n"));
      raise (ExitWithError 1)
  in
  let vm = Client.Console.get_VM rpc session_id c in
  let vm_name_label = Client.VM.get_name_label rpc session_id vm in
  marshal fd (Command (Print (Printf.sprintf "Connecting to console on VM %s. Press Ctrl + ']' to quit." vm_name_label)));
  let l = Client.Console.get_location rpc session_id c in
  let uri = Printf.sprintf "%s&session_id=%s" l (Ref.string_of session_id) in
  marshal fd (Command (HttpConnect uri));
  let response = ref (Response Wait) in
  while !response = Response Wait do response := unmarshal fd done;
  match !response with
  | Response OK -> ()
  | _ ->
    failwith "Failure"


let vm_uninstall_common fd printer rpc session_id params vms =
  let toremove = ref [] in
  let toprint = ref [] in
  (* Destroy the disks too *)
  let choose_objects_to_delete vm =
    let vbds=Client.VM.get_VBDs rpc session_id vm in

    let string_of_vdi vdi =
      (* add extra text if the VDI is being shared *)
      let r = Client.VDI.get_record rpc session_id vdi in
      Printf.sprintf "VDI: %s (%s) %s" r.API.vDI_uuid r.API.vDI_name_label
        (if List.length r.API.vDI_VBDs <= 1 then "" else " ** WARNING: disk is shared by other VMs") in
    let string_of_vm vm =
      let r = Client.VM.get_record rpc session_id vm in
      Printf.sprintf "VM : %s (%s)" r.API.vM_uuid r.API.vM_name_label in

    (* NB If a VDI is deleted then the VBD may be GCed at any time. *)
    let vdis = List.concat (List.map
                              (fun vbd ->
                                 try
                                   (* We only destroy VDIs where VBD.other_config contains 'owner' *)
                                   let other_config = Client.VBD.get_other_config rpc session_id vbd in
                                   let vdi = Client.VBD.get_VDI rpc session_id vbd in
                                   (* Double-check the VDI actually exists *)
                                   ignore(Client.VDI.get_uuid rpc session_id vdi);
                                   if List.mem_assoc Xapi_globs.owner_key other_config
                                   then [ vdi ] else [ ]
                                 with _ -> []) vbds) in
    let suspend_VDI =
      try
        let vdi = Client.VM.get_suspend_VDI rpc session_id vm in
        ignore (Client.VDI.get_uuid rpc session_id vdi);
        vdi
      with _ -> Ref.null in
    let output = string_of_vm vm :: (List.map string_of_vdi vdis) @ (if suspend_VDI = Ref.null then [] else [string_of_vdi suspend_VDI]) in
    toprint := !toprint @ output;
    let destroy () =
      if Client.VM.get_power_state rpc session_id vm <> `Halted then Client.VM.hard_shutdown rpc session_id vm;
      Client.VM.destroy rpc session_id vm;
      List.iter (fun vdi -> Client.VDI.destroy rpc session_id vdi) vdis;
      if suspend_VDI <> Ref.null then try Client.VDI.destroy rpc session_id suspend_VDI with _ -> ()
    in
    toremove := !toremove @ [destroy];
  in
  List.iter choose_objects_to_delete vms;
  marshal fd (Command (Print "The following items are about to be destroyed"));
  List.iter (fun s -> marshal fd (Command (Print s))) !toprint;
  if get_bool_param params "force" then
    (List.iter (fun f -> f ()) !toremove; marshal fd (Command (Print "All objects destroyed")))
  else
    begin
      if user_says_yes fd
      then (List.iter (fun f -> f ()) !toremove; marshal fd (Command (Print "All objects destroyed")))
    end

let vm_uninstall fd printer rpc session_id params =
  let vms = do_vm_op printer rpc session_id (fun vm -> vm.getref()) params [] in
  let snapshots = List.flatten (List.map (fun vm -> Client.VM.get_snapshots rpc session_id vm) vms) in
  vm_uninstall_common fd printer rpc session_id params (vms @ snapshots)

let get_templateVM_by_uuid rpc session_id template_uuid =
  let template_ref = Client.VM.get_by_uuid rpc session_id template_uuid in
  if not (Client.VM.get_is_a_template rpc session_id template_ref) then
    failwith (Printf.sprintf "This operation can only be performed on a VM template. %s is not a VM template." template_uuid);
  template_ref

let template_uninstall fd printer rpc session_id params =
  let uuid = List.assoc "template-uuid" params in
  let vm = get_templateVM_by_uuid rpc session_id uuid in
  vm_uninstall_common fd printer rpc session_id params [ vm ]

let vm_clone_aux clone_op cloned_string printer include_template_vms rpc session_id params =
  let new_name = List.assoc "new-name-label" params in
  let desc = try Some (List.assoc "new-name-description" params) with _ -> None in
  let new_vms = do_vm_op printer ~include_template_vms rpc session_id
      (fun vm -> clone_op ~rpc ~session_id ~vm: (vm.getref()) ~new_name) params ["new-name-label"; "new-name-description"] in
  ignore (may (fun desc -> Client.VM.set_name_description rpc session_id (List.hd new_vms) desc) desc);
  printer (Cli_printer.PList (List.map (fun vm -> Client.VM.get_uuid rpc session_id vm) new_vms))

let vm_clone printer = vm_clone_aux Client.VM.clone "Cloned " printer true
let vm_snapshot printer = vm_clone_aux Client.VM.snapshot "Snapshotted " printer false
let vm_snapshot_with_quiesce printer = vm_clone_aux Client.VM.snapshot_with_quiesce "Snapshotted" printer false
let vm_checkpoint printer = vm_clone_aux Client.VM.checkpoint "Checkpointed " printer false

let get_snapshot_uuid params =
  if List.mem_assoc "snapshot-uuid" params
  then List.assoc "snapshot-uuid" params
  else if List.mem_assoc "uuid" params
  then List.assoc "uuid" params
  else raise (failwith "Required parameter not found: snapshot-uuid or uuid.")

let get_snapshotVM_by_uuid rpc session_id snap_uuid =
  let snap_ref = Client.VM.get_by_uuid rpc session_id snap_uuid in
  if not (Client.VM.get_is_a_snapshot rpc session_id snap_ref) then
    failwith (Printf.sprintf "This operation can only be performed on a VM snapshot. %s is not a VM snapshot." snap_uuid);
  snap_ref

let snapshot_revert printer rpc session_id params =
  let snap_uuid = get_snapshot_uuid params in
  let snap_ref = get_snapshotVM_by_uuid rpc session_id snap_uuid in
  Client.VM.revert ~rpc ~session_id ~snapshot:snap_ref

let snapshot_op op printer rpc session_id params =
  let new_name = List.assoc "new-name-label" params in
  let desc = if List.mem_assoc "new-name-description" params then Some (List.assoc "new-name-description" params) else None in
  let uuid = get_snapshot_uuid params in
  let ref = get_snapshotVM_by_uuid rpc session_id uuid in
  let new_ref = op ~rpc ~session_id ~vm:ref ~new_name in
  ignore (may (fun desc -> Client.VM.set_name_description rpc session_id new_ref desc) desc);
  let new_uuid = Client.VM.get_uuid ~rpc ~session_id ~self:new_ref in
  printer (Cli_printer.PList [new_uuid])

let snapshot_clone printer = snapshot_op Client.VM.clone printer

let snapshot_copy printer rpc session_id params =
  let sr = if List.mem_assoc "sr-uuid" params then Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params) else Ref.null in
  let op = Client.VM.copy ~sr:sr in
  snapshot_op op printer rpc session_id params

let snapshot_destroy printer rpc session_id params =
  let snap_uuid = get_snapshot_uuid params in
  let snap_ref = get_snapshotVM_by_uuid rpc session_id snap_uuid in
  if Client.VM.get_power_state rpc session_id snap_ref <> `Halted then Client.VM.hard_shutdown ~rpc ~session_id ~vm:snap_ref;
  Client.VM.destroy ~rpc ~session_id ~self:snap_ref

let snapshot_uninstall fd printer rpc session_id params =
  let snap_uuid = get_snapshot_uuid params in
  let snap_ref = get_snapshotVM_by_uuid rpc session_id snap_uuid in
  vm_uninstall_common fd printer rpc session_id params [snap_ref]

let vm_copy printer rpc session_id params =
  let new_name = List.assoc "new-name-label" params in
  let desc = try Some (List.assoc "new-name-description" params) with _ -> None in
  let sr = if List.mem_assoc "sr-uuid" params then Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params) else Ref.null in
  let new_vms = do_vm_op printer ~multiple:false ~include_template_vms:true rpc session_id (fun vm -> Client.VM.copy rpc session_id (vm.getref()) new_name sr) params ["new-name-label"; "sr-uuid"; "new-name-description"] in
  ignore (may (fun desc -> Client.VM.set_name_description rpc session_id (List.hd new_vms) desc) desc);
  printer (Cli_printer.PList (List.map (fun vm -> Client.VM.get_uuid rpc session_id vm) new_vms))

let vm_reset_powerstate printer rpc session_id params =
  if not (List.mem_assoc "force" params) then
    failwith "This operation is extremely dangerous and may cause data loss. This operation must be forced (use --force).";
  ignore (do_vm_op printer rpc session_id (fun vm -> Client.VM.power_state_reset rpc session_id (vm.getref())) params [])

let snapshot_reset_powerstate printer rpc session_id params =
  if not (List.mem_assoc "force" params) then
    failwith "This operation is extremely dangerous and may cause data loss. This operation must be forced (use --force).";
  let snapshot_uuid = get_snapshot_uuid params in
  let snapshot = get_snapshotVM_by_uuid rpc session_id snapshot_uuid in
  Client.VM.power_state_reset rpc session_id snapshot

let vm_shutdown printer rpc session_id params =
  let force = get_bool_param params "force" in
  ignore(if force
         then do_vm_op printer rpc session_id (fun vm -> Client.Async.VM.hard_shutdown rpc session_id (vm.getref()) |> waiter printer rpc session_id params) params ["progress"]
         else do_vm_op printer rpc session_id (fun vm -> Client.Async.VM.clean_shutdown rpc session_id (vm.getref()) |> waiter printer rpc session_id params) params ["progress"])

let vm_reboot printer rpc session_id params =
  let force = get_bool_param params "force" in
  ignore(if force
         then do_vm_op printer rpc session_id (fun vm -> Client.VM.hard_reboot rpc session_id (vm.getref())) params []
         else do_vm_op printer rpc session_id (fun vm -> Client.VM.clean_reboot rpc session_id (vm.getref())) params [])

let vm_compute_maximum_memory printer rpc session_id params =
  let total = Record_util.bytes_of_string "total" (List.assoc "total" params) in
  let approximate = get_bool_param params "approximate" in
  ignore(do_vm_op printer rpc session_id
           (fun vm ->
              let max = Client.VM.maximise_memory rpc session_id (vm.getref()) total approximate in
              printer (Cli_printer.PList [Printf.sprintf "%Ld" max]))
           params [ "total"; "approximate" ])

let vm_retrieve_wlb_recommendations printer rpc session_id params =
  let table vm =
    List.map (fun (host,recom) -> ((Client.Host.get_name_label rpc session_id host) ^ "(" ^ (Client.Host.get_uuid rpc session_id host) ^ ")", String.concat " " recom))
      (Client.VM.retrieve_wlb_recommendations rpc session_id (vm.getref()))
  in
  try
    let vms = select_vms rpc session_id params [] in
    match List.length vms with
    | 0 -> failwith "No matching VMs found"
    | 1 -> printer (Cli_printer.PTable [("Host(Uuid)", "Stars, RecID, ZeroScoreReason") :: table (List.hd vms)])
    | _ -> failwith "Multiple VMs found. Operation can only be performed on one VM at a time"
  with
  | Records.CLI_failed_to_find_param name ->
    failwith ("Parameter '"^name^"' is not a field of the VM class. Failed to select VM for operation.")

let vm_migrate_sxm_params = ["remote-master"; "remote-username"; "vif"; "remote-password";
                             "remote-network"; "vdi"; "vgpu"]

let vm_migrate printer rpc session_id params =
  (* Hack to match host-uuid and host-name for backwards compatibility *)
  let params = List.map (fun (k, v) -> if (k = "host-uuid") || (k = "host-name") then ("host", v) else (k, v)) params in
  let options = List.map_assoc_with_key (string_of_bool +++ bool_of_string) (List.restrict_with_default "false" ["force"; "live"; "copy"] params) in
  (* We assume the user wants to do Storage XenMotion if they supply any of the
     SXM-specific parameters, and then we use the new codepath. *)
  let use_sxm_migration =
    (* This is a safe assumption, because vm_migrate_sxm_params do not clash with
       the VM selector keys. *)
    let is_sxm_param k = List.exists (fun p -> String.startswith p k) vm_migrate_sxm_params in
    List.exists (fun (k,_) -> is_sxm_param k) params
  in
  if use_sxm_migration then begin
    printer (Cli_printer.PMsg "Performing a Storage XenMotion migration. Your VM's VDIs will be migrated with the VM.");
    if not ((List.mem_assoc "remote-master" params) && (List.mem_assoc "remote-username" params)
            && (List.mem_assoc "remote-password" params)) then begin
      failwith "Storage XenMotion requires remote-master, remote-username, and \
                remote-password to be specified. Please see 'xe help vm-migrate' for \
                help."
    end;
    let ip = List.assoc "remote-master" params in
    let remote_rpc xml =
      let open Xmlrpc_client in
      let http = xmlrpc ~version:"1.0" "/" in
      XMLRPC_protocol.rpc ~srcstr:"cli" ~dststr:"dst_xapi" ~transport:(SSL(SSL.make ~use_fork_exec_helper:false (), ip, 443)) ~http xml in
    let username = List.assoc "remote-username" params in
    let password = List.assoc "remote-password" params in
    let remote_session = Client.Session.login_with_password remote_rpc username password "1.3" Xapi_globs.xapi_user_agent in
    finally
      (fun () ->
         let host, host_record =
           let all = Client.Host.get_all_records remote_rpc remote_session in
           if List.mem_assoc "host" params then begin
             let x = List.assoc "host" params in
             try
               List.find (fun (_, h) -> h.API.host_hostname = x || h.API.host_name_label = x || h.API.host_uuid = x) all
             with Not_found ->
               failwith (Printf.sprintf "Failed to find host: %s" x)
           end else begin
             List.hd all
           end in
         let network, network_record =
           let all = Client.Network.get_all_records remote_rpc remote_session in
           if List.mem_assoc "remote-network" params then begin
             let x = List.assoc "remote-network" params in
             try
               List.find (fun (_, net) -> net.API.network_bridge = x || net.API.network_name_label = x || net.API.network_uuid = x) all
             with Not_found ->
               failwith (Printf.sprintf "Failed to find network: %s" x)
           end else begin
             let pifs = host_record.API.host_PIFs in
             let management_pifs = List.filter (fun pif ->
                 Client.PIF.get_management remote_rpc remote_session pif) pifs in
             if List.length management_pifs = 0 then
               failwith (Printf.sprintf "Could not find management PIF on host %s" (host_record.API.host_uuid));
             let pif = List.hd management_pifs in
             let net = Client.PIF.get_network remote_rpc remote_session pif in
             (net, Client.Network.get_record remote_rpc remote_session net)
           end in
         let vif_map = List.map (fun (vif_uuid,net_uuid) ->
             let vif = Client.VIF.get_by_uuid rpc session_id vif_uuid in
             let net = Client.Network.get_by_uuid remote_rpc remote_session net_uuid in
             vif,net) (read_map_params "vif" params) in

         let vdi_map = List.map (fun (vdi_uuid,sr_uuid) ->
             let vdi = Client.VDI.get_by_uuid rpc session_id vdi_uuid in
             let sr = Client.SR.get_by_uuid remote_rpc remote_session sr_uuid in
             vdi,sr) (read_map_params "vdi" params) in

         let vgpu_map = List.map (fun (vgpu_uuid,gpu_group_uuid) ->
             let vgpu = Client.VGPU.get_by_uuid rpc session_id vgpu_uuid in
             let gpu_group = Client.GPU_group.get_by_uuid remote_rpc remote_session gpu_group_uuid in
             vgpu,gpu_group) (read_map_params "vgpu" params) in

         let preferred_sr =
           (* The preferred SR is determined to be as the SR that the destine host has a PDB attached to it,
              and among the choices of that the shared is preferred first(as it is recommended to have shared storage
              in pool to host VMs), and then the one with the maximum available space *)
           try
             let query = Printf.sprintf {|(field "host"="%s") and (field "currently_attached"="true")|} (Ref.string_of host) in
             let host_pbds = Client.PBD.get_all_records_where remote_rpc remote_session query in
             let srs = List.map (fun (pbd_ref, pbd_rec) ->
                 pbd_rec.API.pBD_SR, Client.SR.get_record remote_rpc remote_session pbd_rec.API.pBD_SR) host_pbds in
             (* In the following loop, the current SR:sr' will be compared with previous checked ones,
                first if it is an ISO type, then pass this one for selection, then the only shared one from this and
                previous one will be valued, and if not that case (both shared or none shared), choose the one with
                more space available *)
             let (sr, _) = List.fold_left (fun (sr, free_space) ((_, sr_rec') as sr') ->
                 if sr_rec'.API.sR_content_type = "iso" then (sr, free_space)
                 else
                   let free_space' = Int64.sub sr_rec'.API.sR_physical_size sr_rec'.API.sR_physical_utilisation in
                   match sr with
                   | None -> (Some sr', free_space')
                   | Some ((_, sr_rec) as sr) ->
                     match sr_rec.API.sR_shared, sr_rec'.API.sR_shared with
                     | true, false -> (Some sr, free_space)
                     | false, true -> (Some sr', free_space')
                     | _ -> if (free_space' > free_space) then (Some sr', free_space')
                       else (Some sr, free_space)
               ) (None, Int64.zero) srs in
             match sr with
             | Some (sr_ref, _) -> Some sr_ref
             | _ -> None
           with _ -> None in

         let vdi_map = match preferred_sr with
           | None -> vdi_map
           | Some preferred_sr ->
             let vms = select_vms ~include_template_vms:true rpc session_id params
                 ( "host" :: "host-uuid" :: "host-name" :: "live" :: "force" :: "copy"
                   :: vm_migrate_sxm_params ) in
             if vms = [] then failwith "No matching VMs found" ;
             let vbds = Client.VM.get_VBDs rpc session_id ((List.hd vms).getref ()) in
             let vbds = List.filter (fun vbd ->
                 not (Client.VBD.get_empty rpc session_id vbd)) vbds in
             let vdis = List.map
                 (fun vbd -> Client.VBD.get_VDI rpc session_id vbd) vbds in
             let overrides = List.map (fun vdi ->
                 if List.mem_assoc vdi vdi_map
                 then (vdi, List.assoc vdi vdi_map)
                 else (vdi, preferred_sr)
               ) vdis in
             let filtered_orig_list = List.filter (fun (vdi,_) ->
                 not (List.mem_assoc vdi overrides)) vdi_map in
             overrides @ filtered_orig_list
         in

         let params = List.filter (fun (s,_) -> if String.length s < 5 then true
                                    else let start = String.sub s 0 4 in start <> "vif:" && start <> "vdi:") params in
         printer (Cli_printer.PMsg (Printf.sprintf "Will migrate to remote host: %s, using remote network: %s. Here is the VDI mapping:" host_record.API.host_name_label network_record.API.network_name_label));
         List.iter (fun (vdi,sr) ->
             printer (Cli_printer.PMsg (Printf.sprintf "VDI %s -> SR %s"
                                          (Client.VDI.get_uuid rpc session_id vdi)
                                          (Client.SR.get_uuid remote_rpc remote_session sr))))
           vdi_map ;
         let token = Client.Host.migrate_receive remote_rpc remote_session host network options in
         let new_vm =
           do_vm_op ~include_control_vms:false ~include_template_vms:true printer rpc session_id (fun vm -> Client.VM.migrate_send rpc session_id (vm.getref ()) token true vdi_map vif_map options vgpu_map)
             params (["host"; "host-uuid"; "host-name"; "live"; "force"; "copy"] @ vm_migrate_sxm_params) |> List.hd in
         if get_bool_param params "copy" then
           printer (Cli_printer.PList [Client.VM.get_uuid remote_rpc remote_session new_vm])
      )
      (fun () -> Client.Session.logout remote_rpc remote_session)
  end else begin
    if not (List.mem_assoc "host" params) then failwith "No destination host specified";
    let host = (get_host_by_name_or_id rpc session_id (List.assoc "host" params)).getref () in

    ignore(do_vm_op ~include_control_vms:true printer rpc session_id (fun vm -> Client.VM.pool_migrate rpc session_id (vm.getref ()) host options)
             params ["host"; "host-uuid"; "host-name"; "live"])
  end

let vm_disk_list_aux vm is_cd_list printer rpc session_id params =
  let vbds = List.filter (fun vbd -> Client.VBD.get_type rpc session_id vbd = (if is_cd_list then `CD else `Disk)) (vm.record()).API.vM_VBDs in
  let vbdrecords = List.map (fun vbd-> (vbd_record rpc session_id vbd)) vbds in
  let vdirecords = List.map (fun vbd ->
      if not (Client.VBD.get_empty rpc session_id vbd) then Some (vdi_record rpc session_id (Client.VBD.get_VDI rpc session_id vbd)) else None) vbds in
  (* Hack - convert 'vbd-params' to 'params' *)
  let params' = List.map (fun (a,b) -> if a="vbd-params" then ("params",b) else (a,b)) params in
  let selectedvbd = select_fields params' vbdrecords
      (if is_cd_list
       then ["uuid"; "vm-name-label"; "userdevice"; "empty"]
       else ["uuid"; "vm-name-label"; "userdevice"])
  in
  let params' = List.map (fun (a,b) -> if a="vdi-params" then ("params",b) else (a,b)) params in
  let rec doit vbds vdis n =
    match (vbds,vdis) with
    | ([],[]) -> ()
    | (vbd::vbds,vdi::vdis) ->
      let disk = (if is_cd_list then "CD " else "Disk ")^string_of_int n in
      printer (Cli_printer.PMsg (disk ^ " VBD:"));
      printer (Cli_printer.PTable [(List.map print_field vbd)]);
      (* Only print out the VDI if there is one - empty cds don't have one *)
      begin
        match vdi with
          Some vdi ->
          let selectedvdi = List.hd (select_fields params' [vdi] ["uuid"; "name-label"; "virtual-size"; "sr-name-label"]) in
          printer (Cli_printer.PMsg (disk ^ " VDI:"));
          printer (Cli_printer.PTable [(List.map print_field selectedvdi)]);
        | None -> ()
      end;
      doit vbds vdis (n+1)
    | _ -> (failwith "Unexpected mismatch in list length in vm_disk_list")
  in doit selectedvbd vdirecords 0

let vm_disk_list is_cd_list printer rpc session_id params =
  let op vm = vm_disk_list_aux vm is_cd_list printer rpc session_id params in
  let ( _ : unit list) = do_vm_op printer rpc session_id op params ["vbd-params";"vdi-params"] in ()

let snapshot_disk_list is_cd_list printer rpc session_id params =
  let snapshot_uuid = get_snapshot_uuid params in
  let snapshot_ref = get_snapshotVM_by_uuid rpc session_id snapshot_uuid in
  let snapshot = vm_record rpc session_id snapshot_ref in
  vm_disk_list_aux snapshot is_cd_list printer rpc session_id params

let vm_crashdump_list printer rpc session_id params =
  let op vm =
    let records = List.map (fun crashdump -> (crashdump_record rpc session_id crashdump).fields) (vm.record()).API.vM_crash_dumps in
    printer (Cli_printer.PTable (List.map (List.map print_field) records))
  in
  ignore(do_vm_op printer rpc session_id op params [])

(* Disk add creates a VDI with the size, sr specified. The name and sector size
 * can be optionally specified. A VBD is then creased with the device name as specified *)
let vm_disk_add printer rpc session_id params =
  (* Required params *)
  let vdi_size = Record_util.bytes_of_string "disk-size" (List.assoc "disk-size" params) in
  let vbd_device = List.assoc "device" params in
  let sr =
    if List.mem_assoc "sr-uuid" params then
      let sr_uuid = List.assoc "sr-uuid" params in
      Client.SR.get_by_uuid rpc session_id sr_uuid
    else
      match get_default_sr_uuid rpc session_id with
      | Some x -> Client.SR.get_by_uuid rpc session_id x
      | None -> failwith "No default Pool SR set; you must specify an SR on the commandline"
  in
  (* Optional params *)
  let vdi_name = "Created by xe" in
  let op vm =
    let vm=vm.getref() in
    let vmuuid = Client.VM.get_uuid ~rpc ~session_id ~self:vm in
    let sm_config = [ Xapi_globs._sm_vm_hint, vmuuid ] in
    let vdi = Client.VDI.create ~rpc ~session_id ~name_label:vdi_name ~name_description:vdi_name ~sR:sr ~virtual_size:vdi_size ~_type:`user ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config ~tags:[] in
    try
      let _ =
        create_owner_vbd_and_plug rpc session_id vm vdi
          vbd_device false `RW `Disk true "" [] in
      ()
    with
      e ->
      Client.VDI.destroy rpc session_id vdi;
      raise e
  in
  ignore(do_vm_op printer rpc session_id op params ["sr-uuid";"device";"disk-size"])

let vm_disk_remove printer rpc session_id params =
  let device = List.assoc "device" params in
  let op vm =
    let vm=vm.getref() in
    let vm_record = Client.VM.get_record rpc session_id vm in
    let vbd_to_remove = List.filter (fun x -> device = Client.VBD.get_userdevice rpc session_id x) vm_record.API.vM_VBDs in
    if List.length vbd_to_remove < 1 then (failwith "Disk not found") else
      let vbd = List.nth vbd_to_remove 0 in
      let vdi = Client.VBD.get_VDI rpc session_id vbd in
      Client.VBD.destroy rpc session_id vbd;
      Client.VDI.destroy rpc session_id vdi
  in
  ignore(do_vm_op printer rpc session_id op params ["device"])

let vm_disk_detach printer rpc session_id params =
  let device = List.assoc "device" params in
  let op vm =
    let vm_record = vm.record () in
    let vbd_to_remove = List.filter (fun x -> device = Client.VBD.get_userdevice rpc session_id x) vm_record.API.vM_VBDs in
    if List.length vbd_to_remove < 1 then (failwith "Disk not found") else
      let vbd = List.nth vbd_to_remove 0 in
      Client.VBD.destroy rpc session_id vbd
  in
  ignore(do_vm_op printer rpc session_id op params ["device"])

let vm_disk_resize printer rpc session_id params =
  let device = List.assoc "device" params in
  let new_size = Record_util.bytes_of_string "disk-size" (List.assoc "disk-size" params) in
  let op vm =
    let vm_record = vm.record () in
    let vbd_to_resize = List.filter (fun x -> device = Client.VBD.get_userdevice rpc session_id x) vm_record.API.vM_VBDs in
    if List.length vbd_to_resize < 1 then (failwith "Disk not found") else
      let vbd = List.nth vbd_to_resize 0 in
      let vdi = Client.VBD.get_VDI rpc session_id vbd in
      Client.VDI.resize rpc session_id vdi new_size
  in
  ignore(do_vm_op printer rpc session_id op params ["device";"disk-size"])

let vm_cd_remove printer rpc session_id params =
  let disk_name = List.assoc "cd-name" params in
  let op vm =
    let vm_record = vm.record () in
    let vbd_to_remove = List.filter
        (fun x ->
           try
             let vdi = (Client.VBD.get_VDI rpc session_id x) in
             let sr = (Client.VDI.get_SR rpc session_id vdi) in
             ("iso"=Client.SR.get_content_type rpc session_id sr) &&
             (disk_name = Client.VDI.get_name_label rpc session_id vdi)
           with _ (* VDI handle invalid *) -> disk_name="<EMPTY>") vm_record.API.vM_VBDs in
    if List.length vbd_to_remove < 1 then raise (failwith "Disk not found") else
      let vbd = List.nth vbd_to_remove 0 in
      Client.VBD.destroy rpc session_id vbd
  in
  ignore(do_vm_op printer rpc session_id op params ["cd-name"])

let vm_cd_add printer rpc session_id params =
  let cd_name = List.assoc "cd-name" params in
  let vdis = Client.VDI.get_by_name_label rpc session_id cd_name in
  let vdis = List.filter (fun vdi -> let sr = Client.VDI.get_SR rpc session_id vdi in "iso"=Client.SR.get_content_type rpc session_id sr) vdis in
  (if List.length vdis = 0 then (failwith ("CD "^cd_name^" not found!")));
  let vdi = List.nth vdis 0 in
  let op vm = create_vbd_and_plug
      rpc session_id (vm.getref()) vdi (List.assoc "device" params) false `RO `CD true "" []
  in
  ignore(do_vm_op printer rpc session_id op params ["cd-name";"device";"cd-location"])
(* cd-location was a geneva-style param *)

let vm_cd_eject printer rpc session_id params =
  let op vm =
    let vm_record = vm.record () in
    let vbds = vm_record.API.vM_VBDs in
    let cdvbds = List.filter (fun vbd -> Client.VBD.get_type rpc session_id vbd = `CD) vbds in
    if List.length cdvbds = 0 then (failwith "No CDs found");
    if List.length cdvbds > 1 then (failwith "Two or more CDs found. Please use vbd-eject");
    let cd = List.hd cdvbds in
    Client.VBD.eject rpc session_id cd
  in
  ignore(do_vm_op printer rpc session_id op params [])

let vm_cd_insert printer rpc session_id params =
  let cd_name = List.assoc "cd-name" params in
  let vdis =
    Client.VDI.get_by_name_label rpc session_id cd_name in
  let vdis = List.filter (fun vdi -> let sr = Client.VDI.get_SR rpc session_id vdi in "iso"=Client.SR.get_content_type rpc session_id sr) vdis in
  (if List.length vdis = 0 then (failwith ("CD "^cd_name^" not found")));
  (if List.length vdis > 1 then (failwith ("Multiple CDs named "^cd_name^" found. Please use vbd-insert and specify uuids")));
  let op vm =
    let vm_record = vm.record () in
    let vbds = vm_record.API.vM_VBDs in
    let cdvbds = List.filter (fun vbd -> (Client.VBD.get_type rpc session_id vbd = `CD) && (Client.VBD.get_empty rpc session_id vbd)) vbds in
    if List.length cdvbds = 0 then raise (Api_errors.Server_error(Api_errors.vm_no_empty_cd_vbd, [ Ref.string_of (vm.getref ()) ]));
    if List.length cdvbds > 1 then (failwith "Two or more empty CD devices found. Please use vbd-insert");
    let cd = List.hd cdvbds in
    Client.VBD.insert rpc session_id cd (List.hd vdis)
  in
  ignore(do_vm_op printer rpc session_id op params ["cd-name"])

let host_careful_op op warnings fd printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id uuid in
  let pool = List.hd (Client.Pool.get_all rpc session_id) in
  let _ (* unused variable 'pool_master' *) = Client.Pool.get_master rpc session_id pool in
  (* if pool_master = host then failwith "Cannot forget pool master"; *)

  let force = get_bool_param params "force" in

  let go () = ignore (op ~rpc ~session_id ~self:host) in

  if force
  then go ()
  else begin
    (* Best-effort attempt to warn the user *)
    List.iter (fun x-> marshal fd (Command (Print x))) warnings;
    if user_says_yes fd
    then go ()
  end

let host_forget x =
  let warnings = [
    "WARNING: A host should only be forgotten if it is physically unrecoverable;";
    "WARNING: if possible, Hosts should be 'ejected' from the Pool instead.";
    "WARNING: Once a host has been forgotten it will have to be re-installed.";
    "WARNING: This operation is irreversible."] in
  host_careful_op Client.Host.destroy warnings x

let host_declare_dead x =
  let warnings = [
    "WARNING: A host should only be declared dead if it is verified offline.";
    "WARNING: Performing this operation if the host is still online and has any";
    "WARNING: running VMs may lead to possible data loss and/or corruption."
  ] in
  host_careful_op (fun ~rpc ~session_id ~self -> Client.Host.declare_dead ~rpc ~session_id ~host:self) warnings x

let host_license_add fd printer rpc session_id params =
  let host =
    if List.mem_assoc "host-uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params)
    else
      get_host_from_session rpc session_id in
  let license_file = List.assoc "license-file" params in
  match get_client_file fd license_file with
  | Some license ->
    debug "Checking license [%s]" license;
    (try
       Client.Host.license_add rpc session_id host (Base64.encode license);
       marshal fd (Command (Print "License applied."))
     with _ ->
       marshal fd (Command (PrintStderr "Failed to apply license file.\n"));
       raise (ExitWithError 1)
    )
  | None ->
    marshal fd (Command (PrintStderr "Failed to read license file.\n"));
    raise (ExitWithError 1)

let host_license_remove printer rpc session_id params =
  let host =
    if List.mem_assoc "host-uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params)
    else
      get_host_from_session rpc session_id in
  Client.Host.license_remove rpc session_id host

let host_license_view printer rpc session_id params =
  let host =
    if List.mem_assoc "host-uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params)
    else
      get_host_from_session rpc session_id in
  let params = Client.Host.get_license_params rpc session_id host in
  let tohide = [ "sku_type" ] in
  let params = List.filter (fun (x, _) -> not (List.mem x tohide)) params in
  printer (Cli_printer.PTable [params])

let with_license_server_changes printer rpc session_id params hosts f =
  (* Save the original license server details for each host;
     	 * in case of failure we will need to roll back. *)
  let current_license_servers =
    List.map
      (fun host -> (host, Client.Host.get_license_server rpc session_id host))
      hosts
  in
  (* Set any new license server address across the pool. *)
  if List.mem_assoc "license-server-address" params then begin
    let address = List.assoc "license-server-address" params in
    List.iter
      (fun host ->
         Client.Host.remove_from_license_server rpc session_id host "address";
         Client.Host.add_to_license_server rpc session_id host "address" address)
      hosts
  end;
  (* Set any new license server port across the pool. *)
  if List.mem_assoc "license-server-port" params then begin
    let port = List.assoc "license-server-port" params in
    let port_int = try int_of_string port with _ -> -1 in
    if port_int < 0 || port_int > 65535 then
      printer (Cli_printer.PStderr "NOTE: The given port number is invalid; reverting to the current value.\n")
    else begin
      List.iter
        (fun host ->
           Client.Host.remove_from_license_server rpc session_id host "port";
           Client.Host.add_to_license_server rpc session_id host "port" port)
        hosts
    end
  end;
  let now = (Unix.gettimeofday ()) in
  try
    f rpc session_id
  with
  | Api_errors.Server_error (name, args) as e
    when name = Api_errors.license_checkout_error ->
    (* Put back original license_server_details *)
    List.iter
      (fun (host, license_server) ->
         Client.Host.set_license_server rpc session_id host license_server)
      current_license_servers;
    let alerts = Client.Message.get_since rpc session_id (Date.of_float (now -. 1.)) in
    let print_if_checkout_error (ref, msg) =
      if false
      || msg.API.message_name = (fst Api_messages.v6_rejected)
      || msg.API.message_name = (fst Api_messages.v6_comm_error)
      || msg.API.message_name = (fst Api_messages.v6_license_server_version_obsolete)
      then begin
        Client.Message.destroy rpc session_id ref;
        printer (Cli_printer.PStderr (msg.API.message_body ^ "\n"))
      end
    in
    if alerts = []
    then raise e
    else begin
      List.iter print_if_checkout_error alerts;
      raise (ExitWithError 1)
    end
  | Api_errors.Server_error (name, args) as e
    when name = Api_errors.invalid_edition ->
    let editions = (V6_client.get_editions "host_apply_edition")
                   |> List.map V6_interface.(fun ed -> ed.title)
                   |> String.concat ", "
    in
    printer (Cli_printer.PStderr ("Valid editions are: " ^ editions ^ "\n"));
    raise e
  | e -> raise e

let host_apply_edition printer rpc session_id params =
  let host =
    if List.mem_assoc "host-uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params)
    else
      get_host_from_session rpc session_id in
  let edition = List.assoc "edition" params in
  with_license_server_changes printer rpc session_id params [host]
    (fun rpc session_id -> Client.Host.apply_edition rpc session_id host edition false)

let host_all_editions printer rpc session_id params =
  let editions = List.map V6_interface.(fun ed -> ed.title) (V6_client.get_editions "host_all_editions") in
  printer (Cli_printer.PList editions)

let host_evacuate printer rpc session_id params =
  ignore (do_host_op rpc session_id ~multiple:false
            (fun _ host ->
               Client.Host.evacuate rpc session_id (host.getref ()))
            params [])

let host_get_vms_which_prevent_evacuation printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id uuid in
  let vms = Client.Host.get_vms_which_prevent_evacuation rpc session_id host in

  let op (vm, error) =
    let error = String.concat "," error in
    let record = vm_record rpc session_id vm in
    let extra_field = make_field ~name:"reason" ~get:(fun () -> error) () in
    let record = { record with fields = record.fields @ [ extra_field ] } in
    let selected = List.hd (select_fields params [record] [ "uuid"; "name-label"; "reason"]) in
    let table = List.map print_field selected in
    printer (Cli_printer.PTable [table])
  in
  ignore(List.iter op vms)

let host_retrieve_wlb_evacuate_recommendations printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id uuid in
  let vms = Client.Host.retrieve_wlb_evacuate_recommendations rpc session_id host in
  let table = List.map (fun (vm, result) ->
      Printf.sprintf "%s (%s)" (Client.VM.get_uuid rpc session_id vm) (Client.VM.get_name_label rpc session_id vm),
      String.concat " " result) vms in
  printer (Cli_printer.PTable [ ("VM", "[Host, RecID] / Error") :: table ])

let host_shutdown_agent printer rpc session_id params =
  ignore(Client.Host.shutdown_agent rpc session_id)

let vdi_import fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let format =
    if List.mem_assoc "format" params
    then "&format=" ^ (List.assoc "format" params)
    else "" in
  let progress_bar = get_bool_param params "progress" in
  let make_command task_id =
    let prefix = uri_of_someone rpc session_id Master in
    let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s&vdi=%s%s"
        prefix Constants.import_raw_vdi_uri (Ref.string_of session_id)
        (Ref.string_of task_id) (Ref.string_of vdi) format in
    debug "requesting HttpPut('%s','%s')" filename uri;
    HttpPut (filename, uri) in
  ignore(track_http_operation ~progress_bar fd rpc session_id make_command "VDI import")

let vdi_export fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let vdi = Client.VDI.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let format =
    if List.mem_assoc "format" params
    then "&format=" ^ (List.assoc "format" params)
    else "" in
  let base =
    if List.mem_assoc "base" params
    then "&base=" ^ (List.assoc "base" params)
    else "" in
  let progress_bar = get_bool_param params "progress" in
  let make_command task_id =
    let prefix = uri_of_someone rpc session_id Master in
    let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s&vdi=%s%s%s"
        prefix Constants.export_raw_vdi_uri (Ref.string_of session_id)
        (Ref.string_of task_id) (Ref.string_of vdi) format base in
    debug "requesting HttpGet('%s','%s')" filename uri;
    HttpGet (filename, uri) in
  ignore(track_http_operation ~progress_bar fd rpc session_id make_command "VDI export")

let wait_for_task_complete rpc session_id task_id =
  let finished () =
    match (Client.Task.get_status rpc session_id task_id) with
      `success | `failure | `cancelled -> true
    | _ -> false in
  (* All successes and failures are communicated via the task object *)
  while not (finished ()) do
    Thread.delay 1.0
  done

let download_file ~__context rpc session_id task fd filename uri label =
  marshal fd (Command (HttpGet (filename, uri)));
  let response = ref (Response Wait) in
  while !response = Response Wait do response := unmarshal fd done;
  let ok =
    match !response with
    | Response OK -> true
    | Response Failed ->
      (* Need to check whether the thin cli managed to contact the server
         				   or not. If not, we need to mark the task as failed *)
      if Client.Task.get_progress rpc session_id task < 0.0
      then Db_actions.DB_Action.Task.set_status ~__context ~self:task ~value:`failure;
      false
    | _ -> false
  in
  wait_for_task_complete rpc session_id task;

  (* Check the server status -- even if the client thinks it's ok, we need
     	   to check that the server does too. *)
  match Client.Task.get_status rpc session_id task with
  | `success ->
    if ok
    then
      (if filename <> "" then
         marshal fd (Command (Print (Printf.sprintf "%s succeeded" label))))
    else
      (marshal fd (Command (PrintStderr (Printf.sprintf "%s failed, unknown error.\n" label)));
       raise (ExitWithError 1))
  | `failure ->
    let result = Client.Task.get_error_info rpc session_id task in
    if result = []
    then
      marshal fd (Command (PrintStderr (Printf.sprintf "%s failed, unknown error\n" label)))
    else
      raise (Api_errors.Server_error ((List.hd result),(List.tl result)))
  | `cancelled ->
    marshal fd (Command (PrintStderr (Printf.sprintf "%s cancelled\n" label)));
    raise (ExitWithError 1)
  | _ ->
    marshal fd (Command (PrintStderr "Internal error\n")); (* should never happen *)
    raise (ExitWithError 1)

let download_file_with_task fd rpc session_id filename uri query label
    task_name =
  let task = Client.Task.create rpc session_id task_name "" in

  (* Initially mark the task progress as -1.0. The first thing the HTTP handler does it to mark it as zero *)
  (* This is used as a flag to show that the 'ownership' of the task has been passed to the handler, and it's *)
  (* not our responsibility any more to mark the task as completed/failed/etc. *)
  let __context = Context.make task_name in
  Db_actions.DB_Action.Task.set_progress ~__context ~self:task ~value:(-1.0);
  finally
    (fun () ->
       download_file ~__context rpc session_id task fd filename
         (Printf.sprintf "%s?session_id=%s&task_id=%s%s%s" uri
            (Ref.string_of session_id)
            (Ref.string_of task)
            (if query = "" then "" else "&")
            query)
         label)
    (fun () -> Client.Task.destroy rpc session_id task)

let pool_retrieve_wlb_report fd printer rpc session_id params =
  let report = List.assoc "report" params in
  let filename = List.assoc_default "filename" params "" in
  let other_params =
    List.filter
      (fun (k, _) -> not (List.mem k (["report"; "filename"] @ stdparams)))
      params
  in
  download_file_with_task fd rpc session_id filename
    Constants.wlb_report_uri
    (Printf.sprintf
       "report=%s%s%s"
       (Http.urlencode report)
       (if List.length other_params = 0 then "" else "&")
       (String.concat "&"
          (List.map (fun (k, v) ->
               (Printf.sprintf "%s=%s"
                  (Http.urlencode k)
                  (Http.urlencode v))) other_params)))
    "Report generation"
    (Printf.sprintf "WLB report: %s" report)

let pool_retrieve_wlb_diagnostics fd printer rpc session_id params =
  let filename = List.assoc_default "filename" params "" in
  download_file_with_task fd rpc session_id filename
    Constants.wlb_diagnostics_uri ""
    "WLB diagnostics download"
    "WLB diagnostics download"

let vm_import fd printer rpc session_id params =
  let sr =
    if List.mem_assoc "sr-uuid" params
    then Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params)
    else
      match Cli_util.get_default_sr_uuid rpc session_id with
      | Some uuid ->  Client.SR.get_by_uuid rpc session_id uuid
      | None -> raise (Cli_util.Cli_failure "No SR specified and Pool default SR is null")
  in
  let _type = if List.mem_assoc "type" params
    then List.assoc "type" params
    else "default" in
  let full_restore = get_bool_param params "preserve" in
  let vm_metadata_only = get_bool_param params "metadata" in
  let force = get_bool_param params "force" in
  let dry_run = get_bool_param params "dry-run" in
  let vdi_map = read_map_params "vdi" params in
  if List.mem_assoc "url" params && List.mem_assoc "filename" params then begin
    marshal fd (Command (PrintStderr "Invalid arguments. The 'url' and 'filename' parameters should not both be specified.\n"));
    raise (ExitWithError 1)
  end;
  if (Vpx.serverType_of_string _type) <> Vpx.XenServer then begin
    let username = List.assoc "host-username" params in
    let password = List.assoc "host-password" params in
    let remote_config = read_map_params "remote-config" params in
    Client.VM.import_convert rpc session_id _type username password sr remote_config
  end
  else if List.mem_assoc "url" params then begin
    let url = List.assoc "url" params in
    let vm_refs = Client.VM.import ~rpc ~session_id ~url ~sr ~full_restore ~force in
    let uuids = List.map (fun vm -> Client.VM.get_uuid rpc session_id vm) vm_refs in
    marshal fd (Command (Print (String.concat "," uuids)))
  end else begin
    let filename = List.assoc "filename" params in
    if not vm_metadata_only && dry_run then begin
      marshal fd (Command (PrintStderr "Only metadata import function support dry-run\n"));
      raise (ExitWithError 1)
    end;

    (* Special-case where the user accidentally sets filename=<path to ova.xml file> *)
    let filename =
      if String.endswith "ova.xml" (String.lowercase_ascii filename)
      then String.sub filename 0 (String.length filename - (String.length "ova.xml"))
      else filename in

    marshal fd (Command (Load (filename ^ "/ova.xml")));
    match unmarshal fd with
    | Response OK ->
      debug "Looking like a Zurich/Geneva XVA";
      (* Zurich/Geneva style XVA import *)
      (* If a task was passed in, use that - else create a new one. UI uses "task_id" to pass reference [UI uses ThinCLI for Geneva import];
         						xe now allows task-uuid on cmd-line *)
      let using_existing_task = (List.mem_assoc "task_id" params) || (List.mem_assoc "task-uuid" params) in
      let importtask =
        if List.mem_assoc "task_id" params
        then (Ref.of_string (List.assoc "task_id" params))
        else if List.mem_assoc "task-uuid" params then Client.Task.get_by_uuid rpc session_id (List.assoc "task-uuid" params)
        else Client.Task.create rpc session_id "Import of Zurich/Geneva style XVA" ""
      in

      (* Initially mark the task progress as -1.0. The first thing the import handler does it to mark it as zero *)
      (* This is used as a flag to show that the 'ownership' of the task has been passed to the handler, and it's *)
      (* not our responsibility any more to mark the task as completed/failed/etc. *)
      let __context = Context.make "import" in
      Db_actions.DB_Action.Task.set_progress ~__context ~self:importtask ~value:(-1.0);

      Pervasiveext.finally (fun () ->
          begin
            let buffer = get_chunks fd in
            begin
              try
                let vm, vdis = Xva.of_xml (Xml.parse_string buffer) in
                (* Only import the first VM *)
                let vm = List.hd vm in
                let disks = List.sort compare (List.map (fun x -> x.Xva.device) vm.Xva.vbds) in
                let host =
                  if sr<>Ref.null
                  then Importexport.find_host_for_sr ~__context sr
                  else Helpers.get_localhost __context
                in
                let address = Client.Host.get_address rpc session_id host in
                (* Although it's inefficient use a loopback HTTP connection *)
                debug "address is: %s" address;
                let request = Xapi_http.http_request
                    ~cookie:(["session_id", Ref.string_of session_id;
                              "task_id", Ref.string_of importtask] @
                             (if sr <> Ref.null then [ "sr_id", Ref.string_of sr ] else []))
                    Http.Put Constants.import_uri in
                (* Stream the disk data from the client *)
                let writer (response, sock) =
                  try
                    (* First add the metadata file *)
                    let hdr = Tar_unix.Header.make Xva.xml_filename (Int64.of_int (String.length buffer)) in
                    Tar_unix.write_block hdr (fun ofd -> Unixext.really_write_string ofd buffer) sock;
                    List.iter
                      (fun vdi ->
                         let counter = ref 0 in
                         let finished = ref false in
                         while not(!finished) do
                           (* Nb.
                              														* The check for task cancelling is done here in the cli server. This is due to the fact that we've got
                              														* 3 parties talking to one another here: the thin cli, the cli server and the import handler. If the
                              														* import handler was checking, it would close its socket on task cancelling. This only happens after
                              														* each chunk is sent. Unfortunately the cli server wouldn't notice until it had already requested the
                              														* data from the thin cli, and would have to wait for it to finish sending its chunk before it could
                              														* alert it to the failure. *)

                           (let l=Client.Task.get_current_operations rpc session_id importtask in
                            if List.exists (fun (_,x) -> x=`cancel) l
                            then raise Api_errors.(Server_error(task_cancelled,[Ref.string_of importtask])));

                           (* Cancelling will close the connection, which will be interpreted by the import handler as failure *)

                           let chunk = Printf.sprintf "%s/chunk-%09d.gz" vdi !counter in
                           marshal fd (Command (Load (filename ^ "/" ^ chunk)));
                           match unmarshal fd with
                           | Response OK ->
                             (* A single chunk always follows the OK *)
                             let length = match unmarshal fd with
                               | Blob (Chunk x) -> x
                               | _ -> failwith "Thin CLI protocol error"
                             in
                             let hdr = Tar_unix.Header.make chunk (Int64.of_int32 length) in
                             Tar_unix.write_block hdr
                               (fun ofd ->
                                  let limit = Int64.of_int32 length in
                                  let total_bytes = Unixext.copy_file ~limit fd ofd in
                                  debug "File %s has size %Ld; we received %Ld%s" chunk limit total_bytes
                                    (if limit = total_bytes then "" else " ** truncated **")
                               )
                               sock;
                             (match unmarshal fd with | Blob End -> () | _ -> (failwith "Thin CLI protocol error"));
                             incr counter
                           | Response Failed ->
                             finished := true
                           | m ->
                             debug "Protocol failure: unexpected: %s" (string_of_message m)
                         done) disks;
                    Tar_unix.write_end sock;
                    true
                  with e ->
                    debug "vm_import caught %s while writing data" (Printexc.to_string e);
                    false
                in

                let open Xmlrpc_client in
                let transport = SSL(SSL.make ~use_stunnel_cache:true ~task_id:(Ref.string_of (Context.get_task_id __context)) (), address, !Xapi_globs.https_port) in
                let stream_ok = with_transport transport (with_http request writer) in
                if not stream_ok then
                  begin
                    (* If the progress is negative, we never got to talk to the import handler, and must complete *)
                    (* the task ourselves *)
                    if Client.Task.get_progress rpc session_id importtask < 0.0
                    then Db_actions.DB_Action.Task.set_status ~__context ~self:importtask ~value:`failure;
                  end;

                wait_for_task_complete rpc session_id importtask;
                (match Client.Task.get_status rpc session_id importtask with
                 | `success ->
                   if stream_ok then
                     let result = Client.Task.get_result rpc session_id importtask in
                     let vmrefs = API.Legacy.From.ref_VM_set "" (Xml.parse_string result) in
                     let uuids = List.map (fun vm -> Client.VM.get_uuid rpc session_id vm) vmrefs in
                     marshal fd (Command (Print (String.concat "," uuids)))
                   else
                     begin
                       marshal fd (Command (PrintStderr "Warning: Streaming failed, but task succeeded. Manual check required.\n"));
                       raise (ExitWithError 1)
                     end
                 | `failure ->
                   let result = Client.Task.get_error_info rpc session_id importtask in
                   if result = [] then
                     begin
                       marshal fd (Command (PrintStderr "Import failed, unknown error\n"));
                       raise (ExitWithError 1)
                     end
                   else Cli_util.server_error (List.hd result) (List.tl result) fd
                 | `cancelled ->
                   marshal fd (Command (PrintStderr "Import cancelled\n"));
                   raise (ExitWithError 1)
                 | _ ->
                   marshal fd (Command (PrintStderr "Internal error\n")); (* should never happen *)
                   raise (ExitWithError 1))
              with e ->
                marshal fd (Command (Debug ("Caught exception: " ^ (Printexc.to_string e))));
                marshal fd (Command (PrintStderr "Failed to import directory-format XVA\n"));
                debug "Import failed with exception: %s" (Printexc.to_string e);
                (if (Db_actions.DB_Action.Task.get_progress ~__context ~self:importtask = (-1.0))
                 then TaskHelper.failed ~__context:(Context.from_forwarded_task importtask) (Api_errors.Server_error(Api_errors.import_error_generic,[(Printexc.to_string e)]))
                );
                raise (ExitWithError 2)
            end
          end)
        (fun () ->
           if using_existing_task then () else Client.Task.destroy rpc session_id importtask)
    | Response Failed ->
      (* possibly a Rio import *)
      let make_command task_id =
        let prefix = uri_of_someone rpc session_id Master in
        let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s&restore=%b&force=%b&dry_run=%b%s%s"
            prefix
            (if vm_metadata_only then Constants.import_metadata_uri else Constants.import_uri)
            (Ref.string_of session_id) (Ref.string_of task_id) full_restore force dry_run
            (if sr <> Ref.null then "&sr_id=" ^ (Ref.string_of sr) else "")
            (String.concat "" (List.map (fun (a, b) -> "&vdi:" ^ a ^ "=" ^ b) vdi_map))
        in
        debug "requesting HttpPut('%s','%s')" filename uri;
        HttpPut (filename, uri) in
      let importtask =
        if List.mem_assoc "task-uuid" params then
          Some (Client.Task.get_by_uuid rpc session_id (List.assoc "task-uuid" params))
        else None (* track_http_operation will create one for us *) in
      let result = track_http_operation ?use_existing_task:importtask fd rpc session_id make_command "VM import" in
      let vmrefs = API.Legacy.From.ref_VM_set "" (Xml.parse_string result) in
      let uuids = List.map (fun vm -> Client.VM.get_uuid rpc session_id vm) vmrefs in
      let uuids = if uuids = [] && dry_run then ["xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"] else uuids in
      marshal fd (Command (Print (String.concat "," uuids)))
    | _ -> failwith "Thin CLI protocol error"
  end

let blob_get fd printer rpc session_id params =
  let blob_uuid = List.assoc "uuid" params in
  let blob_ref = Client.Blob.get_by_uuid rpc session_id blob_uuid in
  let filename = List.assoc "filename" params in
  let __context = Context.make "import" in
  let blobtask = Client.Task.create rpc session_id (Printf.sprintf "Obtaining blob, ref=%s" (Ref.string_of blob_ref)) "" in
  Db_actions.DB_Action.Task.set_progress ~__context ~self:blobtask ~value:(-1.0);

  let bloburi = Printf.sprintf "%s?session_id=%s&task_id=%s&ref=%s"
      (Constants.blob_uri) (Ref.string_of session_id) (Ref.string_of blobtask) (Ref.string_of blob_ref)
  in
  finally
    (fun () ->
       marshal fd (Command (HttpGet (filename, bloburi)));
       let response = ref (Response Wait) in
       while !response = Response Wait do response := unmarshal fd done;
       let ok = match !response with
         | Response OK -> true
         | Response Failed ->
           if Client.Task.get_progress rpc session_id blobtask < 0.0
           then Db_actions.DB_Action.Task.set_status ~__context ~self:blobtask ~value:`failure;
           false
         | _ -> false
       in

       wait_for_task_complete rpc session_id blobtask;

       (* if the client thinks it's ok, check that the server does too *)
       (match Client.Task.get_status rpc session_id blobtask with
        | `success ->
          if ok
          then (marshal fd (Command (Print "Blob get succeeded")))
          else (marshal fd (Command (PrintStderr "Blob get failed, unknown error.\n"));
                raise (ExitWithError 1))
        | `failure ->
          let result = Client.Task.get_error_info rpc session_id blobtask in
          if result = []
          then marshal fd (Command (PrintStderr "Blob get failed, unknown error\n"))
          else raise (Api_errors.Server_error ((List.hd result),(List.tl result)))
        | `cancelled ->
          marshal fd (Command (PrintStderr "Blob get cancelled\n"));
          raise (ExitWithError 1)
        | _ ->
          marshal fd (Command (PrintStderr "Internal error\n")); (* should never happen *)
          raise (ExitWithError 1)
       ))
    (fun () -> Client.Task.destroy rpc session_id blobtask)


let blob_put fd printer rpc session_id params =
  let blob_uuid = List.assoc "uuid" params in
  let blob_ref = Client.Blob.get_by_uuid rpc session_id blob_uuid in
  let filename = List.assoc "filename" params in
  let __context = Context.make "import" in
  let blobtask = Client.Task.create rpc session_id (Printf.sprintf "Blob PUT, ref=%s" (Ref.string_of blob_ref)) "" in
  Db_actions.DB_Action.Task.set_progress ~__context ~self:blobtask ~value:(-1.0);

  let bloburi = Printf.sprintf "%s?session_id=%s&task_id=%s&ref=%s"
      (Constants.blob_uri) (Ref.string_of session_id) (Ref.string_of blobtask) (Ref.string_of blob_ref)
  in
  finally
    (fun () ->
       marshal fd (Command (HttpPut (filename, bloburi)));
       let response = ref (Response Wait) in
       while !response = Response Wait do response := unmarshal fd done;
       let ok = match !response with
         | Response OK -> true
         | Response Failed ->
           if Client.Task.get_progress rpc session_id blobtask < 0.0
           then Db_actions.DB_Action.Task.set_status ~__context ~self:blobtask ~value:`failure;
           false
         | _ -> false
       in

       wait_for_task_complete rpc session_id blobtask;

       (* if the client thinks it's ok, check that the server does too *)
       (match Client.Task.get_status rpc session_id blobtask with
        | `success ->
          if ok
          then (marshal fd (Command (Print "Blob put succeeded")))
          else (marshal fd (Command (PrintStderr "Blob put failed, unknown error.\n"));
                raise (ExitWithError 1))
        | `failure ->
          let result = Client.Task.get_error_info rpc session_id blobtask in
          if result = []
          then marshal fd (Command (PrintStderr "Blob put failed, unknown error\n"))
          else raise (Api_errors.Server_error ((List.hd result),(List.tl result)))
        | `cancelled ->
          marshal fd (Command (PrintStderr "Blob put cancelled\n"));
          raise (ExitWithError 1)
        | _ ->
          marshal fd (Command (PrintStderr "Internal error\n")); (* should never happen *)
          raise (ExitWithError 1)
       ))
    (fun () -> Client.Task.destroy rpc session_id blobtask)

let blob_create printer rpc session_id params =
  let name = List.assoc "name" params in
  let mime_type = List.assoc_default "mime-type" params "" in
  let public = try bool_of_string "public" (List.assoc "public" params) with _ -> false in
  if (List.mem_assoc "vm-uuid" params) then
    begin
      let uuid = List.assoc "vm-uuid" params in
      let vm = Client.VM.get_by_uuid rpc session_id uuid in
      let blob = Client.VM.create_new_blob rpc session_id vm name mime_type public in
      let blob_uuid = Client.Blob.get_uuid rpc session_id blob in
      printer (Cli_printer.PList [blob_uuid])
    end
  else if (List.mem_assoc "pool-uuid" params) then
    begin
      let uuid = List.assoc "pool-uuid" params in
      let pool = Client.Pool.get_by_uuid rpc session_id uuid in
      let blob = Client.Pool.create_new_blob rpc session_id pool name mime_type public in
      let blob_uuid = Client.Blob.get_uuid rpc session_id blob in
      printer (Cli_printer.PList [blob_uuid])
    end
  else if (List.mem_assoc "sr-uuid" params) then
    begin
      let uuid = List.assoc "sr-uuid" params in
      let sr = Client.SR.get_by_uuid rpc session_id uuid in
      let blob = Client.SR.create_new_blob rpc session_id sr name mime_type public in
      let blob_uuid = Client.Blob.get_uuid rpc session_id blob in
      printer (Cli_printer.PList [blob_uuid])
    end
  else if (List.mem_assoc "host-uuid" params) then
    begin
      let uuid = List.assoc "host-uuid" params in
      let host = Client.Host.get_by_uuid rpc session_id uuid in
      let blob = Client.Host.create_new_blob rpc session_id host name mime_type public in
      let blob_uuid = Client.Blob.get_uuid rpc session_id blob in
      printer (Cli_printer.PList [blob_uuid])
    end
  else if (List.mem_assoc "network-uuid" params) then
    begin
      let uuid = List.assoc "network-uuid" params in
      let network = Client.Network.get_by_uuid rpc session_id uuid in
      let blob = Client.Network.create_new_blob rpc session_id network name mime_type public in
      let blob_uuid = Client.Blob.get_uuid rpc session_id blob in
      printer (Cli_printer.PList [blob_uuid])
    end
  else
    raise (Cli_util.Cli_failure "Need one of: vm-uuid, host-uuid, network-uuid, sr-uuid or pool-uuid")


let export_common fd printer rpc session_id params filename num ?task_uuid use_compression preserve_power_state vm =
  let vm_metadata_only : bool = get_bool_param params "metadata" in
  let export_snapshots : bool =
    if List.mem_assoc "include-snapshots" params
    then bool_of_string "include-snapshots" (List.assoc "include-snapshots" params)
    else vm_metadata_only in
  let vm_metadata_only = get_bool_param params "metadata" in
  let vm_record = vm.record () in
  let exporttask, task_destroy_fn =
    match task_uuid with
    | None -> (* manage task internally *)
      let exporttask = Client.Task.create rpc session_id (Printf.sprintf "Export of VM: %s" (vm_record.API.vM_uuid)) "" in
      (exporttask,(fun ()->Client.Task.destroy rpc session_id exporttask))
    | Some task_uuid -> (* do not destroy the task that has been received *)
      ((Client.Task.get_by_uuid rpc session_id task_uuid),(fun ()->()))
  in

  (* Initially mark the task progress as -1.0. The first thing the export handler does it to mark it as zero *)
  (* This is used as a flag to show that the 'ownership' of the task has been passed to the handler, and it's *)
  (* not our responsibility any more to mark the task as completed/failed/etc. *)
  let __context = Context.make "export" in
  Db_actions.DB_Action.Task.set_progress ~__context ~self:exporttask ~value:(-1.0);

  finally
    (fun () ->
       let f = if !num > 1 then filename ^ (string_of_int !num) else filename in
       download_file ~__context rpc session_id exporttask fd f
         (Printf.sprintf
            "%s?session_id=%s&task_id=%s&ref=%s&%s=%s&preserve_power_state=%b&export_snapshots=%b"
            (if vm_metadata_only then Constants.export_metadata_uri else Constants.export_uri)
            (Ref.string_of session_id)
            (Ref.string_of exporttask)
            (Ref.string_of (vm.getref ()))
            Constants.use_compression
            (if use_compression then "true" else "false")
            preserve_power_state
            export_snapshots)
         "Export";
       num := !num + 1)
    (fun () -> task_destroy_fn ())

let vm_export fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let use_compression = get_bool_param params "compress" in
  let preserve_power_state = get_bool_param params "preserve-power-state" in
  let task_uuid = if (List.mem_assoc "task-uuid" params) then Some (List.assoc "task-uuid" params) else None in
  let num = ref 1 in
  let op vm =
    export_common fd printer rpc session_id params filename num ?task_uuid use_compression preserve_power_state vm
  in
  ignore(do_vm_op printer rpc session_id op params ["filename"; "metadata"; "compress"; "preserve-power-state"; "include-snapshots"])

let vm_export_aux obj_type fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let use_compression = get_bool_param params "compress" in
  let preserve_power_state = get_bool_param params "preserve-power-state" in
  let num = ref 1 in
  let uuid = List.assoc (obj_type ^ "-uuid") params in
  let ref = Client.VM.get_by_uuid rpc session_id uuid in
  if obj_type = "template" && not (Client.VM.get_is_a_template rpc session_id ref) then
    failwith (Printf.sprintf "This operation can only be performed on a VM template. %s is not a VM template." uuid);
  if obj_type = "snapshot" && not (Client.VM.get_is_a_snapshot rpc session_id ref) then
    failwith (Printf.sprintf "This operation can only be performed on a VM snapshot. %s is not a VM snapshot." uuid);
  export_common fd printer rpc session_id params filename num use_compression preserve_power_state (vm_record rpc session_id ref)

let vm_copy_bios_strings printer rpc session_id params =
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
  let op vm =
    Client.VM.copy_bios_strings rpc session_id (vm.getref ()) host in
  ignore(do_vm_op printer rpc session_id op params ["host-uuid"])

let vm_is_bios_customized printer rpc session_id params =
  let op vm =
    let bios_strings = Client.VM.get_bios_strings rpc session_id (vm.getref ()) in
    if List.length bios_strings = 0 then
      printer (Cli_printer.PMsg "The BIOS strings of this VM have not yet been set.")
    else if bios_strings = Xapi_globs.generic_bios_strings then
      printer (Cli_printer.PMsg "This VM is BIOS-generic.")
    else
      printer (Cli_printer.PMsg "This VM is BIOS-customized.")
  in
  ignore(do_vm_op printer rpc session_id op params [])

let template_export fd printer = vm_export_aux "template" fd printer
let snapshot_export fd printer = vm_export_aux "snapshot" fd printer

let vm_vcpu_hotplug printer rpc session_id params =
  let vcpus=List.assoc "new-vcpus" params in
  let nvcpu =
    try
      Int64.of_string vcpus
    with
      _ -> failwith "Failed to parse parameter 'new-vcpus': expecting an integer"
  in
  let op vm =
    Client.VM.set_VCPUs_number_live ~rpc ~session_id ~self:(vm.getref ()) ~nvcpu
  in
  ignore(do_vm_op printer rpc session_id op params ["new-vcpus"])

let vm_vif_list printer rpc session_id params =
  let op vm =
    let vm_record = vm.record () in
    let vifs = vm_record.API.vM_VIFs in
    let table vif =
      let record = vif_record rpc session_id vif in
      let selected = List.hd (select_fields params [record] [ "uuid"; "device"; "MAC"; "network-uuid"; "network-name-label"; "vm-name-label"]) in
      List.map print_field selected in
    printer (Cli_printer.PTable (List.map table vifs))
  in
  ignore(do_vm_op printer rpc session_id op (("multiple","true")::params) ["params"]) (* always list multiple vms *)

let with_database_vdi rpc session_id params f =
  let database_params = read_map_params "database" params in
  let database_uuid =
    if List.mem_assoc "vdi-uuid" database_params then
      List.assoc "vdi-uuid" database_params
    else
      failwith "A parameter of the form 'database:vdi-uuid=<uuid>' must be specified to run this command."
  in
  let database_vdi = Client.VDI.get_by_uuid ~rpc ~session_id ~uuid:database_uuid in
  let database_session = Client.VDI.open_database ~rpc ~session_id ~self:database_vdi in
  finally
    (fun () -> f database_session)
    (fun () -> Client.Session.logout ~rpc ~session_id:database_session)

let vm_recover printer rpc session_id params =
  let force = get_bool_param params "force" in
  let uuid = List.assoc "uuid" params in
  with_database_vdi rpc session_id params
    (fun database_session ->
       let vm = Client.VM.get_by_uuid ~rpc ~session_id:database_session ~uuid in
       Client.VM.recover ~rpc ~session_id:database_session ~self:vm ~session_to:session_id ~force)

let vm_assert_can_be_recovered printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  with_database_vdi rpc session_id params
    (fun database_session ->
       let vm = Client.VM.get_by_uuid ~rpc ~session_id:database_session ~uuid in
       Client.VM.assert_can_be_recovered ~rpc ~session_id:database_session ~self:vm  ~session_to:session_id)

let cd_list printer rpc session_id params =
  let srs = Client.SR.get_all_records_where rpc session_id "true" in
  let cd_srs = List.filter (fun (sr,sr_record) -> sr_record.API.sR_content_type = "iso") srs in
  let cd_vdis = List.flatten (List.map (fun (sr,sr_record) -> Client.SR.get_VDIs rpc session_id sr) cd_srs) in
  let table cd =
    let record = vdi_record rpc session_id cd in
    let selected = List.hd (select_fields params [record] ["name-label"; "uuid"]) in
    List.map print_field selected in
  printer (Cli_printer.PTable (List.map table cd_vdis))

let validate_and_get_vlan params =
  try Int64.of_string (List.assoc "vlan" params)
  with _ -> failwith "Failed to parse parameter 'vlan': expecting an integer"

let vlan_create printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "network-uuid" params) in
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "pif-uuid" params) in
  let vLAN = validate_and_get_vlan params in
  let vlan = Client.VLAN.create rpc session_id pif vLAN network in
  let pif' = Client.VLAN.get_untagged_PIF rpc session_id vlan in
  let uuid = Client.PIF.get_uuid rpc session_id pif' in
  (* XXX: technically Rio displayed the PIF UUID here *)
  printer (Cli_printer.PList [uuid])

let pool_vlan_create printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "network-uuid" params) in
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "pif-uuid" params) in
  let vLAN = validate_and_get_vlan params in
  let vlan_pifs = Client.Pool.create_VLAN_from_PIF rpc session_id pif network vLAN in
  let vlan_pif_uuids = List.map (fun pif -> Client.PIF.get_uuid rpc session_id pif) vlan_pifs in
  (* XXX: technically Rio displayed the PIF UUID here *)
  printer (Cli_printer.PList vlan_pif_uuids)


let vlan_destroy printer rpc session_id params =
  (* Rio allowed a PIF UUID to be provided; support this mechanism *)
  let uuid = List.assoc "uuid" params in
  try
    let vlan = Client.VLAN.get_by_uuid rpc session_id uuid in
    Client.VLAN.destroy rpc session_id vlan
  with
  | Api_errors.Server_error(s,_) as e when s=Api_errors.handle_invalid || s=Api_errors.host_offline ->
    raise e
  | e ->
    let pif = try Some (Client.PIF.get_by_uuid rpc session_id uuid) with _ -> None in
    match pif with | Some pif -> Client.PIF.destroy rpc session_id pif | None -> raise e

let tunnel_create printer rpc session_id params =
  let network = Client.Network.get_by_uuid rpc session_id (List.assoc "network-uuid" params) in
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "pif-uuid" params) in
  let tunnel = Client.Tunnel.create rpc session_id pif network in
  let pif' = Client.Tunnel.get_access_PIF rpc session_id tunnel in
  let uuid = Client.PIF.get_uuid rpc session_id pif' in
  printer (Cli_printer.PList [uuid])

let tunnel_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let tunnel = Client.Tunnel.get_by_uuid rpc session_id uuid in
  Client.Tunnel.destroy rpc session_id tunnel

let pif_reconfigure_ip printer rpc session_id params =
  let read_optional_case_insensitive key =
    let lower_case_params = List.map (fun (k,v)->(String.lowercase_ascii k,v)) params in
    let lower_case_key = String.lowercase_ascii key in
    List.assoc_default lower_case_key lower_case_params "" in

  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let mode = Record_util.ip_configuration_mode_of_string (List.assoc "mode" params) in
  let ip = read_optional_case_insensitive "IP" in
  let netmask = List.assoc_default "netmask" params "" in
  let gateway = List.assoc_default "gateway" params "" in
  let dns = read_optional_case_insensitive "DNS" in
  let () = Client.PIF.reconfigure_ip rpc session_id pif mode ip netmask gateway dns in ()

let pif_reconfigure_ipv6 printer rpc session_id params =
  let read_optional_case_insensitive key =
    let lower_case_params = List.map (fun (k,v)->(String.lowercase_ascii k,v)) params in
    let lower_case_key = String.lowercase_ascii key in
    List.assoc_default lower_case_key lower_case_params "" in

  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let mode = Record_util.ipv6_configuration_mode_of_string (List.assoc "mode" params) in
  let ipv6 = read_optional_case_insensitive "IPv6" in
  let gateway = List.assoc_default "gateway" params "" in
  let dns = read_optional_case_insensitive "DNS" in
  let () = Client.PIF.reconfigure_ipv6 rpc session_id pif mode ipv6 gateway dns in ()

let pif_set_primary_address_type printer rpc session_id params =
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let address_type = Record_util.primary_address_type_of_string (List.assoc "primary_address_type" params) in
  let () = Client.PIF.set_primary_address_type rpc session_id pif address_type in ()

let pif_unplug printer rpc session_id params =
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let () = Client.PIF.unplug rpc session_id pif in ()

let pif_plug printer rpc session_id params =
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let () = Client.PIF.plug rpc session_id pif in ()

let pif_scan printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let () = Client.PIF.scan rpc session_id host in
  ()

let pif_introduce printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let mac = List.assoc_default "mac" params "" in
  let device = List.assoc "device" params in
  let managed = get_bool_param params ~default:true "managed" in
  let pif = Client.PIF.introduce rpc session_id host mac device managed in
  let uuid = Client.PIF.get_uuid rpc session_id pif in
  printer (Cli_printer.PList [uuid])

let pif_forget printer rpc session_id params =
  let pif_uuid = List.assoc "uuid" params in
  let pif = Client.PIF.get_by_uuid rpc session_id pif_uuid in
  let () = Client.PIF.forget rpc session_id pif in
  ()

let pif_db_forget printer rpc session_id params =
  let pif_uuid = List.assoc "uuid" params in
  let pif = Client.PIF.get_by_uuid rpc session_id pif_uuid in
  let () = Client.PIF.db_forget rpc session_id pif in
  ()

let bond_create printer rpc session_id params =
  let network = List.assoc "network-uuid" params in
  let mac = List.assoc_default "mac" params "" in
  let network = Client.Network.get_by_uuid rpc session_id network in
  let pifs = List.assoc "pif-uuids" params in
  let uuids = String.split ',' pifs in
  let pifs = List.map (fun uuid -> Client.PIF.get_by_uuid rpc session_id uuid) uuids in
  let mode = Record_util.bond_mode_of_string (List.assoc_default "mode" params "") in
  let properties = read_map_params "properties" params in
  let bond = Client.Bond.create rpc session_id network pifs mac mode properties in
  let uuid = Client.Bond.get_uuid rpc session_id bond in
  printer (Cli_printer.PList [ uuid])

let bond_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let bond = Client.Bond.get_by_uuid rpc session_id uuid in
  Client.Bond.destroy rpc session_id bond

let bond_set_mode printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let bond = Client.Bond.get_by_uuid rpc session_id uuid in
  let mode = Record_util.bond_mode_of_string (List.assoc_default "mode" params "") in
  Client.Bond.set_mode rpc session_id bond mode

let host_disable printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.disable rpc session_id (host.getref ())) params [])

let host_sync_data printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.sync_data rpc session_id (host.getref ())) params [])

(*
  BAD BAD MAN
  We remove the GUI-specific maintenance mode key in other config here
  to stop the gui from re-disabling the host

  http://scale.ad.xensource.com/browse/CA-12656
  Host doesn't exit from maintenance mode through CLI.

  This should be cleaned up at some point.
 *)
let host_enable printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host ->
      Client.Host.remove_from_other_config rpc session_id (host.getref ()) "MAINTENANCE_MODE";
      Client.Host.enable rpc session_id (host.getref ())) params [])

let host_shutdown printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.shutdown rpc session_id (host.getref ())) params [])

let host_reboot printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.reboot rpc session_id (host.getref ())) params [])

let host_power_on printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.power_on rpc session_id (host.getref ())) params [])

let host_prepare_for_poweroff _printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let host = Client.Host.get_by_uuid ~rpc ~session_id ~uuid in
  Client.Host.prepare_for_poweroff ~rpc ~session_id ~host

let host_dmesg printer rpc session_id params =
  let op _ host =
    let dmesg = Client.Host.dmesg rpc session_id (host.getref ()) in
    printer (Cli_printer.PList [ dmesg ])
  in
  ignore(do_host_op rpc session_id op params [])

let host_enable_local_storage_caching printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host ->
      let sr_uuid = List.assoc "sr-uuid" params in
      let sr = Client.SR.get_by_uuid rpc session_id sr_uuid in
      Client.Host.enable_local_storage_caching rpc session_id (host.getref ()) sr
    ) params ["sr-uuid"])

let host_disable_local_storage_caching printer rpc session_id params =
  ignore(do_host_op rpc session_id (fun _ host -> Client.Host.disable_local_storage_caching rpc session_id (host.getref ())) params [])

let pool_enable_local_storage_caching printer rpc session_id params =
  let pool = List.hd (Client.Pool.get_all rpc session_id) in
  Client.Pool.enable_local_storage_caching rpc session_id pool

let pool_disable_local_storage_caching printer rpc session_id params =
  let pool = List.hd (Client.Pool.get_all rpc session_id) in
  Client.Pool.disable_local_storage_caching rpc session_id pool

let pool_apply_edition printer rpc session_id params =
  let pool = get_pool_with_default rpc session_id params "uuid" in
  let edition = List.assoc "edition" params in
  let hosts = Client.Host.get_all rpc session_id in
  with_license_server_changes printer rpc session_id params hosts
    (fun rpc session_id -> Client.Pool.apply_edition rpc session_id pool edition)

let host_set_power_on_mode printer rpc session_id params =
  let power_on_mode = List.assoc "power-on-mode" params in
  let power_on_config = read_map_params "power-on-config" params in
  ignore(
    do_host_op rpc session_id (fun _ host -> Client.Host.set_power_on_mode ~rpc ~session_id ~self:(host.getref ()) ~power_on_mode ~power_on_config )
      params ["power-on-mode";"power-on-config"]
  )

let host_crash_upload printer rpc session_id params =
  let crash = Client.Host_crashdump.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let url = List.assoc_default "url" params "" in
  (* pass everything else in as an option *)
  let options = List.filter (fun (k, _) -> k <> "uuid" && k <> "url") params in
  Client.Host_crashdump.upload rpc session_id crash url options

let host_crash_destroy printer rpc session_id params =
  let crash = Client.Host_crashdump.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  Client.Host_crashdump.destroy rpc session_id crash

let host_bugreport_upload printer rpc session_id params =
  let op _ host =
    let url = List.assoc_default "url" params "" in
    (* pass everything else in as an option *)
    let options = List.filter (fun (k, _) -> k <> "host" && k <> "url") params in
    Client.Host.bugreport_upload rpc session_id (host.getref ()) url options
  in
  ignore(do_host_op rpc session_id op params ["url"; "http_proxy"])

let host_backup fd printer rpc session_id params =
  let op _ host =
    let filename = List.assoc "file-name" params in
    let prefix =
      let uuid = safe_get_field (field_lookup host.fields "uuid") in
      let someone = try SpecificHost (Client.Host.get_by_uuid rpc session_id uuid) with _ -> Master in
      uri_of_someone rpc session_id someone in
    let make_command task_id =
      let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s" prefix
          Constants.host_backup_uri (Ref.string_of session_id) (Ref.string_of task_id) in
      HttpGet (filename, uri) in
    ignore(track_http_operation fd rpc session_id make_command "host backup download")
  in
  ignore(do_host_op rpc session_id op params ["file-name"] ~multiple:false)

let pool_dump_db fd printer rpc session_id params =
  let filename = List.assoc "file-name" params in
  let make_command task_id =
    let pool = List.hd (Client.Pool.get_all rpc session_id) in
    let master = Client.Pool.get_master rpc session_id pool in
    let master_address = Client.Host.get_address rpc session_id master in
    let uri = Printf.sprintf "https://%s%s?session_id=%s&task_id=%s"
        master_address
        Constants.pool_xml_db_sync (Ref.string_of session_id) (Ref.string_of task_id) in
    debug "%s" uri;
    HttpGet (filename, uri) in
  ignore(track_http_operation fd rpc session_id make_command "dump database")

let pool_restore_db fd printer rpc session_id params =
  let dry_run = List.mem_assoc "dry-run" params in
  if not(List.mem_assoc "force" params) && not(dry_run)
  then failwith "This operation will restore the database backup to this host, making it the master. All slave hosts are assumed dead and they will be forgotten. This operation must be forced (use --force).";
  let filename = List.assoc "file-name" params in
  let make_command task_id =
    let prefix = uri_of_someone rpc session_id Master in
    let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s&dry_run=%b"
        prefix
        Constants.pool_xml_db_sync (Ref.string_of session_id) (Ref.string_of task_id)
        dry_run in
    debug "%s" uri;
    HttpPut (filename, uri) in
  ignore(track_http_operation fd rpc session_id make_command "restore database");
  if dry_run
  then printer (Cli_printer.PList [ "Dry-run backup restore successful" ])
  else printer (Cli_printer.PList ["Host will reboot with restored database in "^(string_of_float !Xapi_globs.db_restore_fuse_time)^" seconds..."])


let pool_enable_external_auth printer rpc session_id params =
  let pool = get_pool_with_default rpc session_id params "uuid" in
  let auth_type = List.assoc "auth-type" params in
  let service_name = List.assoc "service-name" params in
  let config = read_map_params "config" params in
  Client.Pool.enable_external_auth rpc session_id pool config service_name auth_type

let pool_disable_external_auth printer rpc session_id params =
  let pool = get_pool_with_default rpc session_id params "uuid" in
  let config = read_map_params "config" params in
  Client.Pool.disable_external_auth rpc session_id pool config

let host_restore fd printer rpc session_id params =
  let filename = List.assoc "file-name" params in
  let op _ host =
    let prefix =
      let uuid = safe_get_field (field_lookup host.fields "uuid") in
      let someone = try SpecificHost (Client.Host.get_by_uuid rpc session_id uuid) with _ -> Master in
      uri_of_someone rpc session_id someone in
    let make_command task_id =
      let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s" prefix
          Constants.host_restore_uri (Ref.string_of session_id) (Ref.string_of task_id) in
      HttpPut (filename, uri) in
    ignore(track_http_operation fd rpc session_id make_command "host backup upload")
  in
  ignore(do_host_op rpc session_id op params ["file-name"] ~multiple:false)


let host_get_system_status_capabilities printer rpc session_id params =
  printer (Cli_printer.PList
             (do_host_op rpc session_id
                (fun _ host ->
                   Client.Host.get_system_status_capabilities ~rpc ~session_id
                     ~host:(host.getref ())) params []))


let wait_for_task rpc session_id task __context fd op_str =
  let ok = match unmarshal fd with
    | Response OK -> true
    | Response Failed ->
      (* Need to check whether the thin cli managed to contact the server or
         			   not. If not, we need to mark the task as failed *)
      if Client.Task.get_progress rpc session_id task < 0.0
      then Db_actions.DB_Action.Task.set_status ~__context
          ~self:task ~value:`failure;
      false
    | _ -> false in
  wait_for_task_complete rpc session_id task;

  (* if the client thinks it's ok, check that the server does too *)
  (match Client.Task.get_status rpc session_id task with
   | `success ->
     if ok
     then (marshal fd (Command (Print (op_str ^ " succeeded"))))
     else (marshal fd (Command (PrintStderr (op_str ^ " failed, unknown error.\n")));
           raise (ExitWithError 1))
   | `failure ->
     let result = Client.Task.get_error_info rpc session_id task in
     if result = []
     then marshal fd (Command (PrintStderr (op_str ^ " failed, unknown error\n")))
     else raise (Api_errors.Server_error ((List.hd result),(List.tl result)))
   | `cancelled ->
     marshal fd (Command (PrintStderr (op_str ^ " cancelled\n")));
     raise (ExitWithError 1)
   | _ ->
     marshal fd (Command (PrintStderr "Internal error\n")); (* should never happen *)
     raise (ExitWithError 1)
  )

let host_get_system_status fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let entries = List.assoc_default "entries" params "" in
  let output = try List.assoc "output" params with _ -> "tar.bz2" in
  begin match output with "tar.bz2" | "tar" | "zip" -> () | _ ->
    failwith "Invalid output format. Must be 'tar', 'zip' or 'tar.bz2'" end;

  let op n host =
    let doit task_id =
      let uuid = safe_get_field (field_lookup host.fields "uuid") in
      let someone = try SpecificHost (Client.Host.get_by_uuid rpc session_id uuid) with _ -> Master in
      let prefix = uri_of_someone rpc session_id someone in

      let url =
        Printf.sprintf "%s%s?session_id=%s&entries=%s&output=%s&task_id=%s"
          prefix Constants.system_status_uri
          (Ref.string_of session_id) entries output
          (Ref.string_of task_id) in
      HttpGet (filename, url) in
    track_http_operation fd rpc session_id doit "system-status download"
  in
  ignore (do_host_op rpc session_id op params ["filename"; "entries"; "output"])

let host_set_hostname_live printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let hostname = List.assoc "host-name" params in
  Client.Host.set_hostname_live rpc session_id host hostname

let host_call_plugin printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let plugin = List.assoc "plugin" params in
  let fn = List.assoc "fn" params in
  let args = read_map_params "args" params in
  let result = Client.Host.call_plugin rpc session_id host plugin fn args in
  printer (Cli_printer.PList [ result ])

let host_enable_external_auth printer rpc session_id params =
  if not (List.mem_assoc "force" params) then
    failwith "This operation is provided only to recover individual hosts that are unable to access the external authentication service. This operation must be forced (use --force).";
  let host_uuid = List.assoc "host-uuid" params in
  let auth_type = List.assoc "auth-type" params in
  let service_name = List.assoc "service-name" params in
  let config = read_map_params "config" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  Client.Host.enable_external_auth rpc session_id host config service_name auth_type

let host_disable_external_auth printer rpc session_id params =
  if not (List.mem_assoc "force" params) then
    failwith "This operation is provided only to recover individual hosts that are unable to access the external authentication service. This operation must be forced (use --force).";
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  let config = read_map_params "config" params in
  Client.Host.disable_external_auth rpc session_id host config

let host_refresh_pack_info printer rpc session_id params =
  let host_uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id host_uuid in
  Client.Host.refresh_pack_info rpc session_id host

let host_cpu_info printer rpc session_id params =
  let host =
    if List.mem_assoc "uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "uuid" params)
    else
      get_host_from_session rpc session_id in
  let cpu_info = Client.Host.get_cpu_info rpc session_id host in
  printer (Cli_printer.PTable [cpu_info])

let host_get_cpu_features printer rpc session_id params =
  let host =
    if List.mem_assoc "uuid" params then
      Client.Host.get_by_uuid rpc session_id (List.assoc "uuid" params)
    else
      get_host_from_session rpc session_id in
  let cpu_info = Client.Host.get_cpu_info rpc session_id host in
  let features = List.assoc "features" cpu_info in
  printer (Cli_printer.PMsg features)

let host_enable_display printer rpc session_id params =
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let result = Client.Host.enable_display rpc session_id host in
  printer (Cli_printer.PMsg (Record_util.host_display_to_string result))

let host_disable_display printer rpc session_id params =
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "uuid" params) in
  let result = Client.Host.disable_display rpc session_id host in
  printer (Cli_printer.PMsg (Record_util.host_display_to_string result))

let set_update_vdi_name rpc session_id update_ref =
  let update_vdi = Client.Pool_update.get_vdi rpc session_id update_ref in
  let update_name = Client.Pool_update.get_name_label rpc session_id update_ref in
  Client.VDI.set_name_label rpc session_id update_vdi (Printf.sprintf "Update: %s" update_name)

let patch_upload fd printer rpc session_id params =
  let filename = List.assoc "file-name" params in
  let make_command task_id =
    let prefix = uri_of_someone rpc session_id Master in
    let pools = Client.Pool.get_all rpc session_id in
    let default_sr = Client.Pool.get_default_SR ~rpc ~session_id ~self:(List.hd pools) in
    let uri = Printf.sprintf "%s%s?session_id=%s&sr_id=%s&task_id=%s"
        prefix Constants.pool_patch_upload_uri (Ref.string_of session_id) (Ref.string_of default_sr)(Ref.string_of task_id) in
    let _ = debug "trying to post patch to uri:%s" uri in
    HttpPut (filename, uri) in
  let result = track_http_operation fd rpc session_id make_command "host patch upload" in
  let patch_ref = Ref.of_string result in
  let patch_uuid = Client.Pool_patch.get_uuid rpc session_id patch_ref in
  let update_ref = Client.Pool_patch.get_pool_update rpc session_id patch_ref in
  set_update_vdi_name rpc session_id update_ref;
  marshal fd (Command (Print patch_uuid))

let update_upload fd printer rpc session_id params =
  let filename = List.assoc "file-name" params in
  let make_command task_id =
    let prefix = uri_of_someone rpc session_id Master in
    let pools = Client.Pool.get_all rpc session_id in
    let sr =
      if List.mem_assoc "sr-uuid" params
      then Client.SR.get_by_uuid rpc session_id (List.assoc "sr-uuid" params)
      else begin
        let sr = Client.Pool.get_default_SR ~rpc ~session_id ~self:(List.hd pools) in
        if Cli_util.is_valid_ref session_id sr then sr
        else failwith "No sr-uuid parameter was given, and the pool's default SR \
                       is unspecified or invalid. Please explicitly specify the SR to use \
                       in the sr-uuid parameter, or set the pool's default SR."
      end
    in
    let uri = Printf.sprintf "%s%s?session_id=%s&sr_id=%s&task_id=%s"
        prefix Constants.import_raw_vdi_uri (Ref.string_of session_id) (Ref.string_of sr)(Ref.string_of task_id) in
    let _ = debug "trying to post patch to uri:%s" uri in
    HttpPut (filename, uri)
  in
  let result = track_http_operation fd rpc session_id make_command "host patch upload" in
  let vdi_ref = API.Legacy.From.ref_VDI "" (Xml.parse_string result) in
  let update_ref =
    try Client.Pool_update.introduce rpc session_id vdi_ref
    with e ->
      Client.VDI.destroy rpc session_id vdi_ref;
      raise e
  in
  let update_uuid = Client.Pool_update.get_uuid rpc session_id update_ref in
  set_update_vdi_name rpc session_id update_ref;
  marshal fd (Command (Print update_uuid))

let patch_clean printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id uuid in
  Client.Pool_patch.clean rpc session_id patch_ref

let patch_pool_clean printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id uuid in
  Client.Pool_patch.pool_clean rpc session_id patch_ref

let patch_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id uuid in
  Client.Pool_patch.destroy rpc session_id patch_ref

let patch_apply printer rpc session_id params =
  let patch_uuid = List.assoc "uuid" params in
  let host_uuid = List.assoc "host-uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id patch_uuid in
  let host_ref  = Client.Host.get_by_uuid rpc session_id host_uuid in
  let result = Client.Pool_patch.apply rpc session_id patch_ref host_ref in
  printer (Cli_printer.PList [ result ])

let patch_precheck printer rpc session_id params =
  let patch_uuid = List.assoc "uuid" params in
  let host_uuid = List.assoc "host-uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id patch_uuid in
  let host_ref  = Client.Host.get_by_uuid rpc session_id host_uuid in
  let result = Client.Pool_patch.precheck rpc session_id patch_ref host_ref in
  printer (Cli_printer.PList [ result ])

let patch_pool_apply printer rpc session_id params =
  let patch_uuid = List.assoc "uuid" params in
  let patch_ref = Client.Pool_patch.get_by_uuid rpc session_id patch_uuid in
  Client.Pool_patch.pool_apply rpc session_id patch_ref

let host_logs_download fd printer rpc session_id params =
  let op n host =
    let filename = if List.mem_assoc "file-name" params then List.assoc "file-name" params
      else
        let tm = Unix.gmtime (Unix.time ()) in
        Printf.sprintf "logs-%d-%d-%dT%02d%02d%02dZ"
          (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
          tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in
    let prefix =
      let uuid = safe_get_field (field_lookup host.fields "uuid") in
      let someone = try SpecificHost (Client.Host.get_by_uuid rpc session_id uuid) with _ -> Master in
      uri_of_someone rpc session_id someone in
    let filesuffix =
      if n=1 then "" else "-"^(safe_get_field (field_lookup host.fields "name-label"))
    in
    let make_command task_id =
      let uri = Printf.sprintf "%s%s?session_id=%s&task_id=%s" prefix
          Constants.host_logs_download_uri (Ref.string_of session_id) (Ref.string_of task_id) in
      HttpGet (filename^filesuffix, uri) in
    ignore(track_http_operation fd rpc session_id make_command "host logs download")
  in
  ignore(do_host_op rpc session_id op params ["file-name"])

let host_is_in_emergency_mode printer rpc session_id params =
  let mode = Client.Host.is_in_emergency_mode ~rpc ~session_id in
  printer (Cli_printer.PMsg (Printf.sprintf "%b" mode))

let host_emergency_management_reconfigure printer rpc session_id params =
  let interface = List.assoc "interface" params in
  Client.Host.local_management_reconfigure rpc session_id interface

let host_emergency_ha_disable printer rpc session_id params =
  let force = get_bool_param params "force" in
  let soft = get_bool_param params "soft" in
  if not force then failwith "This operation is extremely dangerous and may cause data loss. This operation must be forced (use --force).";
  Client.Host.emergency_ha_disable rpc session_id soft

let host_management_reconfigure printer rpc session_id params =
  let pif = Client.PIF.get_by_uuid rpc session_id (List.assoc "pif-uuid" params) in
  Client.Host.management_reconfigure rpc session_id pif

let host_management_disable printer rpc session_id params =
  Client.Host.management_disable rpc session_id

let host_signal_networking_change printer rpc session_id params =
  Client.Host.signal_networking_change rpc session_id

let host_notify printer rpc session_id params =
  let ty = List.assoc "type" params in
  let args = List.assoc_default "params" params "" in
  Client.Host.notify rpc session_id ty args

let host_syslog_reconfigure printer rpc session_id params =
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
  Client.Host.syslog_reconfigure rpc session_id host

let host_send_debug_keys printer rpc session_id params =
  let host = Client.Host.get_by_uuid rpc session_id (List.assoc "host-uuid" params) in
  let keys = List.assoc "keys" params in
  Client.Host.send_debug_keys rpc session_id host keys

(*
  let host_introduce printer rpc session_id params =
  let name = List.assoc "name" params in
  let descr = if List.mem_assoc "description" params then List.assoc "description" params else "" in
  let address = List.assoc "address" params in
  let port = List.assoc "remote-port" params in
  let remote_username = List.assoc "remote-username" params in
  let remote_password = List.assoc "remote-password" params in
  ignore(Client.Credential.create_with_password rpc session_id name descr address (Int64.of_string port) remote_username remote_password)
 *)

let task_cancel printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let task = Client.Task.get_by_uuid rpc session_id uuid in
  Client.Task.cancel rpc session_id task

(*
  let alert_create printer rpc session_id params =
  let string_to_alert_level s =
  match s with
  | "info"             -> `Info
  | "warning" | "warn" -> `Warn
  | "error"            -> `Error
  | _                  -> `Info
  in
  let message = List.assoc "message" params in
  let level = if List.mem_assoc "level" params then List.assoc "level" params else "info" in
  let level = string_to_alert_level level in
  let alert = Client.Alert.create rpc session_id message [] level in
  let uuid = Client.Alert.get_uuid rpc session_id alert in
  printer (Cli_printer.PList [uuid])

  let alert_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let alert = Client.Alert.get_by_uuid rpc session_id uuid in
  Client.Alert.destroy rpc session_id alert
 *)

(*
  let subject_list printer rpc session_id params =
(* we get all subjects from the pool *)
  let subjects = Client.Subject.get_all_records rpc session_id in
  let table_of_subject (subject,record) =
  [ "subject-uuid", record.API.subject_uuid;
  "subject-identifier", record.API.subject_subject_identifier;
(*  "subject-name", Client.Subject.get_subject_name rpc session_id subject;*)
  ] @
  record.API.subject_other_config
  in
  let all = List.map table_of_subject subjects in
  printer (Cli_printer.PTable all)
 *)

let subject_add printer rpc session_id params =
  let subject_name = List.assoc "subject-name" params in
  (* let's try to resolve the subject_name to a subject_id using the external directory *)
  let subject_identifier = Client.Auth.get_subject_identifier ~rpc ~session_id ~subject_name in
  (* obtains a list of name-value pairs with info about the subject from the external directory *)
  let subject_info = Client.Auth.get_subject_information_from_identifier ~rpc ~session_id ~subject_identifier in
  (* now we've got enough information to create our new subject in the pool *)
  let subject_ref = Client.Subject.create ~rpc ~session_id ~subject_identifier ~other_config:subject_info in
  let subject_uuid = Client.Subject.get_uuid rpc session_id subject_ref in
  printer (Cli_printer.PList [subject_uuid])

let subject_remove printer rpc session_id params =
  (* we are removing by subject-uuid *)
  let subject_uuid = List.assoc "subject-uuid" params in
  let subject = Client.Subject.get_by_uuid ~rpc ~session_id ~uuid:subject_uuid in
  Client.Subject.destroy ~rpc ~session_id ~self:subject

let subject_role_common rpc session_id params =
  let role_uuid = List.assoc_default "role-uuid" params "" in
  let role_name = List.assoc_default "role-name" params "" in
  if role_uuid="" && role_name=""
  then failwith "Required parameter not found: role-uuid or role-name"
  else
  if role_uuid<>"" && role_name<>""
  then failwith "Parameters role-uuid and role-name cannot be used together"
  else
    let subject_uuid = List.assoc "uuid" params in
    let role =
      if role_uuid<>""
      then Client.Role.get_by_uuid ~rpc ~session_id ~uuid:role_uuid
      else begin
        let roles = (Client.Role.get_by_name_label ~rpc ~session_id ~label:role_name) in
        if List.length roles > 0
        then List.hd roles (* names are unique, there's either 0 or 1*)
        else Ref.null (*role not found* raise (Api_errors.Server_error (Api_errors.role_not_found, []))*)
      end
    in
    let subject = Client.Subject.get_by_uuid ~rpc ~session_id ~uuid:subject_uuid in
    (subject,role)

let subject_role_add printer rpc session_id params =
  let (subject,role) = subject_role_common rpc session_id params in
  Client.Subject.add_to_roles ~rpc ~session_id ~self:subject ~role

let subject_role_remove printer rpc session_id params =
  let (subject,role) = subject_role_common rpc session_id params in
  Client.Subject.remove_from_roles ~rpc ~session_id ~self:subject ~role

let audit_log_get fd printer rpc session_id params =
  let filename = List.assoc "filename" params in
  let since =
    if List.mem_assoc "since" params
    then (* make sure since has a reasonable length *)
      let unsanitized_since = List.assoc "since" params in
      if String.length unsanitized_since > 255
      then String.sub unsanitized_since 0 255
      else unsanitized_since
    else ""
  in
  let label = Printf.sprintf "audit-log-get%sinto file %s"
      (if since="" then " " else Printf.sprintf " (since \"%s\") " since)
      (if String.length filename <= 255
       then filename (* make sure filename has a reasonable length in the logs *)
       else String.sub filename 0 255
      )
  in
  let query =
    if since="" then ""
    else Printf.sprintf "since=%s" (Http.urlencode since)
  in
  download_file_with_task
    fd rpc session_id filename Constants.audit_log_uri query label label

(* RBAC 2.0 only
   let role_create printer rpc session_id params =
   (*let id = List.assoc "id" params in*)
   let name = List.assoc "name" params in
   ignore (Client.Role.create ~rpc ~session_id ~name ~description:"" ~permissions:[] ~is_basic:false ~is_complete:false)
*)

let session_subject_identifier_list printer rpc session_id params =
  let subject_identifiers = Client.Session.get_all_subject_identifiers ~rpc ~session_id in
  let table_of_subject_identifiers subject_identifier =
    [ "subject-identifier ( RO)", subject_identifier ]
  in
  let all = List.map table_of_subject_identifiers subject_identifiers in
  printer (Cli_printer.PTable all)

let session_subject_identifier_logout printer rpc session_id params =
  let subject_identifier = List.assoc "subject-identifier" params in
  Client.Session.logout_subject_identifier ~rpc ~session_id ~subject_identifier

let session_subject_identifier_logout_all printer rpc session_id params =
  let subject_identifiers = Client.Session.get_all_subject_identifiers ~rpc ~session_id in
  List.iter (fun subject_identifier -> Client.Session.logout_subject_identifier ~rpc ~session_id ~subject_identifier) subject_identifiers

let secret_create printer rpc session_id params =
  let value = List.assoc "value" params in
  let other_config = read_map_params "other-config" params in
  let ref = Client.Secret.create ~rpc ~session_id ~value ~other_config in
  let uuid = Client.Secret.get_uuid ~rpc ~session_id ~self:ref in
  printer (Cli_printer.PList [uuid])

let secret_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.Secret.get_by_uuid ~rpc ~session_id ~uuid in
  Client.Secret.destroy ~rpc ~session_id ~self:ref

let vmss_create printer rpc session_id params =
  let get ?default param_name =
    if List.mem_assoc param_name params
    then List.assoc param_name params
    else match default with
      | Some default_value -> default_value
      | None -> failwith ("No default value for parameter "^param_name)
  in
  let name_label = List.assoc "name-label" params in
  let ty = Record_util.string_to_vmss_type(get "type") in
  let frequency = Record_util.string_to_vmss_frequency(get "frequency") in
  let schedule = read_map_params "schedule" params in
  (* optional parameters with default values *)
  let name_description = get "name-description" ~default:"" in
  let enabled = Record_util.bool_of_string(get "enabled" ~default:"true") in
  let retained_snapshots = Int64.of_string(get "retained-snapshots" ~default:"7") in
  let ref = Client.VMSS.create ~rpc ~session_id ~name_label ~name_description
      ~enabled ~_type:ty ~retained_snapshots ~frequency ~schedule
  in
  let uuid = Client.VMSS.get_uuid ~rpc ~session_id ~self:ref in
  printer (Cli_printer.PList [uuid])

let vmss_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.VMSS.get_by_uuid ~rpc ~session_id ~uuid in
  Client.VMSS.destroy ~rpc ~session_id ~self:ref

let vm_appliance_create printer rpc session_id params =
  let name_label = List.assoc "name-label" params in
  let name_description =
    if List.mem_assoc "name-description" params then
      List.assoc "name-description" params
    else ""
  in
  let ref = Client.VM_appliance.create ~rpc ~session_id ~name_label ~name_description in
  let uuid = Client.VM_appliance.get_uuid ~rpc ~session_id ~self:ref in
  printer (Cli_printer.PList [uuid])

let vm_appliance_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.VM_appliance.get_by_uuid ~rpc ~session_id ~uuid in
  Client.VM_appliance.destroy ~rpc ~session_id ~self:ref

let vm_appliance_start printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let paused = get_bool_param params "paused" in
  let ref = Client.VM_appliance.get_by_uuid ~rpc ~session_id ~uuid in
  Client.VM_appliance.start ~rpc ~session_id ~self:ref ~paused

let vm_appliance_shutdown printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let force = get_bool_param params "force" in
  let ref = Client.VM_appliance.get_by_uuid ~rpc ~session_id ~uuid in
  if force then
    Client.VM_appliance.hard_shutdown ~rpc ~session_id ~self:ref
  else
    Client.VM_appliance.clean_shutdown ~rpc ~session_id ~self:ref

let vm_appliance_recover printer rpc session_id params =
  let force = get_bool_param params "force" in
  let uuid = List.assoc "uuid" params in
  with_database_vdi rpc session_id params
    (fun database_session ->
       let appliance = Client.VM_appliance.get_by_uuid ~rpc ~session_id:database_session ~uuid in
       Client.VM_appliance.recover ~rpc ~session_id:database_session ~self:appliance ~session_to:session_id ~force)

let vm_appliance_assert_can_be_recovered printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  with_database_vdi rpc session_id params
    (fun database_session ->
       let appliance = Client.VM_appliance.get_by_uuid ~rpc ~session_id:database_session ~uuid in
       Client.VM_appliance.assert_can_be_recovered ~rpc ~session_id:database_session ~self:appliance  ~session_to:session_id)

let gpu_group_create printer rpc session_id params =
  let name_label = List.assoc "name-label" params in
  let name_description =
    try List.assoc "name-description" params
    with Not_found -> ""
  in
  let gpu_group =
    Client.GPU_group.create ~rpc ~session_id
      ~name_label ~name_description ~other_config:[]
  in
  let uuid = Client.GPU_group.get_uuid ~rpc ~session_id ~self:gpu_group in
  printer (Cli_printer.PList [uuid])

let gpu_group_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let gpu_group = Client.GPU_group.get_by_uuid ~rpc ~session_id ~uuid in
  Client.GPU_group.destroy ~rpc ~session_id ~self:gpu_group

let gpu_group_get_remaining_capacity printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vgpu_type_uuid = List.assoc "vgpu-type-uuid" params in
  let gpu_group = Client.GPU_group.get_by_uuid ~rpc ~session_id ~uuid in
  let vgpu_type =
    Client.VGPU_type.get_by_uuid ~rpc ~session_id ~uuid:vgpu_type_uuid
  in
  let result = Client.GPU_group.get_remaining_capacity ~rpc ~session_id
      ~self:gpu_group ~vgpu_type in
  printer (Cli_printer.PMsg (Int64.to_string result))

let vgpu_create printer rpc session_id params =
  let device = if List.mem_assoc "device" params then List.assoc "device" params else "0" in
  let gpu_group_uuid = List.assoc "gpu-group-uuid" params in
  let vm_uuid=List.assoc "vm-uuid" params in
  let vM=Client.VM.get_by_uuid rpc session_id vm_uuid in
  let gPU_group=Client.GPU_group.get_by_uuid rpc session_id gpu_group_uuid in
  let _type =
    if List.mem_assoc "vgpu-type-uuid" params
    then Client.VGPU_type.get_by_uuid rpc session_id (List.assoc "vgpu-type-uuid" params)
    else Ref.null
  in
  let vgpu = Client.VGPU.create ~rpc ~session_id ~device ~gPU_group ~vM ~other_config:[] ~_type in
  let uuid = Client.VGPU.get_uuid rpc session_id vgpu in
  printer (Cli_printer.PList [uuid])

let vgpu_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let vgpu = Client.VGPU.get_by_uuid rpc session_id uuid in
  Client.VGPU.destroy rpc session_id vgpu

let dr_task_create printer rpc session_id params =
  let _type = List.assoc "type" params in
  let device_config = parse_device_config params in
  let whitelist = if List.mem_assoc "sr-whitelist" params then String.split ',' (List.assoc "sr-whitelist" params) else [] in
  let dr_task = Client.DR_task.create ~rpc ~session_id ~_type ~device_config ~whitelist in
  let uuid = Client.DR_task.get_uuid ~rpc ~session_id ~self:dr_task in
  printer (Cli_printer.PList [uuid])

let dr_task_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.DR_task.get_by_uuid ~rpc ~session_id ~uuid in
  Client.DR_task.destroy ~rpc ~session_id ~self:ref

let pgpu_enable_dom0_access printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.PGPU.get_by_uuid rpc session_id uuid in
  let result = Client.PGPU.enable_dom0_access rpc session_id ref in
  printer (Cli_printer.PMsg (Record_util.pgpu_dom0_access_to_string result))

let pgpu_disable_dom0_access printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.PGPU.get_by_uuid rpc session_id uuid in
  let result = Client.PGPU.disable_dom0_access rpc session_id ref in
  printer (Cli_printer.PMsg (Record_util.pgpu_dom0_access_to_string result))

let lvhd_enable_thin_provisioning printer rpc session_id params =
  let sr_uuid = List.assoc "sr-uuid" params in
  let initial_allocation = Record_util.bytes_of_string "initial-allocation" (List.assoc "initial-allocation" params) in
  let allocation_quantum = Record_util.bytes_of_string "allocation-quantum" (List.assoc "allocation-quantum" params) in
  ignore(
    do_host_op rpc session_id (fun _ host ->
        let host_ref = host.getref () in
        let sr_ref = Client.SR.get_by_uuid rpc session_id sr_uuid in
        Client.LVHD.enable_thin_provisioning rpc session_id host_ref sr_ref initial_allocation allocation_quantum
      ) params ["sr-uuid"; "initial-allocation";"allocation-quantum"]
  )

module PVS_site = struct
  let introduce printer rpc session_id params =
    let name_label = List.assoc "name-label" params in
    let name_description =
      try List.assoc "name-description" params
      with Not_found -> ""
    in		let pVS_uuid =
          try List.assoc "pvs-uuid" params
          with Not_found -> ""
    in
    let ref   = Client.PVS_site.introduce ~rpc ~session_id ~name_label
        ~name_description ~pVS_uuid in
    let uuid  = Client.PVS_site.get_uuid rpc session_id ref in
    printer (Cli_printer.PList [uuid])

  let forget printer rpc session_id params =
    let uuid  = List.assoc "uuid" params in
    let ref   = Client.PVS_site.get_by_uuid ~rpc ~session_id ~uuid in
    Client.PVS_site.forget rpc session_id ref
end
module PVS_server = struct
  let introduce printer rpc session_id params =
    let addresses  = List.assoc "addresses" params   |> String.split ',' in
    let first_port = List.assoc "first-port" params  |> Int64.of_string in
    let last_port  = List.assoc "last-port" params   |> Int64.of_string in
    let site_uuid  = List.assoc "pvs-site-uuid" params in
    let site = Client.PVS_site.get_by_uuid
        ~rpc ~session_id ~uuid:site_uuid in
    let ref = Client.PVS_server.introduce
        ~rpc ~session_id ~addresses ~first_port ~last_port ~site in
    let uuid = Client.PVS_server.get_uuid ~rpc ~session_id ~self:ref in
    printer (Cli_printer.PList [uuid])

  let forget printer rpc session_id params =
    let uuid  = List.assoc "uuid" params in
    let ref   = Client.PVS_server.get_by_uuid ~rpc ~session_id ~uuid in
    Client.PVS_server.forget rpc session_id ref
end

module PVS_proxy = struct
  let create printer rpc session_id params =
    let site_uuid  = List.assoc "pvs-site-uuid" params in
    let site = Client.PVS_site.get_by_uuid ~rpc ~session_id ~uuid:site_uuid in
    let vif_uuid  = List.assoc "vif-uuid" params in
    let vIF = Client.VIF.get_by_uuid ~rpc ~session_id ~uuid:vif_uuid in
    let ref = Client.PVS_proxy.create
        ~rpc ~session_id ~site ~vIF in
    let uuid = Client.PVS_proxy.get_uuid rpc session_id ref in
    printer (Cli_printer.PList [uuid])

  let destroy printer rpc session_id params =
    let uuid  = List.assoc "uuid" params in
    let ref   = Client.PVS_proxy.get_by_uuid ~rpc ~session_id ~uuid in
    Client.PVS_proxy.destroy rpc session_id ref
end

module PVS_cache_storage = struct
  let create printer rpc session_id params =
    ignore (
      do_host_op rpc session_id ~multiple:false (fun _ host ->
          let sr_uuid  = List.assoc "sr-uuid" params in
          let sR = Client.SR.get_by_uuid ~rpc ~session_id ~uuid:sr_uuid in
          let site_uuid  = List.assoc "pvs-site-uuid" params in
          let site = Client.PVS_site.get_by_uuid ~rpc ~session_id ~uuid:site_uuid in
          let size = Record_util.bytes_of_string "size" (List.assoc "size" params) in
          let ref = Client.PVS_cache_storage.create
              ~rpc ~session_id ~host:(host.getref ()) ~sR ~site ~size in
          let uuid = Client.PVS_cache_storage.get_uuid rpc session_id ref in
          printer (Cli_printer.PList [uuid])
        ) params ["sr-uuid"; "pvs-site-uuid"; "size"]
    )

  let destroy printer rpc session_id params =
    let uuid  = List.assoc "uuid" params in
    let ref   = Client.PVS_cache_storage.get_by_uuid ~rpc ~session_id ~uuid in
    Client.PVS_cache_storage.destroy rpc session_id ref
end

let update_introduce printer rpc session_id params =
  let vdi_uuid = List.assoc "vdi-uuid" params in
  let vdi_ref = Client.VDI.get_by_uuid rpc session_id vdi_uuid in
  let update = Client.Pool_update.introduce rpc session_id vdi_ref in
  let uuid = Client.Pool_update.get_uuid ~rpc ~session_id ~self:update in
  printer (Cli_printer.PList [uuid])

let livepatch_status_to_string state =
  match state with
  | `ok -> "Ok."
  | `ok_livepatch_complete -> "Ok: Patch can be applied without reboot."
  | `ok_livepatch_incomplete -> "Ok: Patch can be applied, but a reboot will be required."

let update_precheck printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let result = do_host_op rpc session_id (fun _ host ->
      let host_ref = host.getref () in
      let ref = Client.Pool_update.get_by_uuid rpc session_id uuid in
      Client.Pool_update.precheck rpc session_id ref host_ref ) params ["uuid"] in
  let result_msg = List.map livepatch_status_to_string result in
  printer (Cli_printer.PList result_msg)

let update_apply printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  ignore(
    do_host_op rpc session_id (fun _ host ->
        let host_ref = host.getref () in
        let ref = Client.Pool_update.get_by_uuid rpc session_id uuid in
        Client.Pool_update.apply rpc session_id ref host_ref
      ) params ["uuid"]
  )

let update_pool_apply printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.Pool_update.get_by_uuid rpc session_id uuid in
  Client.Pool_update.pool_apply rpc session_id ref

let update_pool_clean printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.Pool_update.get_by_uuid rpc session_id uuid in
  Client.Pool_update.pool_clean rpc session_id ref

let update_destroy printer rpc session_id params =
  let uuid = List.assoc "uuid" params in
  let ref = Client.Pool_update.get_by_uuid rpc session_id uuid in
  Client.Pool_update.destroy rpc session_id ref

let update_resync_host printer rpc session_id params =
  let uuid = List.assoc "host-uuid" params in
  let host = Client.Host.get_by_uuid rpc session_id uuid in
  Client.Pool_update.resync_host rpc session_id host

module SDN_controller = struct
  let introduce printer rpc session_id params =
    let port =
      if List.mem_assoc "tcp-port" params
      then
        try Int64.of_string (List.assoc "tcp-port" params)
        with _ -> failwith "port field should be an integer"
      else 0L
    in
    let protocol =
      if List.mem_assoc "protocol" params
      then
        Record_util.sdn_protocol_of_string (List.assoc "protocol" params)
      else
        `ssl
    in
    let address =
      if List.mem_assoc "address" params
      then
        List.assoc "address" params
      else ""
    in
    let sdn = Client.SDN_controller.introduce rpc session_id protocol address port in
    let uuid = Client.SDN_controller.get_uuid ~rpc ~session_id ~self:sdn in
    printer (Cli_printer.PList [uuid])

  let forget printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.SDN_controller.get_by_uuid rpc session_id uuid in
    Client.SDN_controller.forget rpc session_id ref
end

module PUSB = struct
  let scan printer rpc session_id params =
    let host_uuid = List.assoc "host-uuid" params in
    let host = Client.Host.get_by_uuid rpc session_id host_uuid in
    Client.PUSB.scan rpc session_id host
end

module VUSB = struct
  let create printer rpc session_id params =
    let vM = Client.VM.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "vm-uuid" params) in
    let uSB_group = Client.USB_group.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "usb-group-uuid" params) in
    let vusb = Client.VUSB.create ~rpc ~session_id ~vM ~uSB_group ~other_config:[] in
    let uuid = Client.VUSB.get_uuid rpc session_id vusb in
    printer (Cli_printer.PList [uuid])

  let unplug printer rpc session_id params =
    let vusb = Client.VUSB.get_by_uuid ~rpc ~session_id ~uuid:(List.assoc "uuid" params) in
    Client.VUSB.unplug rpc session_id vusb

  let destroy printer rpc session_id params =
    let vusb = Client.VUSB.get_by_uuid rpc session_id (List.assoc "uuid" params) in
    ignore(Client.VUSB.destroy rpc session_id vusb)
end

module Cluster = struct
  let pool_create printer rpc session_id params =
    let network_uuid = List.assoc "network-uuid" params in
    let cluster_stack = get_param params "cluster-stack" ~default:"corosync" in
    let network = Client.Network.get_by_uuid rpc session_id network_uuid in
    let token_timeout = get_float_param params "token-timeout" ~default:Constants.default_token_timeout_s in
    let token_timeout_coefficient = get_float_param params "token-timeout-coefficient" ~default:Constants.default_token_timeout_coefficient_s in
    let cluster = Client.Cluster.pool_create ~rpc ~session_id ~network ~cluster_stack ~token_timeout ~token_timeout_coefficient in
    let uuid = Client.Cluster.get_uuid ~rpc ~session_id ~self:cluster in
    printer (Cli_printer.PList [uuid])

  let pool_force_destroy _printer rpc session_id params =
    let cluster_uuid = List.assoc "cluster-uuid" params in
    let cluster_ref = Client.Cluster.get_by_uuid rpc session_id cluster_uuid in
    Client.Cluster.pool_force_destroy ~rpc ~session_id ~self:cluster_ref

  let pool_destroy _printer rpc session_id params =
    let cluster_uuid = List.assoc "cluster-uuid" params in
    let cluster_ref = Client.Cluster.get_by_uuid rpc session_id cluster_uuid in
    Client.Cluster.pool_destroy ~rpc ~session_id ~self:cluster_ref

  let pool_resync printer rpc session_id params =
    let cluster = List.assoc "cluster-uuid" params in
    let cluster_ref = Client.Cluster.get_by_uuid rpc session_id cluster in
    Client.Cluster.pool_resync rpc session_id cluster_ref

  let create printer rpc session_id params =
    let network_uuid = List.assoc "network-uuid" params in
    let cluster_stack = get_param params "cluster-stack" ~default:"corosync" in
    let pool_auto_join = get_bool_param params "pool-auto-join" ~default:true in
    let token_timeout = get_float_param params "token-timeout" ~default:Constants.default_token_timeout_s in
    let token_timeout_coefficient = get_float_param params "token-timeout-coefficient" ~default:Constants.default_token_timeout_coefficient_s in
    let network = Client.Network.get_by_uuid rpc session_id network_uuid in
    let cluster = Client.Cluster.create ~rpc ~session_id ~network ~cluster_stack ~pool_auto_join ~token_timeout ~token_timeout_coefficient in
    let uuid = Client.Cluster.get_uuid ~rpc ~session_id ~self:cluster in
    printer (Cli_printer.PList [uuid])

  let destroy printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.Cluster.get_by_uuid rpc session_id uuid in
    Client.Cluster.destroy rpc session_id ref
end

module Cluster_host = struct
  let create printer rpc session_id params =
    let cluster_uuid = List.assoc "cluster-uuid" params in
    let host_uuid = List.assoc "host-uuid" params in
    let cluster_ref = Client.Cluster.get_by_uuid rpc session_id cluster_uuid in
    let host_ref = Client.Host.get_by_uuid rpc session_id host_uuid in
    let cluster_host = Client.Cluster_host.create rpc session_id cluster_ref host_ref in
    let uuid = Client.Cluster_host.get_uuid ~rpc ~session_id ~self:cluster_host in
    printer (Cli_printer.PList [uuid])

  let enable printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.Cluster_host.get_by_uuid rpc session_id uuid in
    Client.Cluster_host.enable rpc session_id ref

  let disable printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.Cluster_host.get_by_uuid rpc session_id uuid in
    Client.Cluster_host.disable rpc session_id ref

  let destroy printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.Cluster_host.get_by_uuid rpc session_id uuid in
    Client.Cluster_host.destroy rpc session_id ref

  let force_destroy printer rpc session_id params =
    let uuid = List.assoc "uuid" params in
    let ref = Client.Cluster_host.get_by_uuid rpc session_id uuid in
    Client.Cluster_host.force_destroy rpc session_id ref
end
