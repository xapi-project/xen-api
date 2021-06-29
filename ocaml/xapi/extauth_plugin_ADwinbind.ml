(*
 * Copyright (C) Citrix Systems Inc.
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
 * @winbind group Access Control
*)
module D = Debug.Make (struct
  let name = "extauth_plugin_ADwinbind"
end)

open D
open Xapi_stdext_std.Xstringext
open Auth_signature

let krbtgt = "KRBTGT"

let ( let* ) = Result.bind

let ( <!> ) x f = Rresult.R.reword_error f x

let ( >>| ) = Rresult.( >>| )

let maybe_raise (x : ('a, exn) result) : 'a =
  match x with Ok x -> x | Error e -> raise e

let maybe_raise_not_found (x : ('a, exn) result) : 'a =
  match x with
  | Ok x ->
      x
  | Error e ->
      D.error "found an exception, raising Not_found instead. ex: %s"
        (Printexc.to_string e) ;
      raise Not_found

let auth_ex uname =
  let msg = Printf.sprintf "failed to authenticate user '%s'" uname in
  Auth_signature.(Auth_failure msg)

let generic_ex fmt =
  Printf.ksprintf
    (fun msg -> Auth_signature.(Auth_service_error (E_GENERIC, msg)))
    fmt

let net_cmd = !Xapi_globs.net_cmd

let wb_cmd = !Xapi_globs.wb_cmd

let tdb_tool = !Xapi_globs.tdb_tool

let debug_level () = !Xapi_globs.winbind_debug_level |> string_of_int

let ntlm_auth uname passwd : (unit, exn) result =
  try
    let args = ["--username"; uname] in
    let _stdout =
      Helpers.call_script ~log_output:Never ~stdin:passwd
        !Xapi_globs.ntlm_auth_cmd args
    in
    Ok ()
  with _ -> Error (auth_ex uname)

module Ldap = struct
  type user = {
      upn: string
    ; account_disabled: bool
    ; account_expired: bool
    ; account_locked: bool
    ; password_expired: bool
  }
  [@@deriving rpcty]

  let string_of_user x =
    Rpcmarshal.marshal user.Rpc.Types.ty x |> Jsonrpc.to_string

  let parse_user stdout : (user, string) result =
    (* there are two steps here:
     * 1. parse stdout to a (string, string) map
     * 2. calculate user details using the map *)
    let module Map = Map.Make (String) in
    let module P = struct
      open Angstrom

      let space = char ' '

      let not_spaces = take_while @@ function ' ' -> false | _ -> true

      let is_whitespace = function
        | ' ' | '\n' | '\t' | '\r' ->
            true
        | _ ->
            false

      let ws = skip_while is_whitespace

      let header =
        let* num_replies =
          string "Got" *> space *> not_spaces
          <* space
          <* string "replies"
          <?> "unexpected header"
        in
        match num_replies with
        | "1" ->
            return ()
        | _ ->
            Printf.sprintf "got %s replies" num_replies |> fail

      (* example inputs: "key: value\n" or "key: value with spaces\r\n" *)
      let kvp =
        let* key = take_while (fun x -> x <> ':') <* char ':' in
        let* value =
          space *> take_while (function '\n' | '\r' -> false | _ -> true)
          <* (end_of_line <|> end_of_input)
        in
        return (key, value)

      let kvp_map =
        let* () = ws *> header <* ws in
        let* l = ws *> many kvp <* ws <* end_of_input in
        return (l |> List.to_seq |> Map.of_seq)

      let parse_kvp_map (x : string) : (string Map.t, string) result =
        parse_string ~consume:All kvp_map x
    end in
    let ldap fmt = fmt |> Printf.ksprintf @@ Printf.sprintf "ldap %s" in
    let* kvps = P.parse_kvp_map stdout <!> ldap "parsing failed '%s'" in
    let get_string k =
      match Map.find_opt k kvps with
      | None ->
          Error (ldap "missing key '%s'" k)
      | Some x ->
          Ok x
    in
    let get_int of_string k =
      let* str = get_string k in
      try Ok (of_string str)
      with _ -> Error (ldap "invalid value for key '%s'" k)
    in
    let* upn = get_string "userPrincipalName" in
    let* user_account_control = get_int Int32.of_string "userAccountControl" in
    let* account_expires = get_int Int64.of_string "accountExpires" in
    let account_expired =
      (* see https://docs.microsoft.com/en-us/windows/win32/adschema/a-accountexpires *)
      let windows_nt_time_to_unix_time x =
        Int64.sub (Int64.div x 10000000L) 11644473600L
      in
      match account_expires with
      | x when x = 0L || x = Int64.max_int ->
          false
      | i ->
          let expire_unix_time =
            windows_nt_time_to_unix_time i |> Int64.to_float
          in
          expire_unix_time < Unix.time ()
    in
    let open Int32 in
    (* see https://docs.microsoft.com/en-us/windows/win32/adschema/a-useraccountcontrol#remarks
     * for bit flag docs *)
    let disabled_bit = of_string "0x2" in
    let lockout_bit = of_string "0x10" in
    let passw_expire_bit = of_string "0x800000" in
    Ok
      {
        upn
      ; account_expired
      ; account_disabled= logand user_account_control disabled_bit <> 0l
      ; account_locked= logand user_account_control lockout_bit <> 0l
      ; password_expired= logand user_account_control passw_expire_bit <> 0l
      }

  let net_ads (sid : string) : (string, exn) result =
    try
      let args = ["ads"; "sid"; "-d"; debug_level (); "--machine-pass"; sid] in
      let stdout =
        Helpers.call_script ~log_output:On_failure !Xapi_globs.net_cmd args
      in
      Ok stdout
    with _ -> Error (generic_ex "net ads query failed")

  let query_user sid =
    let* stdout =
      try
        let args =
          ["ads"; "sid"; "-d"; debug_level (); "--machine-pass"; sid]
        in
        let stdout =
          Helpers.call_script ~log_output:On_failure !Xapi_globs.net_cmd args
        in
        Ok stdout
      with _ -> Error (generic_ex "ldap query failed")
    in
    parse_user stdout <!> generic_ex "%s"
end

module Wbinfo = struct
  let exception_of_stderr =
    let open Auth_signature in
    let regex = Re.Perl.(compile (re {|.*(WBC_ERR_[A-Z_]*).*|})) in
    let get_regex_match x =
      Option.bind (Re.exec_opt regex x) (fun g ->
          match Re.Group.all g with [|_; code|] -> Some code | _ -> None)
    in
    fun stderr ->
      get_regex_match stderr
      |> Option.map (fun code ->
             (* see wbclient.h samba source code for this error list *)
             match code with
             | "WBC_ERR_AUTH_ERROR" ->
                 Auth_failure code
             | "WBC_ERR_NOT_IMPLEMENTED"
             | "WBC_ERR_UNKNOWN_FAILURE"
             | "WBC_ERR_NO_MEMORY"
             | "WBC_ERR_INVALID_PARAM"
             | "WBC_ERR_WINBIND_NOT_AVAILABLE"
             | "WBC_ERR_DOMAIN_NOT_FOUND"
             | "WBC_ERR_INVALID_RESPONSE"
             | "WBC_ERR_NSS_ERROR"
             | "WBC_ERR_PWD_CHANGE_FAILED" ->
                 Auth_service_error (E_GENERIC, code)
             | "WBC_ERR_INVALID_SID"
             | "WBC_ERR_UNKNOWN_USER"
             | "WBC_ERR_UNKNOWN_GROUP" ->
                 Not_found
             | _ ->
                 Auth_service_error
                   (E_GENERIC, Printf.sprintf "unknown error code: %s" code))

  let call_wbinfo (args : string list) : (string, exn) result =
    let generic_err () =
      Error (generic_ex "'wbinfo %s' failed" (String.concat " " args))
    in
    (* we trust wbinfo will not print any sensitive info on failure *)
    try
      let stdout = Helpers.call_script ~log_output:On_failure wb_cmd args in
      Ok stdout
    with
    | Forkhelpers.Spawn_internal_error (stderr, _stdout, _status) -> (
      match exception_of_stderr stderr with
      | Some e ->
          Error e
      | None ->
          generic_err ()
    )
    | _ ->
        generic_err ()

  let parsing_ex args =
    generic_ex "parsing 'wbinfo %s' failed" (String.concat " " args)

  let can_resolve_krbtgt () =
    match call_wbinfo ["-n"; krbtgt] with Ok _ -> true | Error _ -> false

  let sid_of_name name =
    (* example:
     *
     * $ wbinfo -n user@domain.net
       S-1-2-34-... SID_USER (1)
     * $ wbinfo -n DOMAIN\user
       # similar output *)
    let args = ["--name-to-sid"; name] in
    let* stdout = call_wbinfo args in
    match String.split_on_char ' ' stdout with
    | sid :: _ ->
        Ok (String.trim sid)
    | [] ->
        Error (parsing_ex args)

  type name = User of string | Other of string

  let string_of_name = function User x -> x | Other x -> x

  let name_of_sid =
    (* example:
     * $ wbinfo -s S-1-5-21-3143668282-2591278241-912959342-502
       CONNAPP\krbtgt 1 *)
    (* the number returned after the name is the 'SID type' (grep for wbcSidType
     * in samba source code). for our purposes, it is sufficient to assume that
     * everything that is not a user is some 'other' type*)
    let regex = Re.Perl.(compile (re {|^([^\s].*)\ (\d+)\s*$|})) in
    let get_regex_match x =
      Option.bind (Re.exec_opt regex x) (fun g ->
          match Re.Group.all g with
          | [|_; name; "1"|] ->
              Some (User name)
          | [|_; name; _|] ->
              Some (Other name)
          | _ ->
              None)
    in
    fun sid ->
      let args = ["--sid-to-name"; sid] in
      let* stdout = call_wbinfo args in
      match get_regex_match stdout with
      | None ->
          Error (parsing_ex args)
      | Some x ->
          Ok x

  let gid_of_sid sid =
    let args = ["--sid-to-gid"; sid] in
    let* stdout = call_wbinfo args in
    try Ok (String.trim stdout |> int_of_string)
    with _ -> Error (parsing_ex args)

  let user_domgroups sid =
    (* example:
     *
     * $ wbinfo --user-domgroups S-1-2-34-...
       S-1-2-34-...
       S-1-5-21-...
       ... *)
    let args = ["--user-domgroups"; sid] in
    let* stdout = call_wbinfo args in
    Ok (String.split_on_char '\n' stdout |> List.map String.trim)

  let uid_of_sid sid =
    let args = ["--sid-to-uid"; sid] in
    let* stdout = call_wbinfo args in
    try Ok (String.trim stdout |> int_of_string)
    with _ -> Error (parsing_ex args)

  type uid_info = {user_name: string; uid: int; gid: int; gecos: string}
  [@@deriving rpcty]

  let string_of_uid_info x =
    Rpcmarshal.marshal uid_info.Rpc.Types.ty x |> Jsonrpc.to_string

  let parse_uid_info stdout =
    (* looks like one line from /etc/passwd: https://en.wikipedia.org/wiki/Passwd#Password_file *)
    match String.split_on_char ':' stdout with
    | [user_name; _passwd; uid; gid; gecos; _homedir; _shell] -> (
      try Ok {user_name; uid= int_of_string uid; gid= int_of_string gid; gecos}
      with _ -> Error ()
    )
    | _ ->
        Error ()

  let uid_info_of_uid (uid : int) =
    let args = ["--uid-info"; string_of_int uid] in
    let* stdout = call_wbinfo args in
    parse_uid_info stdout <!> fun () -> parsing_ex args
end

type domain_info = {
    service_name: string
  ; workgroup: string option
        (* For upgrade case, the legacy db does not contain workgroup *)
  ; netbios_name: string option
        (* Persist netbios_name to support hostname change *)
}

module Migrate_from_pbis = struct
  (* upgrade-pbis-to-winbind handles most of the migration from PBIS database
   * to winbind database
   * This module just migrate necessary information to set to winbind configuration *)
  let range s e step =
    let rec aux n acc = if n >= e then acc else aux (n + step) (n :: acc) in
    aux 0 [] |> List.rev

  let min_valid_pbis_value_length = String.length "X''"

  let extract_raw_value_from_pbis_db key =
    let sql =
      Printf.sprintf "select QUOTE(Value) from regvalues1 where ValueName='%s'"
        key
    in
    let db = Xapi_globs.pbis_db_path in
    let value =
      Helpers.call_script ~log_output:On_failure !Xapi_globs.sqlite3 [db; sql]
      |> String.trim
    in
    if String.length value <= min_valid_pbis_value_length then
      raise
        (Auth_service_error
           (E_GENERIC, Printf.sprintf "No value for %s in %s" key db))
    else
      value

  let from_single_group reg input =
    (* Extract value from single regular expression group
     * raise Not_found if not match *)
    let regex = Re.Perl.(compile (re reg)) in
    Re.exec regex input |> Re.Group.all |> function
    | [|_; v|] ->
        v
    | _ ->
        raise
          (Auth_service_error
             (E_GENERIC, Printf.sprintf "Failed to extract %s from %s" reg input))

  let parse_value_from_pbis raw_value =
    debug "parsing raw_value from pbis %s" raw_value ;
    let hex_len = 2 in
    (* Every hex value has two numbers *)
    (* raw value like X'58005200540055004B002D00300032002D003000330024000000' *)
    let stripped = from_single_group {|X'(.+)'$|} raw_value in
    (* stripped like 58005200540055004B002D00300032002D003000330024000000 *)
    range 0 (String.length stripped - hex_len) 4
    |> List.map (fun p -> String.sub stripped p hex_len)
    |> String.concat "" (* 585254554B2D30322D30332400 *)
    |> from_single_group {|(.+)00$|}
    (* 585254554B2D30322D303324 *)
    |> fun s ->
    Hex.to_string (`Hex s) (* XRTUK-02-03$ *) |> from_single_group {|(.+)\$$|}

  (* XRTUK-02-03$ *)

  let from_key ~key ~default =
    try extract_raw_value_from_pbis_db key |> parse_value_from_pbis
    with e ->
      debug "Failed to migrate %s, error %s, fallback to %s" key
        (ExnHelper.string_of_exn e)
        default ;
      default

  let migrate_netbios_name ~__context =
    (* Migrate netbios_name from PBIS db and persist to xapi db *)
    let self = Helpers.get_localhost ~__context in
    let default =
      Db.Host.get_hostname ~__context ~self |> String.uppercase_ascii
    in
    let netbios_name = from_key ~key:"SamAccountName" ~default in
    (* Persist migrated netbios_name *)
    let key = "netbios_name" in
    let value =
      Db.Host.get_external_auth_configuration ~__context ~self
      |> List.remove_assoc key
      |> fun v -> v @ [(key, netbios_name)]
    in
    Db.Host.set_external_auth_configuration ~__context ~self ~value ;
    debug "Migrated netbios_name %s from PBIS" netbios_name ;
    netbios_name
end

let get_domain_info_from_db () =
  (fun __context ->
    let host = Helpers.get_localhost ~__context in
    let service_name =
      Db.Host.get_external_auth_service_name ~__context ~self:host
    in
    let workgroup, netbios_name =
      Db.Host.get_external_auth_configuration ~__context ~self:host |> fun l ->
      (List.assoc_opt "workgroup" l, List.assoc_opt "netbios_name" l)
    in
    {service_name; workgroup; netbios_name})
  |> Server_helpers.exec_with_new_task
       "retrieving external auth domain workgroup"

let query_domain_workgroup ~domain ~db_workgroup =
  (* If workgroup found in pool database just use it, otherwise, query DC *)
  match db_workgroup with
  | Some wg ->
      wg
  | None -> (
      let key = "Pre-Win2k Domain" in
      let err_msg =
        Printf.sprintf "Failed to look up domain %s workgroup" domain
      in
      let hd msg = function
        | [] ->
            error "%s" msg ;
            raise (Auth_service_error (E_LOOKUP, msg))
        | h :: _ ->
            h
      in
      try
        let kdc =
          Helpers.call_script ~log_output:On_failure net_cmd
            ["lookup"; "kdc"; domain; "-d"; debug_level ()]
          (* Result like 10.71.212.25:88\n10.62.1.25:88\n*)
          |> String.split_on_char '\n'
          |> hd "lookup kdc return invalid result"
          |> String.split_on_char ':'
          |> hd "kdc has invalid address"
        in

        let lines =
          Helpers.call_script ~log_output:On_failure net_cmd
            ["ads"; "lookup"; "-S"; kdc; "-d"; debug_level ()]
        in
        match Xapi_cmd_result.of_output_opt ~sep:':' ~key ~lines with
        | Some v ->
            v
        | None ->
            raise (Auth_service_error (E_LOOKUP, err_msg))
      with _ -> raise (Auth_service_error (E_LOOKUP, err_msg))
    )

let config_winbind_damon ~domain ~workgroup ~netbios_name =
  let open Xapi_stdext_unix in
  let smb_config = "/etc/samba/smb.conf" in
  let conf_contents =
    String.concat "\n"
      [
        "# autogenerated by xapi"
      ; "[global]"
      ; "kerberos method = secrets and keytab"
      ; Printf.sprintf "realm = %s" domain
      ; "security = ADS"
      ; "template shell = /bin/bash"
      ; "winbind offline logon = Yes"
      ; "winbind refresh tickets = Yes"
      ; "winbind enum groups = no"
      ; "winbind enum users = no"
      ; "kerberos encryption types = strong"
      ; Printf.sprintf "workgroup = %s" workgroup
      ; Printf.sprintf "netbios name = %s" netbios_name
      ; "idmap config * : range = 3000000-3999999"
      ; Printf.sprintf "idmap config %s: backend = rid" domain
      ; Printf.sprintf "idmap config %s: range = 2000000-2999999" domain
      ; Printf.sprintf "log level = %s" (debug_level ())
      ; "idmap config * : backend = tdb"
      ; "" (* Empty line at the end *)
      ]
  in
  let len = String.length conf_contents in
  Unixext.atomic_write_to_file smb_config 0o0644 (fun fd ->
      let (_ : int) = Unix.single_write_substring fd conf_contents 0 len in
      Unix.fsync fd)

let from_config ~name ~err_msg ~config_params =
  match List.assoc_opt name config_params with
  | Some v ->
      v
  | _ ->
      raise (Auth_service_error (E_GENERIC, err_msg))

let all_number_re = Re.Perl.re {|^\d+$|} |> Re.Perl.compile

let get_localhost_name () =
  (fun __context ->
    Helpers.get_localhost ~__context |> fun host ->
    Db.Host.get_hostname ~__context ~self:host)
  |> Server_helpers.exec_with_new_task "retrieving hostname"

let assert_hostname_valid ~hostname =
  let all_numbers = Re.matches all_number_re hostname <> [] in
  if all_numbers then
    raise
      (Auth_service_error
         ( E_GENERIC
         , Printf.sprintf "hostname '%s' cannot contain only digits." hostname
         ))

let assert_domain_equal_service_name ~service_name ~config_params =
  (* For legeacy support, if domain exist in config_params, it must be equal to service_name *)
  let domain_key = "domain" in
  match List.assoc_opt domain_key config_params with
  | Some domain when domain <> service_name ->
      raise
        (Auth_service_error
           (E_GENERIC, "if present, config:domain must match service-name."))
  | _ ->
      ()

let extract_ou_config ~config_params =
  try
    let ou = from_config ~name:"ou" ~err_msg:"" ~config_params in
    ([("ou", ou)], [Printf.sprintf "createcomputer=%s" ou])
  with Auth_service_error _ -> ([], [])

let persist_extauth_config ~domain ~user ~ou_conf ~workgroup ~netbios_name =
  let value =
    match (domain, user) with
    | "", "" ->
        []
    | _ ->
        [
          ("domain", domain)
        ; ("user", user)
        ; ("workgroup", workgroup)
        ; ("netbios_name", netbios_name)
        ]
        @ ou_conf
  in
  (fun __context ->
    Helpers.get_localhost ~__context |> fun self ->
    Db.Host.set_external_auth_configuration ~__context ~self ~value ;
    Db.Host.get_name_label ~__context ~self
    |> debug "update external_auth_configuration for host %s")
  |> Server_helpers.exec_with_new_task "update external_auth_configuration"

let disable_machine_account ~service_name = function
  | Some u, Some p -> (
      (* Disable machine account in DC *)
      let env = [|Printf.sprintf "PASSWD=%s" p|] in
      let args =
        ["ads"; "leave"; "-U"; u; "--keep-account"; "-d"; debug_level ()]
      in
      try
        Helpers.call_script ~env net_cmd args |> ignore ;
        debug "Succeed to disable the machine account for domain %s"
          service_name
      with _ ->
        let msg =
          Printf.sprintf "Failed to disable the machine account for domain %s"
            service_name
        in
        debug "%s" msg ;
        raise (Auth_service_error (E_GENERIC, msg))
    )
  | _ ->
      debug
        "username or password not provided, skip cleaning the machine account"

(* Clean local resources like machine password *)
let clean_local_resources () : unit =
  let folder = "/var/lib/samba/private" in
  let secrets_tdb = Filename.concat folder "secrets.tdb" in
  try
    (* Erase secrets database before clean the files *)
    Helpers.call_script tdb_tool [secrets_tdb; "erase"] |> ignore ;
    (* Clean local resource files *)
    Helpers.FileSys.rmrf ~rm_top:false folder ;
    debug "Succeed to clean local winbind resources"
  with e ->
    let msg = "Failed to clean local samba resources" in
    error "%s : %s" msg (ExnHelper.string_of_exn e) ;
    raise (Auth_service_error (E_GENERIC, msg))

let domainify_uname ~domain uname =
  let open Astring.String in
  if
    is_infix ~affix:domain uname
    || is_infix ~affix:"@" uname
    || is_infix ~affix:{|\|} uname
    || uname = krbtgt
  then
    uname
  else
    Printf.sprintf "%s@%s" uname domain

module Winbind = struct
  let name = "winbind"

  let is_ad_enabled ~__context =
    ( Helpers.get_localhost ~__context |> fun self ->
      Db.Host.get_external_auth_type ~__context ~self )
    |> fun x -> x = Xapi_globs.auth_type_AD

  let start ~timeout ~wait_until_success =
    Xapi_systemctl.start ~timeout ~wait_until_success name

  let restart ~timeout ~wait_until_success =
    Xapi_systemctl.restart ~timeout ~wait_until_success name

  let stop ~timeout ~wait_until_success =
    Xapi_systemctl.stop ~timeout ~wait_until_success name

  let configure ~__context =
    (* Refresh winbind configuration to handle upgrade from PBIS
     * The winbind configuration needs to be refreshed before start winbind daemon *)
    let {service_name; workgroup; netbios_name} = get_domain_info_from_db () in
    if netbios_name = None then
      let netbios_name = Migrate_from_pbis.migrate_netbios_name __context in
      let workgroup =
        query_domain_workgroup ~domain:service_name ~db_workgroup:workgroup
      in
      config_winbind_damon ~domain:service_name ~workgroup ~netbios_name

  let init_service ~__context =
    if is_ad_enabled ~__context then (
      configure ~__context ;
      restart ~wait_until_success:false ~timeout:5.
    ) else
      debug "Skip starting %s as AD is not enabled" name

  let check_ready_to_serve ~timeout =
    (* we _need_ to use a username contained in our domain, otherwise the following tests won't work.
       Microsoft KB/Q243330 article provides the KRBTGT account as a well-known built-in SID in AD
       Microsoft KB/Q229909 article says that KRBTGT account cannot be renamed or enabled, making
       it the perfect target for such a test using a username (Administrator account can be renamed) *)
    try
      Helpers.retry_until_timeout ~timeout
        (Printf.sprintf "Checking if %s is ready" name)
        Wbinfo.can_resolve_krbtgt ;
      debug "Service %s is ready" name
    with e ->
      let msg =
        Printf.sprintf
          "%s is not ready after checking for %f seconds, error: %s" name
          timeout
          (ExnHelper.string_of_exn e)
      in
      error "Service not ready error: %s" msg ;
      raise (Auth_service_error (E_GENERIC, msg))

  let random_string len =
    let upper_char_start = 65 in
    let upper_char_len = 26 in
    String.init len (fun _ -> upper_char_start + Random.int upper_char_len |> char_of_int)

  let build_netbios_name hostname =
    (* Winbind follow https://docs.microsoft.com/en-US/troubleshoot/windows-server/identity/naming-conventions-for-computer-domain-site-ou#domain-names to limit netbios length to 15
     * Compress the hostname if exceed the length *)
    let max_length = 15 in
    if String.length hostname > max_length then
      (* format hostname to prefix-random each with 7 chars *)
      let len = 7 in
      let prefix = String.sub hostname 0 len in
      let suffix = random_string len in
      Printf.sprintf "%s-%s" prefix suffix
    else
      hostname
end

module AuthADWinbind : Auth_signature.AUTH_MODULE = struct
  let get_subject_identifier' subject_name =
    let subject_name =
      domainify_uname ~domain:(get_service_name ()) subject_name
    in
    Wbinfo.sid_of_name subject_name

  (* subject_id get_subject_identifier(string subject_name)

      Takes a subject_name (as may be entered into the XenCenter UI when defining subjects --
      see Access Control wiki page); and resolves it to a subject_id against the external
      auth/directory service.
      Raises Not_found (*Subject_cannot_be_resolved*) if authentication is not succesful.
  *)
  let get_subject_identifier subject_name =
    maybe_raise (get_subject_identifier' subject_name)

  (* subject_id Authenticate_username_password(string username, string password)

      Takes a username and password, and tries to authenticate against an already configured
      auth service (see XenAPI requirements Wiki page for details of how auth service configuration
      takes place and the appropriate values are stored within the XenServer Metadata).
      If authentication is successful then a subject_id is returned representing the account
      corresponding to the supplied credentials (where the subject_id is in a namespace managed by
      the auth module/service itself -- e.g. maybe a SID or something in the AD case).
      Raises auth_failure if authentication is not successful
  *)

  let authenticate_username_password uname password =
    (* the ntlm_auth binary expects the username to be in either SAM or UPN format.
     * we use wbinfo to try to convert the provided [uname] into said format.
     * as a last ditch attempt, we try to auth with the provided [uname]
     *
     * see CA-346287 for more information *)
    let orig_uname = uname in
    (let* sid =
       (* we change the exception, since otherwise we get an (incorrect) error
        * message saying that credentials are correct, but we are not authorized *)
       get_subject_identifier' uname <!> function
       | Auth_failure _ as e ->
           e
       | Auth_service_error (E_GENERIC, msg) ->
           Auth_failure msg
       | e ->
           D.error "authenticate_username_password:ex: %s"
             (Printexc.to_string e) ;
           Auth_failure
             (Printf.sprintf "couldn't get SID from username='%s'" uname)
     in
     let* () =
       match Wbinfo.name_of_sid sid >>| Wbinfo.string_of_name with
       | Error e ->
           D.warn
             "authenticate_username_password: trying original uname. ex: %s"
             (Printexc.to_string e) ;
           ntlm_auth orig_uname password
       | Ok uname ->
           ntlm_auth uname password
     in
     Ok sid)
    |> maybe_raise

  (* subject_id Authenticate_ticket(string ticket)

      As above but uses a ticket as credentials (i.e. for single sign-on)
  *)
  (* not implemented now, not needed for our tests, only for a *)
  (* future single sign-on feature *)
  let authenticate_ticket tgt =
    failwith "extauth_plugin authenticate_ticket not implemented"

  let query_subject_information_group (name : string) (gid : int) (sid : string)
      =
    [
      ("subject-name", name)
    ; ("subject-gid", string_of_int gid)
    ; ("subject-sid", sid)
    ; ("subject-is-group", string_of_bool true)
    ]

  let query_subject_information_user (name : string) (uid : int) (sid : string)
      =
    let* {gecos; gid} = Wbinfo.uid_info_of_uid uid in
    let* {
           upn
         ; account_disabled
         ; account_expired
         ; account_locked
         ; password_expired
         } =
      Ldap.query_user sid
    in
    Ok
      [
        ("subject-name", name)
      ; ("subject-gecos", gecos)
      ; ( "subject-displayname"
        , if gecos = "" || gecos = "<null>" then name else gecos )
      ; ("subject-uid", string_of_int uid)
      ; ("subject-gid", string_of_int gid)
      ; ("subject-upn", upn)
      ; ("subject-account-disabled", string_of_bool account_disabled)
      ; ("subject-account-locked", string_of_bool account_locked)
      ; ("subject-account-expired", string_of_bool account_expired)
      ; ("subject-password-expired", string_of_bool password_expired)
      ; ("subject-is-group", string_of_bool false)
      ]

  (* ((string*string) list) query_subject_information(string subject_identifier)

      Takes a subject_identifier and returns the user record from the directory service as
      key/value pairs. In the returned string*string map, there _must_ be a key called
      subject_name that refers to the name of the account (e.g. the user or group name as may
      be displayed in XenCenter). There is no other requirements to include fields from the user
      record -- initially qI'd imagine that we wouldn't bother adding anything else here, but
      it's a string*string list anyway for possible future expansion.
      Raises Not_found (*Subject_cannot_be_resolved*) if subject_id cannot be resolved by external auth service
  *)
  let query_subject_information (sid : string) =
    let res =
      let* name = Wbinfo.name_of_sid sid in
      match name with
      | User name ->
          let* uid = Wbinfo.uid_of_sid sid in
          query_subject_information_user name uid sid
      | Other name ->
          (* if the name doesn't correspond to a user then it ought to be a group *)
          let* gid = Wbinfo.gid_of_sid sid in
          Ok (query_subject_information_group name gid sid)
    in
    (* we must raise Not_found here. see xapi_pool.ml:revalidate_subjects *)
    maybe_raise_not_found res

  (* (string list) query_group_membership(string subject_identifier)

      Takes a subject_identifier and returns its group membership (i.e. a list of subject
      identifiers of the groups that the subject passed in belongs to). The set of groups returned
      _must_ be transitively closed wrt the is_member_of relation if the external directory service
      supports nested groups (as AD does for example)
  *)
  let query_group_membership subject_identifier =
    maybe_raise (Wbinfo.user_domgroups subject_identifier)

  (* unit on_enable(((string*string) list) config_params)

      Called internally by xapi _on each host_ when a client enables an external auth service for the
      pool via the XenAPI [see AD integration wiki page]. The config_params here are the ones passed
      by the client as part of the corresponding XenAPI call.
      On receiving this hook, the auth module should:
      (i) do whatever it needs to do (if anything) to register with the external auth/directory
          service [using the config params supplied to get access]
      (ii) Write the config_params that it needs to store persistently in the XenServer metadata
          into the Pool.external_auth_configuration field. [Note - the rationale for making the plugin
          write the config params it needs long-term into the XenServer metadata itself is so it can
          explicitly filter any one-time credentials [like AD username/password for example] that it
          does not need long-term.]
  *)
  let on_enable config_params =
    let user =
      from_config ~name:"user" ~err_msg:"enable requires username"
        ~config_params
    in
    let pass =
      from_config ~name:"pass" ~err_msg:"enable requires password"
        ~config_params
    in
    let netbios_name = get_localhost_name () |> Winbind.build_netbios_name in

    assert_hostname_valid ~hostname:netbios_name ;

    let {service_name; _} = get_domain_info_from_db () in
    assert_domain_equal_service_name ~service_name ~config_params ;

    let workgroup =
      (* Query new domain workgroup during join domain *)
      query_domain_workgroup ~domain:service_name ~db_workgroup:None
    in
    config_winbind_damon ~domain:service_name ~workgroup ~netbios_name ;

    let ou_conf, ou_param = extract_ou_config ~config_params in

    let args =
      [
        "ads"
      ; "join"
      ; service_name
      ; "-U"
      ; user
      ; "-n"
      ; netbios_name
      ; "-d"
      ; debug_level ()
      ; "--no-dns-updates"
      ]
      @ ou_param
    in
    debug "Joining domain %s with user %s netbios_name %s" service_name user
      netbios_name ;
    let env = [|Printf.sprintf "PASSWD=%s" pass|] in
    try
      Helpers.call_script ~env net_cmd args |> ignore ;
      Winbind.start ~timeout:5. ~wait_until_success:true ;
      Winbind.check_ready_to_serve ~timeout:300. ;
      persist_extauth_config ~domain:service_name ~user ~ou_conf ~workgroup
        ~netbios_name ;
      debug "Succeed to join domain %s" service_name
    with
    | Forkhelpers.Spawn_internal_error _ ->
        let msg = Printf.sprintf "Failed to join domain %s" service_name in
        error "Join domain error: %s" msg ;
        raise (Auth_service_error (E_GENERIC, msg))
    | Xapi_systemctl.Systemctl_fail _ ->
        let msg = Printf.sprintf "Failed to start %s" Winbind.name in
        error "Start daemon error: %s" msg ;
        raise (Auth_service_error (E_GENERIC, msg))
    | e ->
        let msg =
          Printf.sprintf
            "Failed to enable extauth for domain %s with user %s, error: %s"
            service_name user
            (ExnHelper.string_of_exn e)
        in
        error "Enable extauth error: %s" msg ;
        raise (Auth_service_error (E_GENERIC, msg))

  (* unit on_disable()

      Called internally by xapi _on each host_ when a client disables an auth service via the XenAPI.
      The hook will be called _before_ the Pool configuration fields relating to the external-auth
      service are cleared (i.e. so you can access the config params you need from the pool metadata
      within the body of the on_disable method)
  *)
  let on_disable config_params =
    let user = List.assoc_opt "user" config_params in
    let pass = List.assoc_opt "pass" config_params in
    let {service_name; _} = get_domain_info_from_db () in
    disable_machine_account ~service_name (user, pass) ;
    (* Clean local resources *)
    clean_local_resources () ;
    (* Clean extauth config *)
    persist_extauth_config ~domain:"" ~user:"" ~ou_conf:[] ~workgroup:""
      ~netbios_name:"" ;
    debug "Succeed to disable external auth for %s" service_name

  (* unit on_xapi_initialize(bool system_boot)

      Called internally by xapi whenever it starts up. The system_boot flag is true iff xapi is
      starting for the first time after a host boot
  *)
  let on_xapi_initialize _system_boot =
    Winbind.start ~timeout:5. ~wait_until_success:true ;
    Winbind.check_ready_to_serve ~timeout:300.

  (* unit on_xapi_exit()

      Called internally when xapi is doing a clean exit.
  *)
  let on_xapi_exit () = ()

  (* Implement the single value required for the module signature *)
  let methods =
    {
      Auth_signature.authenticate_username_password
    ; Auth_signature.authenticate_ticket
    ; Auth_signature.get_subject_identifier
    ; Auth_signature.query_subject_information
    ; Auth_signature.query_group_membership
    ; Auth_signature.on_enable
    ; Auth_signature.on_disable
    ; Auth_signature.on_xapi_initialize
    ; Auth_signature.on_xapi_exit
    }
end
