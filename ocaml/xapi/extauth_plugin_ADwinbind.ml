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
module Scheduler = Xapi_stdext_threads_scheduler.Scheduler

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let krbtgt = "KRBTGT"

let ( let* ) = Result.bind

let ( let@ ) = ( @@ )

let ( <!> ) x f = Rresult.R.reword_error f x

let ( >>| ) = Rresult.( >>| )

let max_debug_level = 10

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

let domain_krb5_dir = Filename.concat Xapi_globs.samba_dir "lock/smb_krb5"

let debug_level () = !Xapi_globs.winbind_debug_level |> string_of_int

let err_msg_to_tag_map =
  [
    ("not a properly formed account name", Auth_signature.E_INVALID_ACCOUNT)
  ; ("bad username or authentication", Auth_signature.E_CREDENTIALS)
    (* Some other errors *)
  ]

type domain_info = {
    service_name: string
  ; workgroup: string option
        (* For upgrade case, the legacy db does not contain workgroup *)
  ; netbios_name: string option
        (* Persist netbios_name to support hostname change *)
  ; machine_pwd_last_change_time: float option
}

let generic_error msg =
  error "%s" msg ;
  raise (Auth_service_error (E_GENERIC, msg))

let fail fmt = Printf.ksprintf generic_error fmt

let is_samba_updated =
  (* This is temporary workaround to be compatible for old and new samba, to decouple the merge of xapi and samba *)
  let check_file = "/usr/lib64/samba/libxattr-tdb-private-samba.so" in
  Sys.file_exists check_file

let kerberos_opt =
  match is_samba_updated with true -> [] | false -> ["--kerberos"]

(** Kerberos Domain Controller. The current implementation does not
    work with non-standard ports *)
module KDC : sig
  type t

  val server : t -> string
  (** IP address *)

  val _port : t -> int
  (** port number *)

  val from_lookup : string -> t
  (** parses net(1) command output format *)

  val from_db : string -> t
  (** parse xapi DB entry *)

  val to_db : t -> string
  (** create xapi DB entry *)

  val to_msg : t -> string
  (** format for logging *)
end = struct
  type t = {ip: Ipaddr.t  (** IPv4/v6 of domain controller *); port: int}

  let default_port = 88

  (** the custom URI scheme we use to store the KDC in the xapi database *)
  let scheme = "kdc"

  let server t = Ipaddr.to_string t.ip

  (** currently not used by client code *)
  let _port t = t.port

  let from_lookup str =
    (* examples for IPv4 str returned by "net lookup kdc":
       10.71.212.25:88 10.62.1.25:88. Based on experiments I believe
       this is also true for IPv6 although the colon is used inside an
       IPv6 address. So we split off the last number as port *)
    match Astring.String.cut ~rev:true ~sep:":" str with
    | Some (ip, "88") -> (
      try {ip= Ipaddr.of_string ip |> Result.get_ok; port= default_port}
      with _ -> fail "%s: can't parse %s as address:port" __FUNCTION__ str
    )
    | Some (ip, port) ->
        fail "%s: KDC %s uses non-default port %s" __FUNCTION__ ip port
    | None ->
        fail "%s: can't parse %s as address:port" __FUNCTION__ str

  (** Read IP from XAPI. The legacy format is just the IP address; the
     new format uses a URI, which includes the port *)
  let from_db str =
    let ip host = Ipaddr.of_string host |> Result.get_ok in
    try
      let uri = Uri.of_string str in
      match Uri.(scheme uri, host uri, port uri) with
      | Some s, Some host, Some port when s = scheme ->
          {ip= ip host; port}
      | _ ->
          (* we try to parse the legacy format *)
          {ip= ip str; port= default_port}
    with _ -> fail "%s: can't parse %s" __FUNCTION__ str

  (** store host and port as a custom URI *)
  let to_db t =
    Uri.make ~scheme ~host:(Ipaddr.to_string t.ip) ~port:t.port ()
    |> Uri.to_string

  (** this format is only used for logging *)
  let to_msg t = Printf.sprintf "%s (port %d)" (Ipaddr.to_string t.ip) t.port
end

let hd msg = function [] -> generic_error msg | h :: _ -> h

let max_netbios_name_length = 15

let tag_from_err_msg msg =
  match
    List.find_opt
      (fun (k, _) -> Astring.String.is_infix ~affix:k msg)
      err_msg_to_tag_map
  with
  | Some (_, v) ->
      v
  | None ->
      Auth_signature.E_GENERIC

let get_domain_info_from_db () =
  Server_helpers.exec_with_new_task "retrieving external auth domain workgroup"
  @@ fun __context ->
  let host = Helpers.get_localhost ~__context in
  let service_name =
    Db.Host.get_external_auth_service_name ~__context ~self:host
  in
  let workgroup, netbios_name, machine_pwd_last_change_time =
    Db.Host.get_external_auth_configuration ~__context ~self:host
    |> fun config ->
    ( List.assoc_opt "workgroup" config
    , List.assoc_opt "netbios_name" config
    , List.assoc_opt "machine_pwd_last_change_time" config
      |> Option.map (fun s -> float_of_string s)
    )
  in
  {service_name; workgroup; netbios_name; machine_pwd_last_change_time}

let update_extauth_configuration ~__context ~k ~v =
  let self = Helpers.get_localhost ~__context in
  Db.Host.get_external_auth_configuration ~__context ~self |> fun value ->
  (k, v) :: List.remove_assoc k value |> fun value ->
  Db.Host.set_external_auth_configuration ~__context ~self ~value

module Ldap = struct
  module Escape = struct
    (*
     * Escape characters according to
     * https://docs.microsoft.com/en-gb/windows/win32/adsi/search-filter-syntax?redirectedfrom=MSDN#special-characters
     *)

    let reg_star = {|*|} |> Re.str |> Re.compile

    let reg_left_bracket = {|(|} |> Re.str |> Re.compile

    let reg_right_bracket = {|)|} |> Re.str |> Re.compile

    let reg_backward_slash = {|\|} |> Re.str |> Re.compile

    let reg_null = "\000" |> Re.str |> Re.compile

    let reg_slash = {|/|} |> Re.str |> Re.compile

    let escape_map =
      [
        (* backward slash goes first as others will include backward slash*)
        (reg_backward_slash, {|\5d|})
      ; (reg_star, {|\2a|})
      ; (reg_left_bracket, {|\28|})
      ; (reg_right_bracket, {|\29|})
      ; (reg_null, {|\00|})
      ; (reg_slash, {|\2f|})
      ]

    let escape str =
      List.fold_left
        (fun acc element ->
          let reg = fst element in
          let value = snd element in
          Re.replace_string reg ~by:value acc
        )
        str escape_map
  end

  let escape str = Escape.escape str

  type user = {
      name: string
    ; display_name: string
    ; upn: string
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
    let get_string key =
      match Map.find_opt key kvps with
      | None ->
          Error (ldap "missing key '%s'" key)
      | Some x ->
          Ok x
    in
    let get_string_with_default ~key ~default =
      match get_string key with Ok x -> Ok x | Error _ -> Ok default
    in
    let get of_string key =
      let* str = get_string key in
      try Ok (of_string str)
      with _ -> Error (ldap "invalid value for key '%s'" key)
    in
    let get_with_default of_string ~key ~default =
      match get of_string key with Ok x -> Ok x | Error _ -> Ok default
    in
    let windows_nt_time_to_unix_time x =
      Int64.sub (Int64.div x 10000000L) 11644473600L
    in
    let default = "" in
    let* name = get_string_with_default ~key:"name" ~default in
    let* upn = get_string_with_default ~key:"userPrincipalName" ~default in
    let* display_name = get_string_with_default ~key:"displayName" ~default in
    let* user_account_control = get Int32.of_string "userAccountControl" in
    let* account_expires = get Int64.of_string "accountExpires" in
    let* password_expires_computed =
      get_with_default Int64.of_string
        ~key:"msDS-UserPasswordExpiryTimeComputed" ~default:Int64.max_int
    in
    (* see https://docs.microsoft.com/en-us/windows/win32/adschema/a-lockouttime *)
    let* lockout_time =
      get_with_default Int64.of_string ~key:"lockoutTime" ~default:0L
    in
    let is_expired zero_expired = function
      | 0L ->
          zero_expired
      | i when i = Int64.max_int ->
          false
      | _ as t ->
          let expire_unix_time =
            windows_nt_time_to_unix_time t |> Int64.to_float
          in
          expire_unix_time < Unix.time ()
    in

    let open Int32 in
    (* see https://docs.microsoft.com/en-us/windows/win32/adschema/a-useraccountcontrol#remarks
     * for bit flag docs *)
    let disabled_bit = of_string "0x2" in
    Ok
      {
        name
      ; display_name
      ; upn
        (* see https://docs.microsoft.com/en-us/windows/win32/adschema/a-accountexpires *)
      ; account_expired= is_expired false account_expires
      ; account_disabled= logand user_account_control disabled_bit <> 0l
      ; account_locked=
          lockout_time <> 0L
          (* see https://docs.microsoft.com/en-us/openspecs/windows_protocols/ms-adts/f9e9b7e2-c7ac-4db6-ba38-71d9696981e9 *)
      ; password_expired= is_expired true password_expires_computed
      }

  let krb5_conf_path ~domain_netbios =
    Filename.concat domain_krb5_dir
      (Printf.sprintf "krb5.conf.%s" domain_netbios)

  let env_of_krb5 domain_netbios =
    let domain_krb5_cfg = krb5_conf_path ~domain_netbios in
    [|Printf.sprintf "KRB5_CONFIG=%s" domain_krb5_cfg|]

  let query_user ?(log_output = Helpers.On_failure) ?timeout sid domain_netbios
      kdc =
    let env = env_of_krb5 domain_netbios in
    (* msDS-UserPasswordExpiryTimeComputed not in the default attrs list, define it explictly here *)
    let attrs =
      [
        "name"
      ; "userPrincipalName"
      ; "displayName"
      ; "userAccountControl"
      ; "accountExpires"
      ; "msDS-UserPasswordExpiryTimeComputed"
      ; "lockoutTime"
      ]
    in
    let* stdout =
      try
        (* Query KDC instead of use domain here
         * Just in case cannot resolve domain name from DNS *)
        let args =
          [
            "ads"
          ; "sid"
          ; sid
          ; "-d"
          ; debug_level ()
          ; "--server"
          ; KDC.server kdc
          ; "--machine-pass"
          ]
          @ kerberos_opt
          @ attrs
        in
        let stdout =
          Helpers.call_script ~env ~log_output !Xapi_globs.net_cmd ?timeout args
        in
        Ok stdout
      with _ -> Error (generic_ex "ldap query user info from sid failed")
    in
    parse_user stdout <!> generic_ex "%s"

  let query_sid ~name ~kdc ~domain_netbios =
    let key = "objectSid" in
    let env = env_of_krb5 domain_netbios in
    let name = escape name in
    (* Escape name to avoid injection detection *)
    let query = Printf.sprintf "(|(sAMAccountName=%s)(name=%s))" name name in
    let args =
      [
        "ads"
      ; "search"
      ; "-d"
      ; debug_level ()
      ; "--server"
      ; KDC.server kdc
      ; "--machine-pass"
      ]
      @ kerberos_opt
      @ [query; key]
    in
    try
      Helpers.call_script ~env !Xapi_globs.net_cmd args
      |> Xapi_cmd_result.of_output ~sep:':' ~key
      |> fun x -> Ok x
    with
    | Forkhelpers.Spawn_internal_error (_, stdout, _) ->
        Error (generic_ex "Ldap query sid failed: %s" stdout)
    | Not_found ->
        Error (generic_ex "%s not found in ldap result" key)
    | _ ->
        Error (generic_ex "Failed to lookup sid from username %s" name)
end

type domain_name_type = Name | NetbiosName

module Wbinfo = struct
  let exception_of_stderr =
    let open Auth_signature in
    let regex = Re.Perl.(compile (re {|.*(WBC_ERR_[A-Z_]*).*|})) in
    let get_regex_match x =
      Option.bind (Re.exec_opt regex x) (fun g ->
          match Re.Group.all g with [|_; code|] -> Some code | _ -> None
      )
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
                   (E_GENERIC, Printf.sprintf "unknown error code: %s" code)
         )

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

  let kerberos_auth uname passwd : (unit, exn) result =
    try
      let args = ["--krb5auth"; uname; "--krb5ccname"; "/dev/null"] in
      let _stdout =
        Helpers.call_script ~log_output:On_failure ~stdin:passwd wb_cmd args
      in
      Ok ()
    with _ -> Error (auth_ex uname)

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

  let domain_name_of ~target_name_type ~from_name =
    (*
     * The domain name can be a normal domain name or netbios name
     * Query the target domain name from one provided name
     *
     * example output:
     * Name              : UCC
     * Alt_Name          : ucc.local
     * SID               : S-1-5-21-2850064427-2368465266-4270348630
     *)
    let args = ["--domain-info"; from_name] in
    let* stdout = call_wbinfo args in
    let key =
      match target_name_type with Name -> "Alt_Name" | NetbiosName -> "Name"
    in
    try Ok (Xapi_cmd_result.of_output ~sep:':' ~key stdout)
    with _ -> Error (parsing_ex args)

  let is_domain_netbios_valid domain_netbios =
    let min_valid_domain_length = 1 in
    match domain_name_of ~target_name_type:Name ~from_name:domain_netbios with
    | Ok name ->
        String.length name >= min_valid_domain_length
    | Error _ ->
        false

  let domain_of_uname uname =
    match String.split_on_char '\\' uname with
    | [domain_netbios; _] ->
        let* domain_name =
          domain_name_of ~target_name_type:Name ~from_name:domain_netbios
        in
        Ok (domain_netbios, domain_name)
    | _ ->
        Error (generic_ex "Invalid domain user name %s" uname)

  let domain_and_user_of_uname uname =
    let open Astring.String in
    match String.split_on_char '\\' uname with
    | [netbios; user] ->
        let* domain =
          domain_name_of ~target_name_type:Name ~from_name:netbios
        in
        Ok (domain, user)
    | _ -> (
      match String.split_on_char '@' uname with
      | [user; domain] ->
          Ok (domain, user)
      | _ ->
          if is_infix ~affix:"@" uname || is_infix ~affix:{|\|} uname then
            Error (generic_ex "Invalid domain user name %s" uname)
          else
            Ok ((get_domain_info_from_db ()).service_name, uname)
    )

  let all_domain_netbios () =
    (*
     * List all domains (trusted and own domain)
     * wbinfo --all-domains
     *
     * Return netbios_names_of_domains
     *
     * example output:
     * BUILTIN
     * ABCDEFG-FGVVPQL
     * UCC
     * UDD
     * CHILD1
     * GRANDCHILD
     * UDDCHILD1
     *)
    let args = ["--all-domains"] in
    let* stdout = call_wbinfo args in
    Ok
      (String.split_on_char '\n' stdout
      |> List.filter (fun x -> is_domain_netbios_valid x)
      )

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
              None
      )
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
    | user_name :: _passwd :: uid :: gid :: rest -> (
        (* We expect at least homedir and shell at the end *)
        let rest = List.rev rest in
        match rest with
        | _shell :: _homedir :: tail -> (
            (* Rev it back to original order *)
            let tail = List.rev tail in
            let gecos = String.concat ":" tail in
            try
              Ok
                {
                  user_name
                ; uid= int_of_string uid
                ; gid= int_of_string gid
                ; gecos
                }
            with _ -> Error ()
          )
        | _ ->
            debug "%s uid_info format error: %s" __FUNCTION__ stdout ;
            Error ()
      )
    | _ ->
        debug "%s uid_info format error: %s" __FUNCTION__ stdout ;
        Error ()

  let uid_info_of_uid (uid : int) =
    let args = ["--uid-info"; string_of_int uid] in
    let* stdout = call_wbinfo args in
    parse_uid_info stdout <!> fun () -> parsing_ex args
end

module Migrate_from_pbis = struct
  (* upgrade-pbis-to-winbind handles most of the migration from PBIS database
   * to winbind database
   * This module just migrate necessary information to set to winbind configuration *)
  let range _ e step =
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
      raise (generic_ex "No value for %s in %s" key db)
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
        raise (generic_ex "Failed to extract %s from %s" reg input)

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
    update_extauth_configuration ~__context ~k:"netbios_name" ~v:netbios_name ;
    debug "Migrated netbios_name %s from PBIS" netbios_name ;
    netbios_name
end

let kdcs_of_domain domain =
  try
    Helpers.call_script ~log_output:On_failure net_cmd
      (["lookup"; "kdc"; domain; "-d"; debug_level ()] @ kerberos_opt)
    (* Result like 10.71.212.25:88\n10.62.1.25:88\n*)
    |> String.split_on_char '\n'
    |> List.filter (fun x -> String.trim x <> "") (* Remove empty lines *)
    |> List.map KDC.from_lookup
  with _ -> fail "%s: failed to lookup kdcs of domain %s" __FUNCTION__ domain

let workgroup_from_server kdc =
  let err_msg =
    Printf.sprintf "Failed to lookup workgroup from server %s" (KDC.server kdc)
  in
  let key = "Pre-Win2k Domain" in
  try
    Helpers.call_script ~log_output:On_failure net_cmd
      (["ads"; "lookup"; "-S"; KDC.server kdc; "-d"; debug_level ()]
      @ kerberos_opt
      )
    |> Xapi_cmd_result.of_output ~sep:':' ~key
    |> Result.ok
  with _ ->
    debug "Unable to query info from kdc %s, probably is broken down"
      (KDC.to_msg kdc) ;
    Error (Auth_service_error (E_LOOKUP, err_msg))

let kdc_of_domain domain =
  try
    kdcs_of_domain domain
    (* Does not trust DNS as it may cache some invalid kdcs, CA-360951 *)
    |> List.find (fun kdc -> workgroup_from_server kdc |> Result.is_ok)
  with Not_found ->
    raise (generic_ex "No valid kdc found for domain %s" domain)

let query_domain_workgroup ~domain =
  let err_msg = Printf.sprintf "Failed to look up domain %s workgroup" domain in
  try
    let kdc = kdc_of_domain domain in
    workgroup_from_server kdc |> Result.get_ok
  with _ -> raise (Auth_service_error (E_LOOKUP, err_msg))

let config_winbind_daemon ~workgroup ~netbios_name ~domain =
  let smb_config = "/etc/samba/smb.conf" in
  let allow_fallback =
    (*`allow kerberos auth fallback` depends on our internal samba patch,
     * this patch disable fallback to ntlm by default and can be enabled
     * Looks like upstream is doing something similar on master with
     * configuration `weak_crypto`, check and replace the internal patch when
     * upgrade to samba packages with this capacity *)
    if !Xapi_globs.winbind_allow_kerberos_auth_fallback then "yes" else "no"
  in
  let version_conf =
    match is_samba_updated with
    | false ->
        [Printf.sprintf "allow kerberos auth fallback = %s" allow_fallback]
    | true ->
        [
          "client use kerberos = required"
        ; "sync machine password to keytab = \
           /etc/krb5.keytab:account_name:sync_etypes:sync_kvno:machine_password"
        ]
  in
  ( match (workgroup, netbios_name, domain) with
  | Some wkgroup, Some netbios, Some dom ->
      ["# autogenerated by xapi"; "[global]"]
      @ version_conf
      @ [
          "kerberos method = secrets and keytab"
        ; Printf.sprintf "realm = %s" dom
        ; "security = ADS"
        ; "template shell = /bin/bash"
        ; "winbind refresh tickets = yes"
        ; "winbind enum groups = no"
        ; "winbind enum users = no"
        ; "winbind scan trusted domains = yes"
        ; "winbind use krb5 enterprise principals = yes"
        ; Printf.sprintf "winbind cache time = %d"
            !Xapi_globs.winbind_cache_time
        ; Printf.sprintf "machine password timeout = 0"
        ; Printf.sprintf "kerberos encryption types = %s"
            (Kerberos_encryption_types.Winbind.to_string
               !Xapi_globs.winbind_kerberos_encryption_type
            )
        ; Printf.sprintf "workgroup = %s" wkgroup
        ; Printf.sprintf "netbios name = %s" netbios
        ; "idmap config * : range = 3000000-3999999"
        ; Printf.sprintf "idmap config %s: backend = rid" dom
        ; Printf.sprintf "idmap config %s: range = 2000000-2999999" dom
        ; Printf.sprintf "log level = %s" (debug_level ())
        ; "idmap config * : backend = tdb"
        ; "" (* Empty line at the end *)
        ]
  | _ ->
      ["# autogenerated by xapi"; "[global]"; "" (* Empty line at the end *)]
  )
  |> String.concat "\n"
  |> Xapi_stdext_unix.Unixext.write_string_to_file smb_config

let clear_winbind_config () =
  (* Keep the winbind configuration if xapi config file specified explictly,
   * The winbind configure file is useful for debug *)
  if !Xapi_globs.winbind_keep_configuration then
    ()
  else
    config_winbind_daemon ~workgroup:None ~netbios_name:None ~domain:None

let from_config ~name ~err_msg ~config_params =
  match List.assoc_opt name config_params with
  | Some v ->
      v
  | _ ->
      raise (Auth_service_error (E_GENERIC, err_msg))

let all_number_re = Re.Perl.re {|^\d+$|} |> Re.Perl.compile

let get_localhost_name () =
  Server_helpers.exec_with_new_task "retrieving hostname" @@ fun __context ->
  Helpers.get_localhost ~__context |> fun host ->
  Db.Host.get_hostname ~__context ~self:host

let assert_hostname_valid ~hostname =
  let all_numbers = Re.matches all_number_re hostname <> [] in
  if all_numbers then
    raise (generic_ex "hostname '%s' cannot contain only digits." hostname)

let assert_domain_equal_service_name ~service_name ~config_params =
  (* For legeacy support, if domain exist in config_params, it must be equal to service_name *)
  let domain_key = "domain" in
  match List.assoc_opt domain_key config_params with
  | Some domain when domain <> service_name ->
      raise (generic_ex "if present, config:domain must match service-name.")
  | _ ->
      ()

let extract_ou_config ~config_params =
  try
    let ou = from_config ~name:"ou" ~err_msg:"" ~config_params in
    ([("ou", ou)], [Printf.sprintf "createcomputer=%s" ou])
  with Auth_service_error _ -> ([], [])

let persist_extauth_config ~domain ~user ~ou_conf ~workgroup ~netbios_name
    ~machine_pwd_last_change_time =
  let value =
    match
      (domain, user, workgroup, netbios_name, machine_pwd_last_change_time)
    with
    | Some dom, Some u, Some wkg, Some netbios, Some pwd_time ->
        [
          ("domain", dom)
        ; ("user", u)
        ; ("workgroup", wkg)
        ; ("netbios_name", netbios)
        ; ("machine_pwd_last_change_time", pwd_time)
        ]
        @ ou_conf
    | _ ->
        []
  in
  Server_helpers.exec_with_new_task "update external_auth_configuration"
  @@ fun __context ->
  Helpers.get_localhost ~__context |> fun self ->
  Db.Host.set_external_auth_configuration ~__context ~self ~value ;
  Db.Host.get_name_label ~__context ~self
  |> debug "update external_auth_configuration for host %s"

let clear_machine_account ~service_name = function
  | Some u, Some p -> (
      (* Disable machine account in DC *)
      let env = [|Printf.sprintf "PASSWD=%s" p|] in
      let args =
        ["ads"; "leave"; "-U"; u; "-d"; debug_level ()] @ kerberos_opt
      in
      try
        Helpers.call_script ~env net_cmd args |> ignore ;
        debug "Succeed to clear the machine account for domain %s" service_name
      with _ ->
        let msg =
          Printf.sprintf "Failed to clear the machine account for domain %s"
            service_name
        in
        debug "%s" msg ;
        raise (Auth_service_error (E_GENERIC, msg))
    )
  | _ ->
      debug
        "username or password not provided, skip clearing the machine account"

(* Clean local resources like machine password *)
let clear_local_resources () : unit =
  let folder = "/var/lib/samba/private" in
  let secrets_tdb = Filename.concat folder "secrets.tdb" in
  try
    (* Erase secrets database before clear the files *)
    Helpers.call_script tdb_tool [secrets_tdb; "erase"] |> ignore ;
    (* Clean local resource files *)
    Xapi_stdext_unix.Unixext.rm_rec ~rm_top:false folder ;
    debug "Succeed to clear local winbind resources"
  with e ->
    let msg = "Failed to clear local samba resources" in
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
      Db.Host.get_external_auth_type ~__context ~self
    )
    |> fun x -> x = Xapi_globs.auth_type_AD

  let update_workgroup ~__context ~workgroup =
    update_extauth_configuration ~__context ~k:"workgroup" ~v:workgroup

  let start ~timeout ~wait_until_success =
    Xapi_systemctl.start ~timeout ~wait_until_success name

  let restart ~timeout ~wait_until_success =
    Xapi_systemctl.restart ~timeout ~wait_until_success name

  let stop ~timeout ~wait_until_success =
    Xapi_systemctl.stop ~timeout ~wait_until_success name

  let configure ~__context =
    (* Refresh winbind configuration to handle upgrade from PBIS
     * The winbind configuration needs to be refreshed before start winbind daemon *)
    let {service_name; workgroup; netbios_name; _} =
      get_domain_info_from_db ()
    in
    let netbios_name =
      match netbios_name with
      | None ->
          Migrate_from_pbis.migrate_netbios_name ~__context
      | Some name ->
          name
    in
    let workgroup =
      match workgroup with
      | None ->
          let workgroup = query_domain_workgroup ~domain:service_name in
          (* Persist the workgroup to avoid lookup again on next startup *)
          update_workgroup ~__context ~workgroup ;
          workgroup
      | Some workgroup ->
          workgroup
    in
    config_winbind_daemon ~domain:(Some service_name)
      ~workgroup:(Some workgroup) ~netbios_name:(Some netbios_name)

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
    let upper_char_start = Char.code 'A' in
    let upper_char_len = 26 in
    let random_char () =
      upper_char_start + Random.int upper_char_len |> char_of_int
    in
    String.init len (fun _ -> random_char ())

  let build_netbios_name localhost_name =
    (* Winbind follow https://docs.microsoft.com/en-US/troubleshoot/windows-server/identity/naming-conventions-for-computer-domain-site-ou#domain-names to limit netbios length to 15
     * Compress the hostname if exceed the length *)

    (* The localhost_name may be FQDN, need to extract hostname from it, see XSI-1407
     * hostname always be the first part of FQDN *)
    let hostname =
      Domain_name.of_string localhost_name
      |> Result.map (fun x -> Domain_name.get_label x 0)
      |> Result.join
      |> function
      | Error _ ->
          raise
            (generic_ex "Failed to extract hostname from FQDN %s" localhost_name)
      | Ok x ->
          x
    in
    if String.length hostname > max_netbios_name_length then (
      (* format hostname to prefix-random each with 7 chars *)
      let len = 7 in
      let prefix = String.sub hostname 0 len in
      let suffix = random_string len in
      let netbios_name = Printf.sprintf "%s-%s" prefix suffix in
      info "hostname exceeds allowed length, using '%s' instead" netbios_name ;
      netbios_name
    ) else
      hostname

  let set_machine_account_encryption_type netbios_name =
    match !Xapi_globs.winbind_set_machine_account_kerberos_encryption_type with
    | true -> (
        let args =
          [
            "ads"
          ; "enctypes"
          ; "set"
          ; "--machine-pass"
          ; "-d"
          ; debug_level ()
          ; Printf.sprintf "%s$" netbios_name
          ; Printf.sprintf "%d"
              (Kerberos_encryption_types.Winbind.to_encoding
                 !Xapi_globs.winbind_kerberos_encryption_type
              )
          ]
        in
        try
          Helpers.call_script
            ~timeout:Mtime.Span.(5 * s)
            !Xapi_globs.net_cmd args
          |> ignore
        with _ ->
          warn "Failed to set machine account encryption type, ignoring"
      )
    | false ->
        debug "Skip setting machine account encryption type to DC"
end

module ClosestKdc = struct
  let periodic_update_task_name = "Update closest kdc"

  let startup_delay = 5.

  let mtime_this ~name ~f =
    let start = Mtime_clock.counter () in
    match f () with
    | Ok _ ->
        Ok (name, Mtime_clock.count start)
    | Error e ->
        Error e

  let update_db ~domain ~kdc =
    Server_helpers.exec_with_new_task "update domain closest kdc"
    @@ fun __context ->
    update_extauth_configuration ~__context ~k:domain ~v:(KDC.to_db kdc)

  let from_db domain =
    Server_helpers.exec_with_new_task "query domain closest kdc"
    @@ fun __context ->
    let self = Helpers.get_localhost ~__context in
    Db.Host.get_external_auth_configuration ~__context ~self
    |> List.assoc_opt domain
    |> Option.map KDC.from_db

  let lookup domain =
    try
      let open Helpers in
      let* krbtgt_sid =
        Wbinfo.sid_of_name (Printf.sprintf "%s@%s" krbtgt domain)
      in
      let* domain_netbios_name =
        Wbinfo.domain_name_of ~target_name_type:NetbiosName ~from_name:domain
      in
      let log_output =
        if !Xapi_globs.winbind_debug_level >= max_debug_level then
          On_failure
        else
          Never
      in
      kdcs_of_domain domain
      |> List.map (fun kdc ->
             debug "Got domain '%s' kdc '%s'" domain (KDC.to_msg kdc) ;
             kdc
         )
      |> List.map (fun kdc ->
             mtime_this ~name:kdc ~f:(fun () ->
                 Ldap.query_user ~log_output krbtgt_sid domain_netbios_name kdc
             )
         )
      |> List.filter_map Result.to_option
      |> List.sort (fun (_, s1) (_, s2) -> Mtime.Span.compare s1 s2)
      |> hd (Printf.sprintf "domain %s does not has valid kdc" domain)
      |> fun x -> Ok (domain, fst x)
    with e ->
      debug "Failed to lookup domain %s closest kdc" domain ;
      Error e

  let update () =
    try
      Wbinfo.all_domain_netbios ()
      |> maybe_raise
      |> List.map (fun netbios ->
             Wbinfo.domain_name_of ~target_name_type:Name ~from_name:netbios
         )
      |> List.filter_map Result.to_option
      |> List.map (fun domain -> lookup domain)
      |> List.filter_map Result.to_option
      |> List.iter (fun (domain, kdc) -> update_db ~domain ~kdc)
    with e -> error "Failed to update domain kdc %s" (Printexc.to_string e)

  let trigger_update ~start =
    if Pool_role.is_master () then (
      debug "Trigger task: %s" periodic_update_task_name ;
      Scheduler.add_to_queue periodic_update_task_name
        (Scheduler.Periodic !Xapi_globs.winbind_update_closest_kdc_interval)
        start update
    )

  let stop_update () =
    if Pool_role.is_master () then
      Scheduler.remove_from_queue periodic_update_task_name
end

module RotateMachinePassword = struct
  let task_name = "Rotating machine password"

  let rotate () =
    let now = Unix.time () in
    let now_str = string_of_float now in
    try
      let machine_pwd_last_change_time =
        (get_domain_info_from_db ()).machine_pwd_last_change_time
      in
      match machine_pwd_last_change_time with
      | Some time when now < time +. !Xapi_globs.winbind_machine_pwd_timeout ->
          ()
      | _ ->
          Wbinfo.call_wbinfo ["--change-secret"] |> maybe_raise |> ignore ;

          Server_helpers.exec_with_new_task
            "update machine password last change time"
          @@ fun __context ->
          update_extauth_configuration ~__context
            ~k:"machine_pwd_last_change_time" ~v:now_str
    with e ->
      debug "Failed to rotate machine password %s " (ExnHelper.string_of_exn e)

  let trigger_rotate ~start =
    debug "Trigger task: %s" task_name ;
    Scheduler.add_to_queue task_name
      (Scheduler.Periodic !Xapi_globs.winbind_machine_pwd_timeout) start rotate

  let stop_rotate () = Scheduler.remove_from_queue task_name
end

module type LocalHostTag = sig
  val local_ip : string
end

module HostsConfTagIPv4 : LocalHostTag = struct let local_ip = "127.0.0.1" end

module HostsConfTagIPv6 : LocalHostTag = struct let local_ip = "::1" end

module type HostsConf = sig
  (* add the domain info into conf*)
  val join : name:string -> domain:string -> lines:string list -> string list

  (* remove the domain info from conf*)
  val leave : name:string -> domain:string -> lines:string list -> string list
end

module HostsConfFunc (T : LocalHostTag) : HostsConf = struct
  let sep = ' '

  let sep_str = String.make 1 sep

  type t = Add | Remove

  let interest line = String.starts_with ~prefix:T.local_ip line

  let handle op name domain line =
    let line = String.lowercase_ascii line in
    let name = String.lowercase_ascii name in
    let domain = String.lowercase_ascii domain in
    let fqdn = Printf.sprintf "%s.%s" name domain in
    let rec add_hostname pre line =
      match line with
      | ip :: alias when ip = T.local_ip ->
          (* Add localhost IP *)
          add_hostname [ip] alias
      | sp :: left when sp = "" ->
          (* Add space to reserve the indent *)
          add_hostname (pre @ [sp]) left
      | alias :: left ->
          (* hosts entry: ip fqdn alias1 alias2 ... *)
          pre @ [fqdn; name; alias] @ left
      | [] ->
          failwith "Can not add local hostname to non local IP"
    in

    match interest line with
    | false ->
        line
    | true ->
        String.split_on_char sep line
        |> List.filter (fun x -> x <> name && x <> fqdn)
        |> (fun x -> match op with Add -> add_hostname [] x | Remove -> x)
        |> String.concat sep_str

  let leave ~name ~domain ~lines =
    List.map (fun line -> handle Remove name domain line) lines
    (* If no name for local ip left, just remove it *)
    |> List.filter (fun x -> String.trim x <> T.local_ip)

  let join ~name ~domain ~lines =
    List.map (fun line -> handle Add name domain line) lines |> fun x ->
    match List.exists (fun l -> interest l) x with
    | true ->
        x
    | false ->
        (* Does not found and updated the conf, then add one *)
        [
          Printf.sprintf "%s%s%s.%s%s%s" T.local_ip sep_str name domain sep_str
            name
        ]
        @ x
end

module HostsConfIPv4 = HostsConfFunc (HostsConfTagIPv4)
module HostsConfIPv6 = HostsConfFunc (HostsConfTagIPv6)

module ConfigHosts = struct
  open Xapi_stdext_unix.Unixext

  let path = "/etc/hosts"

  let join ~name ~domain =
    read_lines ~path |> fun lines ->
    HostsConfIPv4.join ~name ~domain ~lines |> fun lines ->
    HostsConfIPv6.join ~name ~domain ~lines |> fun x ->
    x @ [""] (* Add final line break *)
    |> String.concat "\n"
    |> write_string_to_file path

  let leave ~name ~domain =
    read_lines ~path |> fun lines ->
    HostsConfIPv4.leave ~name ~domain ~lines |> fun lines ->
    HostsConfIPv6.leave ~name ~domain ~lines |> fun x ->
    x @ [""] (* Add final line break *)
    |> String.concat "\n"
    |> write_string_to_file path
end

module DNSSync = struct
  let task_name = "Sync hostname with DNS"

  type t = Register | Unregister

  let handle op hostname netbios_name domain =
    (* By default, hostname should equal to netbios_name, just register it to DNS server*)
    try
      let ops =
        match op with Register -> "register" | Unregister -> "unregister"
      in
      let netbios_fqdn = Printf.sprintf "%s.%s" netbios_name domain in
      let args = ["ads"; "dns"] @ [ops] @ ["--machine-pass"] in
      Helpers.call_script net_cmd (args @ [netbios_fqdn]) |> ignore ;
      if hostname <> netbios_name then
        let hostname_fqdn = Printf.sprintf "%s.%s" hostname domain in
        (* netbios_name is compressed, op on extra hostname *)
        Helpers.call_script net_cmd (args @ [hostname_fqdn]) |> ignore
    with e ->
      debug "Register/unregister with DNS failed %s" (ExnHelper.string_of_exn e)

  let register hostname netbios_name domain =
    handle Register hostname netbios_name domain

  let unregister hostname netbios_name domain =
    handle Unregister hostname netbios_name domain

  let sync () =
    Server_helpers.exec_with_new_task "sync hostname with DNS"
    @@ fun __context ->
    let host = Helpers.get_localhost ~__context in
    let service_name =
      Db.Host.get_external_auth_service_name ~__context ~self:host
    in
    let netbios_name =
      Db.Host.get_external_auth_configuration ~__context ~self:host
      |> fun config -> List.assoc_opt "netbios_name" config
    in
    let hostname = Db.Host.get_hostname ~__context ~self:host in
    match netbios_name with
    | Some netbios ->
        register hostname netbios service_name
    | None ->
        debug "Netbios name is none, skip sync hostname to DNS"

  let trigger_sync ~start =
    debug "Trigger task: %s" task_name ;
    Scheduler.add_to_queue task_name
      (Scheduler.Periodic !Xapi_globs.winbind_dns_sync_interval) start sync

  let stop_sync () = Scheduler.remove_from_queue task_name
end

let build_netbios_name ~config_params =
  let key = "netbios-name" in
  match List.assoc_opt key config_params with
  | Some name ->
      if String.length name > max_netbios_name_length then
        raise (generic_ex "%s exceeds %d characters" key max_netbios_name_length)
      else
        name
  | None ->
      get_localhost_name () |> Winbind.build_netbios_name

let build_dns_hostname_option ~config_params =
  let key = "dns-hostname" in
  match List.assoc_opt key config_params with
  | Some name ->
      [Printf.sprintf "dnshostname=%s" name]
  | _ ->
      []

let closest_kdc_of_domain domain =
  match ClosestKdc.from_db domain with
  | Some kdc when Result.is_ok (workgroup_from_server kdc) ->
      kdc
  | _ ->
      (* Just pick the first valid KDC in the list *)
      kdc_of_domain domain

module AuthADWinbind : Auth_signature.AUTH_MODULE = struct
  let get_subject_identifier' subject_name =
    (* Called in the login path with a yet unauthenticated user *)
    let* domain =
      try Ok (get_domain_info_from_db ()).service_name with e -> Error e
    in
    let subject_name = domainify_uname ~domain subject_name in
    match Wbinfo.sid_of_name subject_name with
    | Ok sid ->
        Ok sid
    | Error e ->
        debug "Failed to query sid from cache, error: %s, retry ldap"
          (ExnHelper.string_of_exn e) ;
        let* domain, user = Wbinfo.domain_and_user_of_uname subject_name in
        let* domain_netbios =
          Wbinfo.domain_name_of ~target_name_type:NetbiosName ~from_name:domain
        in
        let kdc = closest_kdc_of_domain domain in
        Ldap.query_sid ~name:user ~kdc ~domain_netbios

  (* subject_id get_subject_identifier(string subject_name)

      Takes a subject_name (as may be entered into the XenCenter UI when defining subjects --
      see Access Control wiki page); and resolves it to a subject_id against the external
      auth/directory service.
      Raises Not_found (*Subject_cannot_be_resolved*) if authentication is not succesful.
  *)
  let get_subject_identifier ~__context subject_name =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
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

  let authenticate_username_password ~__context uname password =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    (* the `wbinfo --krb5auth` expects the username to be in either SAM or UPN format.
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
           D.error "authenticate_username_password:ex: %s" (Printexc.to_string e) ;
           Auth_failure
             (Printf.sprintf "couldn't get SID from username='%s'" uname)
     in
     let* () =
       match Wbinfo.name_of_sid sid >>| Wbinfo.string_of_name with
       | Error e ->
           D.warn
             "authenticate_username_password: trying original uname. ex: %s"
             (Printexc.to_string e) ;
           Wbinfo.kerberos_auth orig_uname password
       | Ok uname ->
           Wbinfo.kerberos_auth uname password
     in
     Ok sid
    )
    |> maybe_raise

  (* subject_id Authenticate_ticket(string ticket)

      As above but uses a ticket as credentials (i.e. for single sign-on)
  *)
  (* not implemented now, not needed for our tests, only for a *)
  (* future single sign-on feature *)
  let authenticate_ticket ~__context:_ _tgt =
    failwith "extauth_plugin authenticate_ticket not implemented"

  let query_subject_information_group (name : string) (gid : int) (sid : string)
      =
    [
      ("subject-name", name)
    ; ("subject-gid", string_of_int gid)
    ; ("subject-sid", sid)
    ; ("subject-is-group", string_of_bool true)
    ]

  let query_subject_information_user (uid : int) (sid : string) =
    (* user_name like DOMAIN\user_1 *)
    let* {user_name; gecos; gid; _} = Wbinfo.uid_info_of_uid uid in
    let sam_uname = user_name in
    let* domain_netbios, domain = Wbinfo.domain_of_uname user_name in
    (* uname like user_1 *)
    let* _, uname = Wbinfo.domain_and_user_of_uname user_name in
    let default_account =
      Ldap.
        {
          name= sam_uname
        ; upn= ""
        ; display_name= ""
        ; account_disabled= false
        ; account_expired= false
        ; account_locked= false
        ; password_expired= false
        }
    in

    let* {
           upn
         ; display_name
         ; account_disabled
         ; account_expired
         ; account_locked
         ; password_expired
         ; _
         } =
      match ClosestKdc.from_db domain with
      | Some _ -> (
          let closest_kdc = closest_kdc_of_domain domain in
          let timeout = !Xapi_globs.winbind_ldap_query_subject_timeout in
          match Ldap.query_user sid domain_netbios closest_kdc ~timeout with
          | Ok user ->
              Ok user
          | _ ->
              debug "Ldap query user failed, fallback to default value" ;
              Ok default_account
        )
      | None ->
          (* Xapi database does not have any DC information about this domain
           * This is very likely caused by this domain does not trust
           * the joined domain (with 1 way trust ) , just return the default value
           * This is NOT a regression issue of PBIS
           * PBIS cannot handle such case neither
           *)
          debug "Fallback to default value as no DC info in xapi database" ;
          Ok default_account
    in

    Ok
      [
        ("subject-name", sam_uname)
      ; ("subject-gecos", gecos)
      ; ( "subject-displayname"
        , if display_name <> "" then
            Printf.sprintf "%s\\%s" domain_netbios display_name
          else if gecos <> "" && gecos <> "<null>" then
            Printf.sprintf "%s\\%s" domain_netbios gecos
          else
            sam_uname
        )
      ; ("subject-uid", string_of_int uid)
      ; ("subject-gid", string_of_int gid)
      ; ( "subject-upn"
        , if upn <> "" then upn else Printf.sprintf "%s@%s" uname domain
        )
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
  let query_subject_information ~__context (sid : string) =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let res =
      let* name = Wbinfo.name_of_sid sid in
      match name with
      | User _ ->
          let* uid = Wbinfo.uid_of_sid sid in
          query_subject_information_user uid sid
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
  let query_group_membership ~__context subject_identifier =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    maybe_raise (Wbinfo.user_domgroups subject_identifier)

  let assert_join_domain_user_format uname =
    if Astring.String.is_infix ~affix:{|\|} uname then
      raise
        (generic_ex
           "SAM format for user detected in '%s'. This is not supported, \
            please retry using UPN format"
           uname
        )

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
  let on_enable ~__context config_params =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let user =
      from_config ~name:"user" ~err_msg:"enable requires user" ~config_params
    in
    let pass =
      from_config ~name:"pass" ~err_msg:"enable requires pass" ~config_params
    in

    assert_join_domain_user_format user ;

    let netbios_name = build_netbios_name ~config_params in

    let dns_hostname_option = build_dns_hostname_option ~config_params in

    assert_hostname_valid ~hostname:netbios_name ;

    let {service_name; _} = get_domain_info_from_db () in
    assert_domain_equal_service_name ~service_name ~config_params ;

    let workgroup =
      (* Query new domain workgroup during join domain *)
      query_domain_workgroup ~domain:service_name
    in
    config_winbind_daemon ~domain:(Some service_name)
      ~workgroup:(Some workgroup) ~netbios_name:(Some netbios_name) ;

    let ou_conf, ou_param = extract_ou_config ~config_params in

    let args =
      [
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
        @ kerberos_opt
      ; ou_param
      ; dns_hostname_option
      ]
      |> List.concat
    in
    debug "Joining domain %s with user %s netbios_name %s" service_name user
      netbios_name ;
    let env = [|Printf.sprintf "PASSWD=%s" pass|] in
    try
      Helpers.call_script ~env net_cmd args |> ignore ;
      (* Need to restart to refresh cache *)
      Winbind.restart ~timeout:5. ~wait_until_success:true ;
      Winbind.check_ready_to_serve ~timeout:300. ;
      let machine_pwd_last_change_time = Unix.time () |> string_of_float in
      persist_extauth_config ~domain:(Some service_name) ~user:(Some user)
        ~ou_conf ~workgroup:(Some workgroup)
        ~machine_pwd_last_change_time:(Some machine_pwd_last_change_time)
        ~netbios_name:(Some netbios_name) ;
      (* Trigger right now *)
      ClosestKdc.trigger_update ~start:0. ;
      RotateMachinePassword.trigger_rotate ~start:0. ;
      ConfigHosts.join ~domain:service_name ~name:netbios_name ;
      let _, _ =
        Forkhelpers.execute_command_get_output !Xapi_globs.set_hostname
          [get_localhost_name ()]
      in
      (* Trigger right now *)
      DNSSync.trigger_sync ~start:0. ;
      Winbind.set_machine_account_encryption_type netbios_name ;
      debug "Succeed to join domain %s" service_name
    with
    | Forkhelpers.Spawn_internal_error (_, stdout, _) ->
        error "Join domain: %s error: %s" service_name stdout ;
        clear_winbind_config () ;
        ConfigHosts.leave ~domain:service_name ~name:netbios_name ;
        (* The configure is kept for debug purpose with max level *)
        raise (Auth_service_error (stdout |> tag_from_err_msg, stdout))
    | Xapi_systemctl.Systemctl_fail _ ->
        let msg = Printf.sprintf "Failed to start %s" Winbind.name in
        error "Start daemon error: %s" msg ;
        config_winbind_daemon ~domain:None ~workgroup:None ~netbios_name:None ;
        ConfigHosts.leave ~domain:service_name ~name:netbios_name ;
        raise (Auth_service_error (E_GENERIC, msg))
    | e ->
        let msg =
          Printf.sprintf
            "Failed to enable extauth for domain %s with user %s, error: %s"
            service_name user
            (ExnHelper.string_of_exn e)
        in
        error "Enable extauth error: %s" msg ;
        clear_winbind_config () ;
        ConfigHosts.leave ~domain:service_name ~name:netbios_name ;
        raise (Auth_service_error (E_GENERIC, msg))

  (* unit on_disable()

      Called internally by xapi _on each host_ when a client disables an auth service via the XenAPI.
      The hook will be called _before_ the Pool configuration fields relating to the external-auth
      service are cleared (i.e. so you can access the config params you need from the pool metadata
      within the body of the on_disable method)
  *)
  let on_disable ~__context config_params =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in
    let user = List.assoc_opt "user" config_params in
    let pass = List.assoc_opt "pass" config_params in
    let {service_name; netbios_name; _} = get_domain_info_from_db () in
    DNSSync.stop_sync () ;
    ( match netbios_name with
    | Some netbios ->
        ConfigHosts.leave ~domain:service_name ~name:netbios ;
        let hostname = get_localhost_name () in
        DNSSync.unregister hostname netbios service_name
    | _ ->
        ()
    ) ;

    (* Clean extauth config *)
    persist_extauth_config ~domain:None ~user:None ~ou_conf:[] ~workgroup:None
      ~machine_pwd_last_change_time:None ~netbios_name:None ;
    ClosestKdc.stop_update () ;
    RotateMachinePassword.stop_rotate () ;
    (* The caller disable external auth even disable machine account failed,
     * We run clear_machine_account after some necessary resources get cleared *)
    finally
      (fun () -> clear_machine_account ~service_name (user, pass))
      (fun () -> clear_winbind_config () ; clear_local_resources ()) ;

    debug "Succeed to disable external auth for %s" service_name

  (* unit on_xapi_initialize(bool system_boot)

      Called internally by xapi whenever it starts up. The system_boot flag is true iff xapi is
      starting for the first time after a host boot
  *)
  let on_xapi_initialize ~__context _system_boot =
    let@ __context = Context.with_tracing ~__context __FUNCTION__ in

    Winbind.start ~timeout:5. ~wait_until_success:true ;
    ClosestKdc.trigger_update ~start:ClosestKdc.startup_delay ;
    RotateMachinePassword.trigger_rotate ~start:5. ;
    Winbind.check_ready_to_serve ~timeout:300. ;
    DNSSync.trigger_sync ~start:5. ;

    let {service_name; netbios_name; _} = get_domain_info_from_db () in
    match netbios_name with
    | Some name ->
        ConfigHosts.join ~domain:service_name ~name
    | _ ->
        ()

  (* unit on_xapi_exit()

      Called internally when xapi is doing a clean exit.
  *)
  let on_xapi_exit ~__context:_ () = ()

  (* Implement the single value required for the module signature *)
  let methods =
    Auth_signature.
      {
        authenticate_username_password
      ; authenticate_ticket
      ; get_subject_identifier
      ; query_subject_information
      ; query_group_membership
      ; on_enable
      ; on_disable
      ; on_xapi_initialize
      ; on_xapi_exit
      }
end
