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

open Test_highlevel

module ExtractOuConfig = Generic.MakeStateless (struct
  module Io = struct
    type input_t = (string * string) list

    type output_t = (string * string) list * string list

    let string_of_input_t = Test_printers.(assoc_list string string)

    let string_of_output_t =
      Test_printers.(pair (assoc_list string string) (list string))
  end

  let transform x = Extauth_plugin_ADwinbind.extract_ou_config ~config_params:x

  let tests =
    `QuickAndAutoDocumented
      [
        ([("auth-type", "AD"); ("service-name", "conappada.local")], ([], []))
      ; ( [
            ("auth-type", "AD")
          ; ("service-name", "conappada.local")
          ; ("ou", "TOU")
          ]
        , ([("ou", "TOU")], ["createcomputer=TOU"])
        )
      ]
end)

module Range = Generic.MakeStateless (struct
  module Io = struct
    type input_t = int * int * int

    type output_t = int list

    let string_of_input_t (x, y, z) = Printf.sprintf "%d , %d, %d" x y z

    let string_of_output_t = Test_printers.(list int)
  end

  let transform (s, e, step) =
    Extauth_plugin_ADwinbind.Migrate_from_pbis.range s e step

  let tests =
    `QuickAndAutoDocumented
      [
        ((0, 10, 1), [0; 1; 2; 3; 4; 5; 6; 7; 8; 9])
      ; ((0, 1, 1), [0])
      ; ((0, 0, 1), [])
      ; ((1, 0, 1), [])
      ]
end)

module Errtag = Generic.MakeStateless (struct
  open Auth_signature

  module Io = struct
    type input_t = string

    type output_t = Auth_signature.auth_service_error_tag

    let string_of_input_t = Test_printers.(string)

    let string_of_output_t = function
      | E_GENERIC ->
          "E_GENERIC"
      | E_LOOKUP ->
          "E_LOOKUP"
      | E_DENIED ->
          "E_DENIED"
      | E_CREDENTIALS ->
          "E_CREDENTIALS"
      | E_UNAVAILABLE ->
          "E_UNAVAILABLE"
      | E_INVALID_OU ->
          "E_INVALID_OU"
      | E_INVALID_ACCOUNT ->
          "E_INVALID_ACCOUNT"
  end

  let transform = Extauth_plugin_ADwinbind.tag_from_err_msg

  let tests =
    `QuickAndAutoDocumented
      [
        ( "Failed to join domain: failed to lookup DC info for domain \
           'testdomain.local' over rpc: The name provided is not a properly \
           formed account name."
        , E_CREDENTIALS
        )
      ; ( "The attempted logon is invalid. This is either due to a bad \
           username or authentication information"
        , E_CREDENTIALS
        )
      ]
end)

module ParseValueFromPbis = Generic.MakeStateless (struct
  module Io = struct
    type input_t = string

    type output_t = string

    let string_of_input_t = Test_printers.(string)

    let string_of_output_t = Test_printers.(string)
  end

  let transform s =
    Extauth_plugin_ADwinbind.Migrate_from_pbis.parse_value_from_pbis s

  let tests =
    `QuickAndAutoDocumented
      [
        ( "X'58005200540055004B002D00300032002D003000330024000000'"
        , "XRTUK-02-03"
        )
      ; ("X'4C004F00430041004C0048004F0053005400320024000000'", "LOCALHOST2")
      ]
end)

let test_domainify_uname =
  let open Extauth_plugin_ADwinbind in
  let check uname exp () =
    let msg = Printf.sprintf "%s -> %s" uname exp in
    let ac = domainify_uname ~domain:"domain.net" uname in
    Alcotest.(check string) msg exp ac
  in
  let matrix =
    [
      ("KRBTGT", "KRBTGT")
    ; ({|user|}, {|user@domain.net|})
    ; ({|user@domain.net|}, {|user@domain.net|})
    ; ({|DOMAIN\user|}, {|DOMAIN\user|})
      (* if username already contains a domain, DO NOT try and correct it *)
    ; ({|user@unknowndomain.net|}, {|user@unknowndomain.net|})
    ; ({|UNKNOWNDOMAIN\user|}, {|UNKNOWNDOMAIN\user|})
    ]
  in
  matrix
  |> List.map @@ fun (inp, exp) ->
     (Printf.sprintf "%s -> %s" inp exp, `Quick, check inp exp)

let test_build_netbios_name =
  let open Extauth_plugin_ADwinbind.Winbind in
  let check localhost_name exp () =
    let msg = Printf.sprintf "%s -> %s" localhost_name exp in
    let ac = build_netbios_name localhost_name in
    Alcotest.(check string) msg exp ac
  in
  let matrix =
    [({|HOSTNAME|}, {|HOSTNAME|}); ({|HOSTNAME.domain.net|}, {|HOSTNAME|})]
  in
  matrix
  |> List.map @@ fun (inp, exp) ->
     (Printf.sprintf "%s -> %s" inp exp, `Quick, check inp exp)

let test_ldap_escape =
  let open Extauth_plugin_ADwinbind.Ldap in
  let check str exp () =
    let msg = Printf.sprintf "%s -> %s" str exp in
    let escaped = escape str in
    Alcotest.(check string) msg exp escaped
  in
  let matrix =
    [
      ({|user|}, {|user|})
    ; ({|(user)|}, {|\28user\29|})
    ; ({|(user|}, {|\28user|})
    ; ({|user)|}, {|user\29|})
    ; ({|us\er)|}, {|us\5der\29|})
    ; ({|user)1|}, {|user\291|})
    ; ({|user*|}, {|user\2a|})
    ]
  in
  matrix
  |> List.map @@ fun (inp, exp) ->
     (Printf.sprintf "%s -> %s" inp exp, `Quick, check inp exp)

let test_parse_wbinfo_uid_info =
  let open Extauth_plugin_ADwinbind.Wbinfo in
  let string_of_result x =
    match x with
    | Ok x ->
        Printf.sprintf "Ok %s" (string_of_uid_info x)
    | Error () ->
        "Error ()"
  in
  let uid_info = Test_util.alcotestable_of_pp string_of_result in
  let check stdout exp () =
    let msg = Printf.sprintf "<omit> -> %s" (string_of_result exp) in
    let ac = parse_uid_info stdout in
    Alcotest.(check uid_info) msg exp ac
  in

  let matrix =
    [
      ( {|CONNAPP\ladmin:*:3000003:3000174:ladmin:/home/CONNAPP/ladmin:/bin/bash|}
      , Ok
          {
            user_name= {|CONNAPP\ladmin|}
          ; uid= 3000003
          ; gid= 3000174
          ; gecos= {|ladmin|}
          }
      )
      (* XSI-1901: output of customer environment, has `:` in the gecos,
         other fields does not likely contain it *)
    ; ( {|HVS\udaadmin:*:3000000:3000000:ADMIN: Dalsem, Ulric:/home/HVS/udaadmin:/bin/bash|}
      , Ok
          {
            user_name= {|HVS\udaadmin|}
          ; uid= 3000000
          ; gid= 3000000
          ; gecos= {|ADMIN: Dalsem, Ulric|}
          }
      )
      (* Multiple `:` in gecos *)
    ; ( {|HVS\udaadmin:*:3000000:3000000:ADMIN: Dalsem, Ulric, POOL OP: udaadmin:/home/HVS/udaadmin:/bin/bash|}
      , Ok
          {
            user_name= {|HVS\udaadmin|}
          ; uid= 3000000
          ; gid= 3000000
          ; gecos= {|ADMIN: Dalsem, Ulric, POOL OP: udaadmin|}
          }
      )
    ; ( {|CONNAPP\locked:*:3000004:3000174::/home/CONNAPP/locked:/bin/bash|}
      , Ok
          {user_name= {|CONNAPP\locked|}; uid= 3000004; gid= 3000174; gecos= ""}
      )
    ; ({|blah|}, Error ())
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> (inp, `Quick, check inp exp)

let test_parse_ldap_stdout =
  let open Extauth_plugin_ADwinbind.Ldap in
  let string_of_result = function
    | Ok x ->
        Printf.sprintf "Ok %s" (string_of_user x)
    | Error x ->
        Printf.sprintf "Error %s" x
  in
  let ldap_user = Test_util.alcotestable_of_pp string_of_result in
  let check stdout exp () =
    let msg = Printf.sprintf "<omit> -> %s" (string_of_result exp) in
    let ac = parse_user stdout in
    Alcotest.(check ldap_user) msg exp ac
  in

  let stdout_ladmin =
    {|Got 1 replies

objectClass: top
objectClass: person
objectClass: organizationalPerson
objectClass: user
cn: ladmin
givenName: ladmin
distinguishedName: CN=ladmin,CN=Users,DC=conappada,DC=local
instanceType: 4
whenCreated: 20210203072119.0Z
whenChanged: 20210518070906.0Z
displayName: ladmin
uSNCreated: 70509
memberOf: CN=Domain Admins,CN=Users,DC=conappada,DC=local
memberOf: CN=Enterprise Admins,CN=Users,DC=conappada,DC=local
memberOf: CN=Administrators,CN=Builtin,DC=conappada,DC=local
uSNChanged: 362555
name: ladmin
objectGUID: 1a29218b-f145-45ce-a181-e471864c934b
userAccountControl: 66048
badPwdCount: 0
codePage: 0
countryCode: 0
badPasswordTime: 132657962285300918
lastLogoff: 0
lastLogon: 132663205168179135
pwdLastSet: 132573331165736472
primaryGroupID: 513
objectSid: S-1-5-21-3143668282-2591278241-912959342-1178
adminCount: 1
accountExpires: 9223372036854775807
logonCount: 1166
sAMAccountName: ladmin
sAMAccountType: 805306368
userPrincipalName: ladmin@conappada.local
objectCategory: CN=Person,CN=Schema,CN=Configuration,DC=conappada,DC=local
dSCorePropagationData: 20210203085144.0Z
dSCorePropagationData: 16010101000000.0Z
lastLogonTimestamp: 132657953463490742
msDS-SupportedEncryptionTypes: 24
  |}
  in
  let stdout_krbtgt =
    {|


    Got 1 replies

objectClass: top
objectClass: person
objectClass: organizationalPerson
objectClass: user
cn: krbtgt
description: Key Distribution Center Service Account
distinguishedName: CN=krbtgt,CN=Users,DC=conappada,DC=local
instanceType: 4
whenCreated: 20201029064320.0Z
whenChanged: 20201029065834.0Z
uSNCreated: 12324
memberOf: CN=Denied RODC Password Replication Group,CN=Users,DC=conappada,DC=local
uSNChanged: 12767
showInAdvancedViewOnly: TRUE
name: krbtgt
objectGUID: d898ace2-862f-456d-b5da-e012f412a8a2
userAccountControl: 514
badPwdCount: 0
codePage: 0
countryCode: 0
badPasswordTime: 0
lastLogoff: 0
lastLogon: 0
pwdLastSet: 132484274001314113
primaryGroupID: 513
objectSid: S-1-5-21-3143668282-2591278241-912959342-502
adminCount: 1
accountExpires: 9223372036854775807
logonCount: 0
sAMAccountName: krbtgt
sAMAccountType: 805306368
servicePrincipalName: kadmin/changepw
objectCategory: CN=Person,CN=Schema,CN=Configuration,DC=conappada,DC=local
isCriticalSystemObject: TRUE
dSCorePropagationData: 20201029065834.0Z
dSCorePropagationData: 20201029064320.0Z
dSCorePropagationData: 16010101000416.0Z
msDS-SupportedEncryptionTypes: 0
    |}
  in
  let stdout_locked =
    {| Got 1 replies

objectClass: top
objectClass: person
objectClass: organizationalPerson
objectClass: user
cn: locked
givenName: locked
distinguishedName: CN=locked,CN=Users,DC=conappada,DC=local
instanceType: 4
whenCreated: 20210303032518.0Z
whenChanged: 20210303041203.0Z
displayName: locked
uSNCreated: 101671
uSNChanged: 101737
name: locked
objectGUID: 26f8c3e3-e4cd-4bd1-b0fb-4e4f84c65a49
userAccountControl: 512
badPwdCount: 0
codePage: 0
countryCode: 0
badPasswordTime: 132592179051171201
lastLogoff: 0
lastLogon: 132592972097050310
pwdLastSet: 132592177189171588
primaryGroupID: 513
objectSid: S-1-5-21-3143668282-2591278241-912959342-1202
accountExpires: 9223372036854775807
logonCount: 3
sAMAccountName: locked
sAMAccountType: 805306368
userPrincipalName: locked@conappada.local
lockoutTime: 132756473173422789
objectCategory: CN=Person,CN=Schema,CN=Configuration,DC=conappada,DC=local
dSCorePropagationData: 16010101000000.0Z
lastLogonTimestamp: 132592177310358648



    |}
  in
  let stdout_expired =
    {|Got 1 replies

objectClass: top
objectClass: person
objectClass: organizationalPerson
objectClass: user
cn: experied
givenName: experied
distinguishedName: CN=experied,CN=Users,DC=conappada,DC=local
instanceType: 4
whenCreated: 20210302074856.0Z
whenChanged: 20210302081509.0Z
displayName: experied
uSNCreated: 101115
uSNChanged: 101243
name: experied
objectGUID: caaa3e6a-fe9e-4c0c-9704-931f7445fb3f
userAccountControl: 512
badPwdCount: 16
codePage: 0
countryCode: 0
badPasswordTime: 132591456191947359
lastLogoff: 0
lastLogon: 0
pwdLastSet: 132591449361503688
primaryGroupID: 513
objectSid: S-1-5-21-3143668282-2591278241-912959342-1200
accountExpires: 132665213390000000
logonCount: 0
sAMAccountName: experied
sAMAccountType: 805306368
userPrincipalName: experied@conappada.local
lockoutTime: 0
msDS-UserPasswordExpiryTimeComputed: 132756434082908291
objectCategory: CN=Person,CN=Schema,CN=Configuration,DC=conappada,DC=local
dSCorePropagationData: 16010101000000.0Z
msDS-SupportedEncryptionTypes: 0|}
  in
  let matrix =
    [
      ( stdout_ladmin
      , Ok
          {
            upn= "ladmin@conappada.local"
          ; name= "ladmin"
          ; display_name= "ladmin"
          ; account_disabled= false
          ; account_expired= false
          ; account_locked= false
          ; password_expired= false
          }
      )
    ; ( stdout_locked
      , Ok
          {
            upn= "locked@conappada.local"
          ; name= "locked"
          ; display_name= "locked"
          ; account_disabled= false
          ; account_expired= false
          ; account_locked= true
          ; password_expired= false
          }
      )
    ; ( stdout_expired
      , Ok
          {
            upn= "experied@conappada.local"
          ; name= "experied"
          ; display_name= "experied"
          ; account_disabled= false
          ; account_expired= true
          ; account_locked= false
          ; password_expired= true
          }
      )
    ; ( stdout_krbtgt
      , Ok
          {
            upn= ""
          ; display_name= ""
          ; name= "krbtgt"
          ; password_expired= false
          ; account_locked= false
          ; account_expired= false
          ; account_disabled= true
          }
      )
    ; ("Got 0 replies", Error "ldap parsing failed ': got 0 replies'")
    ; ( "complete garbage"
      , Error "ldap parsing failed 'unexpected header: string'"
      )
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> ("<omit inp>", `Quick, check inp exp)

let test_wbinfo_exception_of_stderr =
  let open Extauth_plugin_ADwinbind.Wbinfo in
  let open Auth_signature in
  let string_of_result x =
    match x with
    | Some ex ->
        Printf.sprintf "Some %s" (Printexc.to_string ex)
    | None ->
        "None"
  in
  let ex = Test_util.alcotestable_of_pp string_of_result in
  let check stderr exp () =
    let msg = Printf.sprintf "<omit> -> %s" (string_of_result exp) in
    let ac = exception_of_stderr stderr in
    Alcotest.(check ex) msg exp ac
  in
  let matrix =
    [
      ( "failed to call wbcLookupName: WBC_ERR_DOMAIN_NOT_FOUND\x0ACould not \
         lookup name ladmin@mydomain.net\x0A"
      , Some (Auth_service_error (E_GENERIC, "WBC_ERR_DOMAIN_NOT_FOUND"))
      )
    ; ("garbage", None)
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> ("<omit inp>", `Quick, check inp exp)

let test_add_ipv4_localhost_to_hosts =
  let open Extauth_plugin_ADwinbind in
  let check inp exp () =
    let msg =
      Printf.sprintf "%s -> %s" (String.concat "\n" inp) (String.concat "\n" exp)
    in
    let actual =
      HostsConfIPv4.join ~name:"hostname" ~domain:"domain" ~lines:inp
    in
    Alcotest.(check @@ list string) msg exp actual
  in
  let matrix =
    [
      ( [
          "127.0.0.1   localhost localhost.localdomain localhost4 \
           localhost4.localdomain4"
        ]
      , [
          "127.0.0.1   hostname.domain hostname localhost \
           localhost.localdomain localhost4 localhost4.localdomain4"
        ]
      )
    ; ( ["127.0.0.1   localhost hostname hostname.domain localhost.localdomain"]
      , ["127.0.0.1   hostname.domain hostname localhost localhost.localdomain"]
      )
    ; ( ["192.168.0.1   some_host"]
      , ["127.0.0.1 hostname.domain hostname"; "192.168.0.1   some_host"]
      )
    ; ([], ["127.0.0.1 hostname.domain hostname"])
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> ("<omit inp>", `Quick, check inp exp)

let test_add_ipv4_and_ipv6_localhost_to_hosts =
  let open Extauth_plugin_ADwinbind in
  let check inp exp () =
    let msg =
      Printf.sprintf "%s -> %s" (String.concat "\n" inp) (String.concat "\n" exp)
    in
    let actual =
      HostsConfIPv6.join ~name:"hostname" ~domain:"domain" ~lines:inp
      |> fun lines ->
      HostsConfIPv4.join ~name:"hostname" ~domain:"domain" ~lines
    in
    Alcotest.(check @@ list string) msg exp actual
  in
  let matrix =
    [
      ( ["127.0.0.1   localhost"]
      , [
          "::1 hostname.domain hostname"
        ; "127.0.0.1   hostname.domain hostname localhost"
        ]
      )
    ; ( ["127.0.0.1   localhost"; "::1 localhost"]
      , [
          "127.0.0.1   hostname.domain hostname localhost"
        ; "::1 hostname.domain hostname localhost"
        ]
      )
    ; ( []
      , ["127.0.0.1 hostname.domain hostname"; "::1 hostname.domain hostname"]
      )
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> ("<omit inp>", `Quick, check inp exp)

let test_remove_ipv4_localhost_from_hosts =
  let open Extauth_plugin_ADwinbind in
  let check inp exp () =
    let msg =
      Printf.sprintf "%s -> %s" (String.concat "\n" inp) (String.concat "\n" exp)
    in
    let actual =
      HostsConfIPv4.leave ~name:"hostname" ~domain:"domain" ~lines:inp
    in
    Alcotest.(check @@ list string) msg exp actual
  in
  let matrix =
    [
      ( [
          "127.0.0.1   localhost localhost.localdomain localhost4 \
           localhost4.localdomain4"
        ]
      , [
          "127.0.0.1   localhost localhost.localdomain localhost4 \
           localhost4.localdomain4"
        ]
      )
    ; ( ["127.0.0.1   localhost hostname hostname.domain localhost.localdomain"]
      , ["127.0.0.1   localhost localhost.localdomain"]
      )
    ; (["127.0.0.1   hostname hostname.domain"], [])
    ; ( ["192.168.0.1   some_host"; "127.0.0.1   localhost hostname"]
      , ["192.168.0.1   some_host"; "127.0.0.1   localhost"]
      )
    ]
  in
  matrix |> List.map @@ fun (inp, exp) -> ("<omit inp>", `Quick, check inp exp)

let tests =
  [
    ("ADwinbind:extract_ou_config", ExtractOuConfig.tests)
  ; ("ADwinbind:test_range", Range.tests)
  ; ("ADwinbind:test_parse_value_from_pbis", ParseValueFromPbis.tests)
  ; ("ADwinbind:test_domainify_uname", test_domainify_uname)
  ; ("ADwinbind:test_build_netbios_name", test_build_netbios_name)
  ; ("ADwinbind:test_ldap_escape", test_ldap_escape)
  ; ("ADwinbind:test_parse_wbinfo_uid_info", test_parse_wbinfo_uid_info)
  ; ("ADwinbind:test_parse_ldap_stdout", test_parse_ldap_stdout)
  ; ( "ADwinbind:test_wbinfo_exception_of_stderr"
    , test_wbinfo_exception_of_stderr
    )
  ; ( "ADwinbind:test_add_ipv4_localhost_to_hosts"
    , test_add_ipv4_localhost_to_hosts
    )
  ; ( "ADwinbind:test_remove_ipv4_localhost_from_hosts"
    , test_remove_ipv4_localhost_from_hosts
    )
  ; ( "ADwinbind:test_add_ipv4_and_ipv6_localhost_to_hosts"
    , test_add_ipv4_and_ipv6_localhost_to_hosts
    )
  ]
