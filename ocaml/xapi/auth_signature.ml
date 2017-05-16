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
 * Interface for External Authentication Plugin component
 * @group Access Control
*)

(*
 * v1 22Oct08
 *
*)

exception Auth_failure of string
type auth_service_error_tag = E_GENERIC|E_LOOKUP|E_DENIED|E_CREDENTIALS|E_UNAVAILABLE|E_INVALID_OU|E_INVALID_ACCOUNT
exception Auth_service_error of auth_service_error_tag * string
exception Subject_cannot_be_resolved

let suffix_of_tag errtag =
  match errtag with
  | E_GENERIC -> ""
  | E_LOOKUP -> Api_errors.auth_suffix_domain_lookup_failed
  | E_DENIED -> Api_errors.auth_suffix_permission_denied
  | E_CREDENTIALS -> Api_errors.auth_suffix_wrong_credentials
  | E_UNAVAILABLE -> Api_errors.auth_suffix_unavailable
  | E_INVALID_OU -> Api_errors.auth_suffix_invalid_ou
  | E_INVALID_ACCOUNT -> Api_errors.auth_suffix_invalid_account

(* required fields in subject.other_config *)
let subject_information_field_subject_name = "subject-name"

type t =
  {

    (* subject_id Authenticate_username_password(string username, string password)

       	 Takes a username and password, and tries to authenticate against an already configured
       	 auth service (see XenAPI requirements Wiki page for details of how auth service configuration
       	 takes place and the appropriate values are stored within the XenServer Metadata).
       	 If authentication is successful then a subject_id is returned representing the account
       	 corresponding to the supplied credentials (where the subject_id is in a namespace managed by
       	 the auth module/service itself -- e.g. maybe a SID or something in the AD case).
       	 Raises auth_failure if authentication is not successful
    *)
    authenticate_username_password : string -> string -> string;

    (* subject_id Authenticate_ticket(string ticket)

       	 As above but uses a ticket as credentials (i.e. for single sign-on)
    *)
    authenticate_ticket : string -> string;

    (* subject_id get_subject_identifier(string subject_name)

       	 Takes a subject_name (as may be entered into the XenCenter UI when defining subjects --
       	 see Access Control wiki page); and resolves it to a subject_id against the external
       	 auth/directory service.
       	 Raises Not_found if authentication is not succesful.
    *)
    get_subject_identifier : string -> string;

    (* ((string*string) list) query_subject_information(string subject_identifier)

       	 Takes a subject_identifier and returns the user record from the directory service as
       	 key/value pairs. In the returned string*string map, there _must_ be a key called
       	 subject_name that refers to the name of the account (e.g. the user or group name as may
       	 be displayed in XenCenter). There is no other requirements to include fields from the user
       	 record -- initially I'd imagine that we wouldn't bother adding anything else here, but
       	 it's a string*string list anyway for possible future expansion.
       	 Raises Not_found if subject_id cannot be resolved by external auth service
    *)
    query_subject_information : string -> ((string*string) list);

    (* (string list) query_group_membership(string subject_identifier)

       	 Takes a subject_identifier and returns its group membership (i.e. a list of subject
       	 identifiers of the groups that the subject passed in belongs to). The set of groups returned
       	 _must_ be transitively closed wrt the is_member_of relation if the external directory service
       	 supports nested groups (as AD does for example)
    *)
    query_group_membership : string -> (string list);

      (*
	In addition, there are some event hooks that auth modules implement as follows:
      *)

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
    on_enable : ((string*string) list) -> unit;

    (* unit on_disable()

       	 Called internally by xapi _on each host_ when a client disables an auth service via the XenAPI.
       	 The hook will be called _before_ the Pool configuration fields relating to the external-auth
       	 service are cleared (i.e. so you can access the config params you need from the pool metadata
       	 within the body of the on_disable method)
    *)
    on_disable : ((string*string) list) -> unit;

    (* unit on_xapi_initialize(bool system_boot)

       	 Called internally by xapi whenever it starts up. The system_boot flag is true iff xapi is
       	 starting for the first time after a host boot
    *)
    on_xapi_initialize : bool -> unit;

    (* unit on_xapi_exit()

       	 Called internally when xapi is doing a clean exit.
    *)
    on_xapi_exit : unit -> unit;

  }

(* Auth modules must implement this signature:*)
module type AUTH_MODULE =
sig
  val methods : t
end
