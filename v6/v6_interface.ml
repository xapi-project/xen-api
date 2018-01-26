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
   @group Licensing
*)
open Rpc
open Idl

let service_name = "v6d"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)
let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)
let uri () = "file:" ^ !default_path
let json_path = "/var/xapi/v6.json"
let xml_path = "/var/xapi/v6"

(** Uninterpreted/sanitised string associated with operation *)
type debug_info = string
[@@deriving rpcty]

(** Record representing software edition *)
type edition =
  {
    title : string ;
    (** Name of edition, this will be passed to apply_edition *)
    official_title : string ;
    (** Marketing title used to advertise edition *)
    code : string ;
    (** Abbreviated form of name, used to show up in logs and on command line *)
    order : int ;
    (** Number indicating ordering among other editions;
        low numbers correspond to fewer features, and vice versa *)
  }
[@@deriving rpcty]

(** List of edition records *)
type edition_list = edition list
[@@deriving rpcty]

(** Record of edition info, including xapi parameters and features *)
type edition_info =
  {
    edition_name: string;
    (** Name of edition *)
    xapi_params: (string * string) list;
    (** List of parameters used by Xapi *)
    additional_params: (string * string) list;
    (** Addition parameters supplied *)
    experimental_features: (string * bool) list;
    (** List of experimental features and whether they're available in this edition *)
  }
[@@deriving rpcty]

(** [string * string] list *)
type string_pair_lst = (string * string) list
[@@deriving rpcty]

(** Wrapper for specific errors in managing features *)
type errors =
  | Invalid_edition of string
  (** Thrown by apply_edition on receiving unfamiliar edition.
   *  Note: get_editions returns list of all valid editions *)
  | License_expired
  (** Thrown by license_check when expiry date matches or precedes current date *)
  | License_processing_error
  (** License could not be processed *)
  | License_checkout_error of string
  (** License could not be checked out *)
  | Missing_connection_details
  (** Thrown if connection port or address parameter not supplied to check_license *)
  | V6d_failure
  (** Daemon failed to enable features *)
[@@default V6d_failure]
[@@deriving rpcty]

(** Pass error variant as exn for error handler *)
exception V6_error of errors
[@@deriving rpcty]

(** handle exception generation and raising *)
let err = Error.{
    def = errors;
    raiser = (function
        | e -> raise (V6_error e));
    matcher = (function
      | V6_error e -> Some e
      | _ -> None)
  }


(** functor to autogenerate code using PPX *)
module RPC_API(R : RPC) = struct
  open R

  (* description of V6d interface module *)
  let description = Interface.{
      name = "Licensing";
      namespace = None;
      description =
        [
          "This interface is used by Xapi and V6d to manage "
        ; "enabling and disabling features."
        ];
      version=(1,0,0);        }

  (* define implementation *)
  let implementation = implement description

  (* define general parameters for API calls *)
  let debug_info_p = Param.mk ~description:[
      "An uninterpreted string to associate with the operation."
    ] debug_info

  (* ---- API call definitions ---- *)

  let apply_edition =
    let edition_p = Param.mk ~description:["Edition title"] Types.string in
    let edition_info_p = Param.mk ~description:["Edition info"] edition_info in
    let current_params_p = Param.mk ~description:["Xapi parameters"] string_pair_lst in
    declare "apply_edition"
      ["Checks license info to ensures enabled features are compatible."]
      ( debug_info_p @-> edition_p @-> current_params_p @-> returning edition_info_p err )

  let get_editions =
    let edition_list_p = Param.mk ~description:["List of editions"] edition_list in
    declare "get_editions"
      ["Gets list of accepted editions."]
      ( debug_info_p @-> returning edition_list_p err )

  let get_version =
    let result_p = Param.mk ~description:["String of version."] Types.string in
    declare "get_version"
      ["Returns version"]
      ( debug_info_p @-> returning result_p err )

end
