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
 * @group Memory
*)
open Rpc
open Idl

let service_name = "memory"
let queue_name = Xcp_service.common_prefix ^ service_name
let json_path = "/var/xapi/memory.json"
let xml_path = "/var/xapi/memory"

type reservation_id =
  string [@@doc [
  "The reservation_id is an opaque identifier associated with a block of ";
  "memory. It is used to reserve memory for a domain before the domain has ";
  "been created.";
]]
[@@deriving rpcty]

type domain_zero_policy =
  | Fixed_size of int64 [@doc ["Never balloon, use the specified fixed size"]]
  | Auto_balloon of int64 * int64 [@doc ["Balloon between the two sizes specified"]]
[@@doc ["Domain zero can have a different policy to that used by guest domains."]]
[@@deriving rpcty]

type errors =
  | Cannot_free_this_much_memory of (int64 * int64)
        [@doc [
          "[Cannot_free_this_much_memory (required, free)] is reported if it is not ";
          "possible to free [required] kib. [free] is the amount of memory currently free"]]
  | Domains_refused_to_cooperate of (int list)
        [@doc [
          "[Domains_refused_to_cooperate (domid list)] is reported if a set of domains do ";
          "not respond in a timely manner to the request to balloon. The uncooperative ";
          "domain ids are returned."]]
  | Unknown_reservation of (reservation_id)
        [@doc [
          "[Unknown_reservation (id)] is reported if a the specified reservation_id is ";
          "unknown."
        ]]
  | No_reservation
      [@doc [
        "[No_reservation] is reported by [query_reservation_of_domain] if the domain ";
        "does not have a reservation."
      ]]
  | Invalid_memory_value of (int64)
        [@doc [
          "[Invalid_memory_value (value)] is reported if a memory value passed is not ";
          "valid, e.g. negative."
        ]]
  | Internal_error of (string)
        [@doc [
          "[Internal_error (value)] is reported if an unexpected error is triggered ";
          "by the library."
        ]]
  | Unknown_error
      [@doc [
        "The default variant for forward compatibility."
      ]]
[@@default Unknown_error]
[@@deriving rpcty]

exception MemoryError of errors

let err = Error.{
    def = errors;
    raiser = (fun e -> raise (MemoryError e));
    matcher = (function
        | MemoryError e -> Some e
        | e -> Some (Internal_error (Printexc.to_string e)))
  }

type debug_info = string
[@@doc ["An uninterpreted string associated with the operation."]]
[@@deriving rpcty]

type session_id = string
[@@doc [
  "An identifier to associate requests with a client. This is ";
  "obtained by a call to [login]."
]]
[@@deriving rpcty]

type reserve_memory_range_result = reservation_id * int64
[@@deriving rpcty]

module API(R : RPC) = struct
  open R

  let description = Interface.{
      name = "Memory";
      namespace = None;
      description = [
        "This interface is used by Xapi and Squeezed to manage the ";
        "dynamic memory usage of VMs on a host.";
      ];
      version=(1,0,0);
    }

  let implementation = implement description

  (* General parameters, used by more than one API call *)

  let debug_info_p = Param.mk ~description:[
      "An uninterpreted string to associate with the operation."
    ] Types.string

  let diagnostics_result_p = Param.mk ~description:[
      "A string containing diagnostic information from the server."
    ] Types.string

  let service_name_p = Param.mk ~description:[
      "The name of the service attempting to interact with the squeeze daemon."
    ] Types.string

  let session_id_p = Param.mk ~description:[
      "An identifier to associate requests with a client. This is ";
      "obtained by a call to [login]."]
      Types.string

  let domid_p = Param.mk ~description:[
      "Domain id of a VM."
    ] Types.int

  let reservation_id_p = Param.mk ~description:[
      "The reservation_id is the token used to identify a memory allocation."
    ] reservation_id

  let size_p = Param.mk ~description:[
      "The size in bytes to reserve"]
      Types.int64

  let unit_p = Param.mk Types.unit

  (* Individual API calls *)

  let get_diagnostics = declare
      "get_diagnostics"
      ["Gets diagnostic information from the server"]
      (debug_info_p @-> returning diagnostics_result_p err)

  let login = declare
      "login"
      ["Logs into the squeeze daemon. Any reservations previously made with the ";
       "specified service name not yet associated with a domain will be removed."]
      (debug_info_p @-> service_name_p @-> returning session_id_p err)


  let reserve_memory = declare
      "reserve_memory"
      ["[reserve_memory dbg session size] reserves memory for a domain. If necessary, ";
       "other domains will be ballooned down to ensure [size] is available. The call ";
       "returns a reservation_id that can later be transferred to a domain."]
      (debug_info_p @-> session_id_p @-> size_p @-> returning reservation_id_p err)

  let reserve_memory_range =
    let result = Param.mk
        ~description:[
          "A tuple containing the reservation_id and the amount of memory actually reserved."
        ]
        reserve_memory_range_result
    in
    declare
      "reserve_memory_range"
      ["[reserve_memory_range dbg session min max] reserves memory for a domain. If necessary, ";
       "other domains will be ballooned down to ensure enough memory is available. The amount ";
       "of memory will be between [min] and [max] according to the policy in operation. The call ";
       "returns a reservation_id and the actual memory amount that can later be transferred to a domain."]
      (debug_info_p @-> session_id_p @-> size_p @-> size_p @-> returning result err)


  let delete_reservation =
    declare
      "delete_reservation"
      ["Deletes a reservation. Note that memory rebalancing is not done synchronously after the ";
       "operation has completed."]
      (debug_info_p @-> session_id_p @-> reservation_id_p @-> returning unit_p err)

  let transfer_reservation_to_domain =
    declare
      "transfer_reservation_to_domain"
      ["Transfers a reservation to a domain. This is called when the domain has been created for ";
       "the VM for which the reservation was initially made."]
      (debug_info_p @-> session_id_p @-> reservation_id_p @-> domid_p @-> returning unit_p err)

  let query_reservation_of_domain =
    declare
      "query_reservation_of_domain"
      ["Queries the reservation_id associated with a domain"]
      (debug_info_p @-> session_id_p @-> domid_p @-> returning reservation_id_p err)

  let balance_memory =
    declare
      "balance_memory"
      ["Forces a rebalance of the hosts memory. Blocks until the system is in a stable ";
       "state."]
      (debug_info_p @-> returning unit_p err)

  let get_host_reserved_memory =
    declare
      "get_host_reserved_memory"
      ["Gets the amount of reserved memory in a host. This is the lower limit of memory that ";
       "squeezed will ensure remains unused by any domain or reservation."]
      (debug_info_p @-> returning size_p err)

  let get_host_initial_free_memory =
    declare
      "get_host_initial_free_memory"
      ["Gets the amount of initial free memory in a host"]
      (debug_info_p @-> returning size_p err)

  let get_domain_zero_policy =
    let result_p = Param.mk ~description:["The policy associated with domain 0"] domain_zero_policy in
    declare
      "get_domain_zero_policy"
      ["Gets the ballooning policy for domain zero."]
      (debug_info_p @-> returning result_p err)

end
