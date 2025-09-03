(*
 * Copyright (c) Cloud Software Group, Inc.
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

module D = Debug.Make (struct let name = __MODULE__ end)

open D

type service_type = Dlm | Nbd | Ssh | Vxlan | Http | Xenha

type status = Enabled | Disabled

type protocol = TCP | UDP

(* 1. For firewalld, xapi is always responsible for dynamically controlling
      firewalld services.
   2. For legacy iptables compatibility:
      - Certain iptables ports (such as 4789 for VXLAN) are managed dynamically.
      - Other ports (such as 22 for SSH) do not require dynamic control, as they
        are already enabled when the host boots up.
      - The `dynamic_control_iptables_port` parameter is used to decide if the
        ports should be controlled dynamically when iptables is selected.
*)
type service_info = {
    name: string
  ; port: int
  ; protocol: protocol
  ; dynamic_control_iptables_port: bool
}

let status_to_string = function Enabled -> "enabled" | Disabled -> "disabled"

let protocol_to_string = function TCP -> "tcp" | UDP -> "udp"

let service_type_to_service_info = function
  | Dlm ->
      {
        name= "dlm"
      ; port= !Xapi_globs.xapi_clusterd_port
      ; protocol= TCP
      ; dynamic_control_iptables_port= true
      }
  | Nbd ->
      {
        name= "nbd"
      ; port= 10809
      ; protocol= TCP
      ; dynamic_control_iptables_port= true
      }
  | Ssh ->
      {
        name= "ssh"
      ; port= 22
      ; protocol= TCP
      ; dynamic_control_iptables_port= false
      }
  | Vxlan ->
      {
        name= "vxlan"
      ; port= 4789
      ; protocol= UDP
      ; dynamic_control_iptables_port= true
      }
  | Http ->
      {
        name= "xapi-insecure"
      ; port= Constants.http_port
      ; protocol= TCP
      ; dynamic_control_iptables_port= true
      }
  | Xenha ->
      {
        name= "xenha"
      ; port= Xapi_globs.xha_udp_port
      ; protocol= UDP
      ; dynamic_control_iptables_port= false
      }

module type FIREWALL = sig
  val update_firewall_status : service:service_type -> status:status -> unit
end

module Firewalld : FIREWALL = struct
  let update_firewall_status ~service ~status =
    if !Xapi_globs.dynamic_control_firewalld_service then (
      let service_option =
        match status with
        | Enabled ->
            "--add-service"
        | Disabled ->
            "--remove-service"
      in
      let service_info = service_type_to_service_info service in
      try
        Helpers.call_script !Xapi_globs.firewall_cmd
          [service_option; service_info.name]
        |> ignore
      with e ->
        error
          "%s: Failed to update firewall service (%s) to (%s) with error: %s"
          __FUNCTION__ service_info.name (status_to_string status)
          (Printexc.to_string e) ;
        Helpers.internal_error "Failed to update firewall service (%s)"
          service_info.name
    )
end

module Iptables : FIREWALL = struct
  let update_firewall_status ~service ~status =
    let op = match status with Enabled -> "open" | Disabled -> "close" in
    let service_info = service_type_to_service_info service in
    if service_info.dynamic_control_iptables_port then (
      try
        Helpers.call_script
          !Xapi_globs.firewall_port_config_script
          [
            op
          ; string_of_int service_info.port
          ; protocol_to_string service_info.protocol
          ]
        |> ignore
      with e ->
        error
          "%s: Failed to update firewall service (%s) to (%s) with error: %s"
          __FUNCTION__ service_info.name (status_to_string status)
          (Printexc.to_string e) ;
        Helpers.internal_error "Failed to update firewall service (%s)"
          service_info.name
    )
end

let firewall_provider (backend : Xapi_globs.firewall_backend_type) :
    (module FIREWALL) =
  match backend with
  | Firewalld ->
      (module Firewalld)
  | Iptables ->
      (module Iptables)
