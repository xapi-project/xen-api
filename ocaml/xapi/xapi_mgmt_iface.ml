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
open Db_filter_types
open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_threads.Threadext
module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "xapi_mgmt_iface" end)

open D
module Addresses = Set.Make (String)

let update_mh_info interface =
  let (_ : string * string) =
    Forkhelpers.execute_command_get_output
      !Xapi_globs.update_mh_info_script
      [interface]
  in
  ()

module Server : sig
  type listening_mode = Off | Any | Local of Addresses.t

  val update : __context:Context.t -> listening_mode -> unit

  val current_mode : unit -> listening_mode

  val is_ipv6_enabled : unit -> bool
end = struct
  (* Keep track of the management interface server thread.
     Stores a key into the table in Http_srv which identifies the server thread bound
     to the management IP. *)
  let management_servers = ref []

  let ipv6_enabled = ref false

  let stop () =
    debug "Shutting down the old management interface (if any)" ;
    List.iter (fun i -> Http_svr.stop i) !management_servers ;
    management_servers := []

  (* Even if xapi listens on all IP addresses, there is still an interface appointed as
     _the_ management interface. Hosts in a pool use the IP addresses of this interface
     to communicate with each other. *)
  let start ~__context ?addr () =
    let socket =
      match addr with
      | None -> (
          info "Starting new server (listening on all IP addresses)" ;
          try
            (* Is it IPv6 ? *)
            let addr = Unix.inet6_addr_any in
            let socket =
              Xapi_http.bind (Unix.ADDR_INET (addr, Constants.http_port))
            in
            ipv6_enabled := true ;
            socket
          with _ ->
            (* No. *)
            let addr = Unix.inet_addr_any in
            let socket =
              Xapi_http.bind (Unix.ADDR_INET (addr, Constants.http_port))
            in
            ipv6_enabled := false ;
            socket
        )
      | Some ip ->
          info "Starting new server (listening on %s)" ip ;
          let addr = Unix.inet_addr_of_string ip in
          let sockaddr = Unix.ADDR_INET (addr, Constants.http_port) in
          Xapi_http.bind sockaddr
    in
    Http_svr.start Xapi_http.server socket ;
    management_servers := socket :: !management_servers ;
    if Pool_role.is_master () && addr = None then
      (* NB if we synchronously bring up the management interface on a master with a blank
         database this can fail... this is ok because the database will be synchronised later *)
      Server_helpers.exec_with_new_task "refreshing consoles" (fun __context ->
          Dbsync_master.set_master_ip ~__context ;
          Dbsync_master.refresh_console_urls ~__context
      )

  type listening_mode = Off | Any | Local of Addresses.t

  (* This is an idempotent function that leaves servers running if they do not
     require any changes. *)
  let update' ~__context = function
    | current, next when current = next ->
        ()
    | _, Off ->
        stop ()
    | _, Any ->
        stop () ; start ~__context ()
    | Local old_addresses, Local new_addresses ->
        if not (Addresses.subset old_addresses new_addresses) then
          stop () ;
        let to_start = Addresses.diff new_addresses old_addresses in
        Addresses.iter (fun addr -> start ~__context ~addr ()) to_start
    | _, Local addresses ->
        stop () ;
        Addresses.iter (fun addr -> start ~__context ~addr ()) addresses

  let mode = ref Off

  let update ~__context next_mode =
    update' ~__context (!mode, next_mode) ;
    mode := next_mode

  let current_mode () = !mode

  let is_ipv6_enabled () = !ipv6_enabled
end

module Client_certificate_auth_server = struct
  let management_server = ref None

  let must_be_running ~__context ~mgmt_enabled =
    let pool = Helpers.get_pool ~__context in
    mgmt_enabled
    && Pool_role.is_master ()
    && Db.Pool.get_client_certificate_auth_enabled ~__context ~self:pool

  let configure_port cmd =
    let cmd' = if cmd then "open" else "close" in
    let _ =
      Helpers.call_script
        !Xapi_globs.firewall_port_config_script
        [cmd'; string_of_int Constants.https_port_clientcert]
    in
    ()

  let start () =
    if !management_server = None then (
      let sock_path = Xapi_globs.unix_domain_socket_clientcert in
      Unixext.mkdir_safe (Filename.dirname sock_path) 0o700 ;
      Unixext.unlink_safe sock_path ;
      let domain_sock = Xapi_http.bind (Unix.ADDR_UNIX sock_path) in
      Http_svr.start Xapi_http.server domain_sock ;
      configure_port true ;
      management_server := Some domain_sock
    )

  let stop () =
    Option.iter Http_svr.stop !management_server ;
    configure_port false ;
    management_server := None

  let update ~__context ~mgmt_enabled =
    if must_be_running ~__context ~mgmt_enabled then
      start ()
    else
      stop ()
end

(* High-level interface *)

let change interface primary_address_type =
  Xapi_inventory.update Xapi_inventory._management_interface interface ;
  Xapi_inventory.update Xapi_inventory._management_address_type
    (Record_util.primary_address_type_to_string primary_address_type) ;
  update_mh_info interface

let himn = ref None

let management_m = Mutex.create ()

let next_server_mode ~mgmt_enabled =
  let localhost = "127.0.0.1" in
  match (mgmt_enabled, !himn) with
  | true, _ ->
      Server.Any
  | false, Some himn ->
      Server.Local (Addresses.of_list [localhost; himn])
  | false, None ->
      Server.Local (Addresses.of_list [localhost])

let mgmt_is_enabled () = Server.current_mode () = Any

let run ~__context ?mgmt_enabled () =
  let mgmt_enabled = Option.value ~default:(mgmt_is_enabled ()) mgmt_enabled in
  Mutex.execute management_m (fun () ->
      Client_certificate_auth_server.update ~__context ~mgmt_enabled ;
      next_server_mode ~mgmt_enabled |> Server.update ~__context ;
      Xapi_stunnel_server.restart ~__context (Server.is_ipv6_enabled ()) ;
  )

let reconfigure_himn ~__context ~addr =
  Mutex.execute management_m (fun () ->
      himn := addr ;
      next_server_mode ~mgmt_enabled:(mgmt_is_enabled ())
      |> Server.update ~__context
  )

let himn_addr () = !himn

let ip_mutex = Mutex.create ()

let ip_cond = Condition.create ()

let wait_for_ip get_ip is_connected =
  let rec loop () =
    let ip = match get_ip () with Some x -> x | None -> "" in
    let connected = is_connected () in
    if ip = "" || not connected then (
      debug "wait_for_ip (IP=%s, connected:%b), waiting" ip connected ;
      Condition.wait ip_cond ip_mutex ;
      loop ()
    ) else
      ip
  in
  Mutex.execute ip_mutex loop

let wait_for_management_ip ~__context =
  let get_ip () = Helpers.get_management_ip_addr ~__context in
  let is_connected () = Helpers.get_management_iface_is_connected ~__context in
  wait_for_ip get_ip is_connected

(* CA-280237: Called in startup sequence after creating cluster_hosts *)
let wait_for_clustering_ip ~__context ~(self : API.ref_Cluster_host) =
  let pIF = Db.Cluster_host.get_PIF ~__context ~self in
  let network = Db.PIF.get_network ~__context ~self:pIF in
  let bridge = Db.Network.get_bridge ~__context ~self:network in
  debug "Waiting for clustering IP on bridge %s (%s)" bridge (Ref.string_of pIF) ;
  let get_ip () =
    match Db.PIF.get_IP ~__context ~self:pIF with "" -> None | s -> Some s
  in
  let is_connected () = Helpers.get_bridge_is_connected ~__context bridge in
  wait_for_ip get_ip is_connected

let on_dom0_networking_change ~__context =
  debug "Checking to see if hostname or management IP has changed" ;
  Helpers.update_pif_addresses ~__context ;
  (* Need to update:
     	   1 Host.hostname
     	   2 Host.address
     	   3. Console URIs *)
  let new_hostname = Networking_info.get_hostname () in
  let localhost = Helpers.get_localhost ~__context in
  if Db.Host.get_hostname ~__context ~self:localhost <> new_hostname then (
    debug "Changing Host.hostname in database to: %s" new_hostname ;
    Db.Host.set_hostname ~__context ~self:localhost ~value:new_hostname
  ) ;
  if
    List.mem
      (Db.Host.get_name_label ~__context ~self:localhost)
      ["localhost"; "localhost.localdomain"]
  then
    Db.Host.set_name_label ~__context ~self:localhost ~value:new_hostname ;
  ( match Helpers.get_management_ip_addr ~__context with
  | Some ip ->
      (* WARNING: this does NOT detect IP address changes that happen before
         xapi's startup (see CA-242706) *)
      if Db.Host.get_address ~__context ~self:localhost <> ip then (
        debug "Changing Host.address in database to: %s" ip ;
        Db.Host.set_address ~__context ~self:localhost ~value:ip ;
        debug "Refreshing console URIs" ;
        Helpers.update_getty () ;
        Dbsync_master.refresh_console_urls ~__context
      )
  | None ->
      if Db.Host.get_address ~__context ~self:localhost <> "" then (
        debug
          "Changing Host.address in database to: '' (host has no management IP \
           address)" ;
        Helpers.update_getty () ;
        Db.Host.set_address ~__context ~self:localhost ~value:""
      )
  ) ;
  Helpers.update_domain_zero_name ~__context localhost new_hostname ;
  debug "Signalling anyone waiting for the management IP address to change" ;
  Mutex.execute ip_mutex (fun () -> Condition.broadcast ip_cond)
