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
open Stdext
open Pervasiveext
open Threadext

module D = Debug.Make (struct let name = "xapi_mgmt_iface" end)

open D

(** Keep track of the management interface server thread *)

let himn_addr = ref None

(* Stores a key into the table in Http_srv which identifies the server thread bound
   	 to the management IP. *)
let management_interface_server = ref []

let listening_all = ref false

let listening_localhost = ref false

let listening_himn = ref false

let management_m = Mutex.create ()

let stunnel_accept_m = Mutex.create ()

let stunnel_accept = ref None

let update_mh_info interface =
  let (_ : string * string) =
    Forkhelpers.execute_command_get_output
      !Xapi_globs.update_mh_info_script
      [interface]
  in
  ()

let _restart_stunnel_no_cache ~__context ~accept =
  let (_ : Thread.t) =
    Thread.create (fun () -> Helpers.Stunnel.restart ~__context ~accept) ()
  in
  ()

let restart_stunnel ~__context ~accept =
  info "Restarting stunnel (accepting connections on %s)" accept ;
  (* cache `accept` so client can call `reconfigure_stunnel` easily *)
  Mutex.execute stunnel_accept_m (fun () -> stunnel_accept := Some accept) ;
  _restart_stunnel_no_cache ~__context ~accept

let reconfigure_stunnel ~__context =
  let f =
    Mutex.execute stunnel_accept_m (fun () ->
        match !stunnel_accept with
        | None ->
            fun () ->
              D.warn
                "reconfigure_stunnel: accept is not set, so not restarting \
                 stunnel"
        | Some accept ->
            fun () -> _restart_stunnel_no_cache ~__context ~accept)
  in
  f ()

let stop () =
  debug "Shutting down the old management interface (if any)" ;
  List.iter (fun i -> Http_svr.stop i) !management_interface_server ;
  management_interface_server := [] ;
  listening_all := false ;
  listening_localhost := false ;
  listening_himn := false

(* Even though xapi listens on all IP addresses, there is still an interface appointed as
 * _the_ management interface. Slaves in a pool use the IP address of this interface to connect
 * the pool master. *)
let start ~__context ?addr () =
  let socket, accept =
    match addr with
    | None -> (
        info "Starting new server (listening on all IP addresses)" ;
        try
          (* Is it IPv6 ? *)
          let addr = Unix.inet6_addr_any in
          (Xapi_http.bind (Unix.ADDR_INET (addr, Constants.http_port)), ":::443")
        with _ ->
          (* No. *)
          let addr = Unix.inet_addr_any in
          (Xapi_http.bind (Unix.ADDR_INET (addr, Constants.http_port)), "443")
      )
    | Some ip -> (
        info "Starting new server (listening on %s)" ip ;
        let addr = Unix.inet_addr_of_string ip in
        let sockaddr = Unix.ADDR_INET (addr, Constants.http_port) in
        ( Xapi_http.bind sockaddr
        , match Unix.domain_of_sockaddr sockaddr with
          | Unix.PF_INET6 ->
              "::1:443"
          | _ ->
              "127.0.0.1:443" )
      )
  in
  Http_svr.start Xapi_http.server socket ;
  management_interface_server := socket :: !management_interface_server ;
  restart_stunnel ~__context ~accept ;
  if Pool_role.is_master () && !listening_all then
    (* NB if we synchronously bring up the management interface on a master with a blank
       		   database this can fail... this is ok because the database will be synchronised later *)
    Server_helpers.exec_with_new_task "refreshing consoles" (fun __context ->
        Dbsync_master.set_master_ip ~__context ;
        Dbsync_master.refresh_console_urls ~__context)

let change interface primary_address_type =
  Xapi_inventory.update Xapi_inventory._management_interface interface ;
  Xapi_inventory.update Xapi_inventory._management_address_type
    (Record_util.primary_address_type_to_string primary_address_type) ;
  update_mh_info interface

let run ~__context ~mgmt_enabled =
  Mutex.execute management_m (fun () ->
      if mgmt_enabled then (
        if not !listening_all then (
          stop () ;
          start ~__context () ;
          listening_all := true
        )
      ) else (
        if !listening_all then
          stop () ;
        if not !listening_localhost then (
          start ~__context ~addr:"127.0.0.1" () ;
          listening_localhost := true
        ) ;
        Option.iter
          (fun addr ->
            if not !listening_himn then (
              start ~__context ~addr () ;
              listening_himn := true
            ))
          !himn_addr
      ))

let enable_himn ~__context ~addr =
  Mutex.execute management_m (fun () -> himn_addr := Some addr) ;
  run ~__context ~mgmt_enabled:!listening_all

let rebind ~__context = run ~__context ~mgmt_enabled:!listening_all

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
  let new_hostname = Helpers.reget_hostname () in
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
