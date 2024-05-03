(*
 * Copyright (C) Cloud Software Group, Inc
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

(* This is a CLI tool called by xenopsd to set up OVS rules for PVS. It
   replaces a bash script setup-pvs-proxy-rules used previously and has
   a compatible interface *)

let name = "pvs-proxy-ovs-setup"

module C = Cmdliner

module D = Debug.Make (struct
  let pid = Unix.getpid ()

  let name = Printf.sprintf "%s[%d]" name pid
end)
(* using __MODULE__ leads to a convoluted long name, so avoiding it *)

module XS = Ezxenstore_core.Xenstore

let error fmt =
  Printf.kprintf
    (fun msg ->
      D.error "%s" msg ;
      Result.error (`Msg msg)
    )
    fmt

let ok x = Result.ok x

let ( let* ) = Result.bind

let ( >>= ) = Result.bind

let ( // ) = Filename.concat

let finally f always = Fun.protect ~finally:always f

let to_int str =
  try Ok (int_of_string str) with _ -> error "not an number: %s" str

(** iterate over a list, appyling [f] to each element *)
let rec iter f = function
  | [] ->
      Ok ()
  | x :: xs ->
      let* () = f x in
      iter f xs

(* iterate from lo to hi (inclusive), calling [f] *)
let seq lo hi f =
  let rec loop = function
    | i when i > hi ->
        Ok ()
    | i ->
        let* () = f i in
        loop (i + 1)
  in
  loop lo

let ip_domain str =
  try
    let addr = Unix.inet_addr_of_string str in
    ok (Unix.domain_of_sockaddr (Unix.ADDR_INET (addr, 1)))
  with _e -> error "not an IPv4 or IPv6 address: %s" str

let is_ipv6 str =
  let* domain = ip_domain str in
  match domain with
  | Unix.PF_INET6 ->
      ok true
  | Unix.PF_INET ->
      ok false
  | Unix.PF_UNIX ->
      error "unexpected address format: %s" str

let ( let+ ) x f = Result.map f x

let is_ipv4 str =
  let+ res = is_ipv6 str in
  not res

(** List.exits but with [p] returning a [Result.t] *)
let rec exists p = function
  | [] ->
      ok false
  | x :: xs ->
      let* res = p x in
      if res then ok true else exists p xs

(** open [path] to be consumed by [f] for scanning with Scanf *)
let scanning path f =
  let io = Scanf.Scanning.open_in path in
  finally (fun () -> f io) (fun () -> Scanf.Scanning.close_in io)

module Device = struct
  type t = VIF | TAP

  let to_string t = match t with VIF -> "vif" | TAP -> "tap"
end

(** read from xenstore *)
let read path =
  try Ok (XS.with_xs (fun xs -> xs.XS.read path))
  with e ->
    let msg = Printexc.to_string e in
    error "%s: can't read %s: %s" __FUNCTION__ path msg

(** directory listing from xenstore *)
let dir path =
  try Ok (XS.with_xs @@ fun xs -> xs.XS.directory path)
  with e ->
    let msg = Printexc.to_string e in
    error "%s: can't read %s: %s" __FUNCTION__ path msg

(** write to xenstore *)
let write path value =
  try Ok (XS.with_xs @@ fun xs -> xs.XS.write path value)
  with e ->
    let msg = Printexc.to_string e in
    error "%s: can't write %s: %s" __FUNCTION__ path msg

(* remove path from xenstore *)
let rm path =
  try Ok (XS.with_xs @@ fun xs -> xs.XS.rm path)
  with e ->
    let msg = Printexc.to_string e in
    error "%s: can't remove %s: %s" __FUNCTION__ path msg

(** returns Ok () if PVS is running for the given VIF UUID *)
let pvs_is_running vif_uuid =
  let prefix = "/xapi/pvs-proxy" in
  let* sites = dir prefix in
  let started vif_uuid site =
    let* state = read (prefix // site // vif_uuid // "state") in
    ok (String.equal state "started")
  in
  sites
  |> List.map (started vif_uuid)
  |> List.exists (function Ok true -> true | _ -> false)
  |> function
  | false ->
      error "%s: PVS proxy is not configured for %s" __FUNCTION__ vif_uuid
  | true ->
      ok ()

(** addresses of PVS server [n] *)
let pvs_server_addrs ~n priv =
  let key = Printf.sprintf "pvs-server-%d-addresses" n in
  let path = priv // key in
  let* str = read path in
  String.split_on_char ',' str |> List.map String.trim |> ok
(* we could convert this to an internal IP representation *)

(** ports of PVS server [n] *)
let pvs_server_ports ~n priv =
  let key = Printf.sprintf "pvs-server-%d-ports" n in
  let path = priv // key in
  let* str = read path in
  try Scanf.sscanf str "%d-%d" (fun lo hi -> ok (lo, hi))
  with _e -> error "%s: can't parse %s as two ports" __FUNCTION__ str

module OVS = struct
  open Bos
  open Rresult.R.Infix

  let vsctl = Cmd.v "/usr/bin/ovs-vsctl"

  let ofctl = Cmd.v "/usr/bin/ovs-ofctl"

  let echo = Cmd.v "/usr/bin/echo"

  (* Patterns for IPv4/IPv6 *)

  let ip addr =
    is_ipv6 addr >>= function
    | true ->
        Printf.sprintf "ipv6" |> ok
    | false ->
        Printf.sprintf "ip" |> ok

  let udp addr =
    is_ipv6 addr >>= function
    | true ->
        Printf.sprintf "udp6" |> ok
    | false ->
        Printf.sprintf "udp" |> ok

  let nw_src addr =
    is_ipv6 addr >>= function
    | true ->
        Printf.sprintf "ipv6_src=%s" addr |> ok
    | false ->
        Printf.sprintf "ip_src=%s" addr |> ok

  let nw_dst addr =
    is_ipv6 addr >>= function
    | true ->
        Printf.sprintf "ipv6_dst=%s" addr |> ok
    | false ->
        Printf.sprintf "ip_dst=%s" addr |> ok

  let run cmd =
    D.info "%s: %s" __FUNCTION__ (Cmd.to_string cmd) ;
    OS.Cmd.(run_out cmd |> to_string)

  let pvs_bridge br =
    let cmd = Cmd.(vsctl % "br-to-parent" % br) in
    let* out = run cmd in
    try Scanf.sscanf out "%s" ok
    with _e ->
      error "%s: failed to scan first word from '%s'" __FUNCTION__ out

  (** the mac address returned by [cmd] is inside double quotes and we
      return it without those. *)
  let mac_addr intf =
    let cmd = Cmd.(vsctl % "get" % "interface" % intf % "mac_in_use") in
    let* out = run cmd in
    try Scanf.sscanf out {|"%s@"|} ok
    with _e -> error "%s: failed to scan address from '%s'" __FUNCTION__ out

  let ofport intf =
    let cmd = Cmd.(vsctl % "get" % "interface" % intf % "ofport") in
    let* out = run cmd in
    try Scanf.sscanf out {|%d|} ok
    with _e -> error "%s: failed to scan port from '%s'" __FUNCTION__ out

  let add_flow ?(debug = false) bridge flow =
    let flow' = String.concat "," flow in
    let cmd =
      match debug with
      | true ->
          Cmd.(echo % bridge % flow')
      | false ->
          Cmd.(ofctl % "--strict" % "add-flow" % bridge % flow')
    in
    D.info "%s: %s" __FUNCTION__ (Cmd.to_string cmd) ;
    OS.Cmd.(run_out cmd |> to_stdout)

  let del_flows bridge extra =
    let args = "del-flows" :: bridge :: extra |> List.map Cmd.v in
    let cmd = Cmd.(List.fold_left add_args ofctl args) in
    D.info "%s: %s" __FUNCTION__ (Cmd.to_string cmd) ;
    OS.Cmd.(run_out cmd |> to_stdout)
end

(** 16bit msb and lsb of the index *)
let interface_id vif =
  let path = "/sys/class/net" // vif // "ifindex" in
  let ( >> ) = Int64.shift_right in
  let ( & ) = Int64.logand in
  try
    scanning path @@ fun io ->
    Scanf.bscanf io {|%Ld|} @@ fun id ->
    (* this is what the original code does but it looks suspicious: it
       takes the lower 16 bit and upper 32 bit of id *)
    ok Int64.(id, id >> 32 |> to_int, (id & 0xffffL) |> to_int)
  with _ -> error "failed to scan %s" path

module Const = struct
  let rule_prio = 1000

  let client2server = 1

  let proxy2client = 2

  let server2client = 3

  let port_table = 101

  let action_table = 102

  let dir = "reg0"

  let client_msb = "reg1"

  let client_lsb = "reg2"

  let nxm_dir = "NXM_NX_REG0"

  let nxm_client_msb = "NXM_NX_REG1"

  let nxm_client_lsb = "NXM_NX_REG2"
end

(** implement the add command *)
let add debug dev vif priv hotplug =
  D.debug "%s: %s %s %s" __FUNCTION__ vif priv hotplug ;
  let p = Printf.sprintf in
  let* vif_uuid = read (priv // "vif-uuid") in
  let* () = pvs_is_running vif_uuid in
  let* pvs_proxy_if = read (priv // "pvs-interface") in
  let* mac = read (priv // "mac") in
  let* pvs_server_count = read (priv // "pvs-server-num") >>= to_int in
  let* bridge' = read (priv // "bridge") in
  let* pvs_bridge = OVS.pvs_bridge bridge' in
  D.debug "%s: pvs_bridge = %s" __FUNCTION__ pvs_bridge ;
  let* proxy_mac = OVS.mac_addr pvs_proxy_if in
  let* proxy_port = OVS.ofport pvs_proxy_if in
  let* vm_port = OVS.ofport vif in
  let* id, id_msb, id_lsb = interface_id vif in

  let msg =
    String.concat " "
      [
        "adding rules to"
      ; pvs_bridge
      ; "for proxy"
      ; p "%s (%d/%s)" pvs_proxy_if proxy_port proxy_mac
      ; "and VM"
      ; p "%s (%d/%s) id=0x%Lx" vif vm_port mac id
      ]
  in
  D.debug "%s: %s" __FUNCTION__ msg ;

  let add_pvs_server bridge n =
    D.debug "%s: bridge=%s server=%d" __FUNCTION__ bridge n ;
    let* ip_addrs = pvs_server_addrs ~n priv in
    let* has_ipv6 = exists is_ipv6 ip_addrs in
    let* has_ipv4 = exists is_ipv4 ip_addrs in
    let* port_lo, port_hi = pvs_server_ports ~n priv in
    let* () =
      (* flows per server address *)
      ip_addrs
      |> iter @@ fun ip ->
         let* nw_src = OVS.nw_src ip in
         let* nw_dst = OVS.nw_dst ip in
         let* udp = OVS.udp ip in
         let* ip = OVS.ip ip in
         let* () =
           (* Packets from proxied clients that have a PVS-server IP
              must be dropped. This is done separately for vif and tap
              interfaces by matching on the in_port. *)
           OVS.add_flow ~debug bridge
             [
               p "cookie=%Ld" id
             ; p "priority=%d" Const.rule_prio
             ; p "in_port=%d" vm_port
             ; ip
             ; nw_src
             ; "action=drop"
             ]
         in
         (* The following rules are independent of the in_port, so we'll
            need just one copy per VIF. We'll only apply them if the
            script is called for a vif interface, not for a tap
            interface, because tap interfaces are not always present,
            while vifs are. *)
         match dev with
         | Device.TAP ->
             ok ()
         | Device.VIF ->
             iter
               (OVS.add_flow ~debug bridge)
               [
                 [
                   p "cookie=%Ld" id
                 ; p "priority=%d" (Const.rule_prio - 1)
                 ; udp
                 ; p "dl_src=%s" mac
                 ; nw_dst
                 ; p "actions=load:%d->%s[]" id_lsb Const.nxm_client_lsb
                 ; p "load:%d->%s[]" id_msb Const.nxm_client_msb
                 ; p "load:%d->%s[]" Const.client2server Const.nxm_dir
                 ; p "resubmit(,%d)" Const.port_table
                 ]
               ; [
                   p "cookie=%Ld" id
                 ; p "priority=%d" Const.rule_prio
                 ; udp
                 ; p "dl_src=%s" proxy_mac
                 ; p "dl_dst=%s" mac
                 ; nw_src
                 ; p "actions=load:%d->%s[]" id_lsb Const.nxm_client_lsb
                 ; p "load:%d->%s[]" id_msb Const.nxm_client_msb
                 ; p "load:%d->%s[]" Const.proxy2client Const.nxm_dir
                 ; p "resubmit(,%d)" Const.port_table
                 ]
               ; [
                   p "cookie=%Ld" id
                 ; p "priority=%d" (Const.rule_prio - 1)
                 ; udp
                 ; p "dl_dst=%s" mac
                 ; nw_src
                 ; p "actions=load:%d->%s[]" id_lsb Const.nxm_client_lsb
                 ; p "load:%d->%s[]" id_msb Const.nxm_client_msb
                 ; p "load:%d->%s[]" Const.server2client Const.nxm_dir
                 ; p "resubmit(,%d)" Const.port_table
                 ]
               ]
    in
    (* The following rules are independent of the in_port, so we'll
       need just one copy per VIF. We'll only apply them if the script is
       called for a vif interface, not for a tap interface, because tap
       interfaces are not always present, while vifs are. *)
    match dev with
    | Device.TAP ->
        ok ()
    | Device.VIF ->
        let* () =
          seq port_lo port_hi (fun port ->
              (* flows per port *)
              let* () =
                if has_ipv4 then
                  OVS.add_flow ~debug bridge
                    [
                      p "cookie=%Ld" id
                    ; p "table=%d" Const.port_table
                    ; p "priority=%d" Const.rule_prio
                    ; p "%s=%d" Const.client_lsb id_lsb
                    ; p "%s=%d" Const.client_msb id_msb
                    ; "udp"
                    ; p "tp_dst=%d" port
                    ; p "actions=resubmit(,%d)" Const.action_table
                    ]
                else
                  ok ()
              in
              let* () =
                if has_ipv6 then
                  OVS.add_flow ~debug bridge
                    [
                      p "cookie=%Ld" id
                    ; p "table=%d" Const.port_table
                    ; p "priority=%d" Const.rule_prio
                    ; p "%s=%d" Const.client_lsb id_lsb
                    ; p "%s=%d" Const.client_msb id_msb
                    ; "udp6"
                    ; p "tp_dst=%d" port
                    ; p "actions=resubmit(,%d)" Const.action_table
                    ]
                else
                  ok ()
              in
              ok ()
          )
        in
        iter (* over flows below *)
          (OVS.add_flow ~debug bridge)
          [
            [
              p "cookie=%Ld" id
            ; p "table=%d" Const.action_table
            ; p "priority=%d" (Const.rule_prio - 1)
            ; p "%s=%d" Const.client_lsb id_lsb
            ; p "%s=%d" Const.client_msb id_msb
            ; p "%s=%d" Const.dir Const.client2server
            ; p "actions=%d" proxy_port
            ]
          ; [
              p "cookie=%Ld" id
            ; p "table=%d" Const.action_table
            ; p "priority=%d" Const.rule_prio
            ; p "%s=%d" Const.client_lsb id_lsb
            ; p "%s=%d" Const.client_msb id_msb
            ; p "%s=%d" Const.dir Const.proxy2client
            ; "actions=NORMAL"
            ]
          ; [
              p "cookie=%Ld" id
            ; p "table=%d" Const.action_table
            ; p "priority=%d" (Const.rule_prio - 1)
            ; p "%s=%d" Const.client_lsb id_lsb
            ; p "%s=%d" Const.client_msb id_msb
            ; p "%s=%d" Const.dir Const.server2client
            ; p "actions=%d" proxy_port
            ]
          ]
  in

  (* now add flows *)
  let* () =
    iter (* over flows below *)
      (OVS.add_flow ~debug pvs_bridge)
      [
        ["priority=0"; p "table=%d" Const.port_table; "actions=NORMAL"]
      ; ["priority=0"; p "table=%d" Const.action_table; "actions=NORMAL"]
      ]
  in
  (* add flows for each server 0..n *)
  let* () = seq 0 (pvs_server_count - 1) (add_pvs_server pvs_bridge) in
  (* announce that we are ready *)
  let* () =
    let path = hotplug // "pvs-rules-active" in
    match debug with
    | true ->
        p "setup complete: %s" path |> print_endline ;
        ok ()
    | false ->
        write path ""
  in
  ok ()

let remove dev vif priv hotplug =
  D.debug "%s: %s %s %s" __FUNCTION__ vif priv hotplug ;
  let p = Printf.sprintf in
  let* vif_uuid = read (priv // "vif-uuid") in
  let* () = pvs_is_running vif_uuid in
  let* bridge = read (priv // "bridge") in
  let* pvs_bridge = OVS.pvs_bridge bridge in
  let path = hotplug // p "%s-ifindex" (Device.to_string dev) in
  let* str = read path in
  let* intf =
    try Scanf.sscanf str "%d" ok
    with _e -> error "%s: can't parse %s as a number" __FUNCTION__ str
  in
  let* () = OVS.del_flows pvs_bridge [p "cookie=%d/-1" intf] in
  match dev with TAP -> ok () | VIF -> rm (hotplug // "pvs-rules-active")

let reset _dev vif priv hotplug =
  D.debug "%s: %s %s %s" __FUNCTION__ vif priv hotplug ;
  let* vif_uuid = read (priv // "vif-uuid") in
  let* () = pvs_is_running vif_uuid in
  let* bridge = read (priv // "bridge") in
  let* pvs_bridge = OVS.pvs_bridge bridge in
  let* () = OVS.del_flows pvs_bridge [] in
  OVS.add_flow pvs_bridge ["priority=0"; "actions=NORMAL"]

(* Command line parsing is delegated to Cmdliner and confined to the CLI
   module. *)
module CLI = struct
  let build = Xapi_version.version

  let man =
    [
      `P "These options are common to all commands."
    ; `S "MORE HELP"
    ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
    ; `S "BUILD DETAILS"
    ; `P build
    ]

  (** debug flag for development *)
  let debug =
    let doc = "Emit to stdout; don't change anything" in
    C.Arg.(value & flag & info ["debug"; "d"] ~docv:"DEBUG" ~doc)

  (** device type argument *)
  let device =
    let dev = C.Arg.enum [("vif", Device.VIF); ("tap", Device.TAP)] in
    C.Arg.(
      required
      & pos 0 (some dev) None
      & info [] ~docv:"vif|tap" ~doc:"Device type: vif|tap"
    )

  (** device name, a string *)
  let device_name =
    C.Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"vif4.0" ~doc:"Device name, e.g. vif4.0"
    )

  (** xenstore path *)
  let private_path =
    C.Arg.(
      required
      & pos 2 (some string) None
      & info [] ~docv:"PATH" ~doc:"private xenstore path"
    )

  (** xenstore path *)
  let hotplug_path =
    C.Arg.(
      required
      & pos 3 (some string) None
      & info [] ~docv:"HOTPLUG" ~doc:"hotplug xenstore path"
    )

  (** CLI sub command with args *)
  let add =
    let doc = "add OVS rules for Virtual Interface (VIF) or TAP" in
    let man =
      [
        `S C.Manpage.s_description
      ; `P "Add OVS rules for an interface to use the PVS Proxy."
      ; `Blocks man
      ]
    in
    let info = C.Cmd.info "add" ~doc ~man in
    C.(
      Cmd.v info
        Term.(
          term_result
            (const add
            $ debug
            $ device
            $ device_name
            $ private_path
            $ hotplug_path
            )
        )
    )

  (** CLI sub command with args *)
  let remove =
    let doc = "remove OVS rules for a Virtual Interface (VIF) or TAP" in
    let man =
      [
        `S C.Manpage.s_description
      ; `P
          "Remove OVS rules for an interface such that it no longer uses the \
           PVS proxy."
      ; `Blocks man
      ]
    in
    let info = C.Cmd.info "remove" ~doc ~man in
    C.(
      Cmd.v info
        Term.(
          term_result
            (const remove $ device $ device_name $ private_path $ hotplug_path)
        )
    )

  (** CLI sub command with args *)
  let reset =
    let doc = "remove OVS rules for all Virtual Interface (VIF) or TAP" in
    let man =
      [
        `S C.Manpage.s_description
      ; `P "Remove OVS rules for any interface."
      ; `Blocks man
      ]
    in
    let info = C.Cmd.info "reset" ~doc ~man in
    C.(
      Cmd.v info
        Term.(
          term_result
            (const reset $ device $ device_name $ private_path $ hotplug_path)
        )
    )

  (** sub commands; each sub command can take different arguments *)
  let cmds = [add; remove; reset]

  let cmd =
    let help = `Help (`Pager, None) in
    let doc = "set up OVS rules for PVS proxy" in
    let info = C.Cmd.info name ~doc ~man in
    let default = C.Term.(ret @@ const help) in
    C.Cmd.group info ~default cmds
end

(* Program entry is below and starts with command line parsing, which
   invokes the command implementation *)
let main () =
  let this = Sys.argv.(0) in
  Debug.set_facility Syslog.User ;
  D.info "%s %s" this CLI.build ;
  C.Cmd.eval CLI.cmd

let () = if !Sys.interactive then () else main () |> exit
