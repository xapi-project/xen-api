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
open Xenops_utils

module D = Debug.Make (struct let name = "xenopsd" end)

open D

let name = "xenopsd"

let sockets_path = ref Xenops_interface.default_sockets_dir

let persist = ref true

let worker_pool_size = ref 4

let run_hotplug_scripts = ref true

let use_old_pci_add = ref false

let hotplug_timeout = ref 300.

let qemu_dm_ready_timeout = ref 300.

let vgpu_ready_timeout = ref 30.

let varstored_ready_timeout = ref 30.

let swtpm_ready_timeout = ref Mtime.Span.(1 * min)

let use_upstream_qemu = ref false

let pci_quarantine = ref true

let watch_queue_length = ref 1000

let default_vbd_backend_kind = ref "vbd"

let ca_140252_workaround = ref false

let action_after_qemu_crash = ref None

let additional_ballooning_timeout = ref 120.

let vif_ready_for_igmp_query_timeout = ref 120

let feature_flags_path = ref "/etc/xenserver/features.d"

let pvinpvh_xen_cmdline = ref "pv-shim console=xen"

(* O(N^2) operations, until we get a xenstore cache, so use a small number here *)
let vm_guest_agent_xenstore_quota = ref 128

let vm_guest_agent_xenstore_quota_warn_interval = ref 3600

let oxenstored_conf = ref "/etc/xen/oxenstored.conf"

let for_each_line path f =
  let ic = open_in path in
  log_and_ignore_exn (fun () ->
      try
        while true do
          f ic
        done
      with End_of_file -> ()
  ) ;
  close_in_noerr ic

let parse_oxenstored_conf path =
  D.debug "Parsing %s" path ;
  let config = ref [] in
  log_and_ignore_exn (fun () ->
      for_each_line path @@ fun ic ->
      match ic |> input_line |> Xcp_service.Config_file.parse_line with
      | Some (k, v) ->
          config := (k, v) :: !config
      | None ->
          ()
  ) ;
  D.debug "%s: %d config entries" path (List.length !config) ;
  !config

(* for backward compatibility compute how much memory N entries
   would've taken *)
let max_bytes_of_xenstore_entries entries =
  (* defaults from oxenstored.conf *)
  let conf = parse_oxenstored_conf !oxenstored_conf in
  let get key default conv =
    try List.assoc key conf |> conv with _ -> default
  in
  if not (get "quota-activate" true bool_of_string) then (
    warn
      "Quotas are turned off in oxenstored. This is insecure and not a \
       supported configuration!" ;
    max_int
  ) else
    let default_path_max =
      get "quota-path-max" 1024 int_of_string
      (* maximum size in bytes of a xenstore path *)
    in
    let default_maxsize =
      get "quota-maxsize" 2048 int_of_string
      (* maximum size in bytes of a xenstore value *)
    in
    D.debug "entry_overhead = %d" Xenops_utils.entry_overhead ;
    D.debug "default_path_max = %d" default_path_max ;
    D.debug "longest_encoded_char = %d" Xenops_utils.longest_encoded_char ;
    D.debug "default_maxsize = %d" default_maxsize ;
    D.debug "entries = %d" entries ;
    entries
    * (Xenops_utils.entry_overhead
      + default_path_max
      + (Xenops_utils.longest_encoded_char * default_maxsize)
      )

let vm_guest_agent_xenstore_quota_bytes = ref (25 * 1024 * 1024)

let test_open = ref 0

let options =
  [
    ( "queue"
    , Arg.Set_string Xenops_interface.queue_name
    , (fun () -> !Xenops_interface.queue_name)
    , "Listen on a specific queue"
    )
  ; ( "sockets-path"
    , Arg.Set_string sockets_path
    , (fun () -> !sockets_path)
    , "Directory to create listening sockets"
    )
  ; ( "persist"
    , Arg.Bool (fun b -> persist := b)
    , (fun () -> string_of_bool !persist)
    , "True if we want to persist metadata across restarts"
    )
  ; ( "worker-pool-size"
    , Arg.Set_int worker_pool_size
    , (fun () -> string_of_int !worker_pool_size)
    , "Number of threads for the worker pool"
    )
  ; ( "database-path"
    , Arg.String (fun x -> Xenops_utils.root := Some x)
    , (fun () -> Xenops_utils.get_root ())
    , "Location to store the metadata"
    )
  ; ( "run_hotplug_scripts"
    , Arg.Bool (fun x -> run_hotplug_scripts := x)
    , (fun () -> string_of_bool !run_hotplug_scripts)
    , "True if xenopsd should execute the hotplug scripts directly"
    )
  ; ( "use_old_pci_add"
    , Arg.Bool (fun x -> use_old_pci_add := x)
    , (fun () -> string_of_bool !use_old_pci_add)
    , "True if xenopsd should use the old pci add function"
    )
  ; ( "hotplug_timeout"
    , Arg.Set_float hotplug_timeout
    , (fun () -> string_of_float !hotplug_timeout)
    , "Time before we assume hotplug scripts have failed"
    )
  ; ( "qemu_dm_ready_timeout"
    , Arg.Set_float qemu_dm_ready_timeout
    , (fun () -> string_of_float !qemu_dm_ready_timeout)
    , "Time before we assume qemu has become stuck"
    )
  ; ( "vgpu-ready-timeout"
    , Arg.Set_float vgpu_ready_timeout
    , (fun () -> string_of_float !vgpu_ready_timeout)
    , "Time before we assume vgpu has become stuck or unresponsive"
    )
  ; ( "varstored-ready-timeout"
    , Arg.Set_float varstored_ready_timeout
    , (fun () -> string_of_float !varstored_ready_timeout)
    , "Time before we assume varstored has become stuck or unresponsive"
    )
  ; ( "watch_queue_length"
    , Arg.Set_int watch_queue_length
    , (fun () -> string_of_int !watch_queue_length)
    , "Maximum number of unprocessed xenstore watch events before we restart"
    )
  ; ( "use-upstream-qemu"
    , Arg.Bool (fun x -> use_upstream_qemu := x)
    , (fun () -> string_of_bool !use_upstream_qemu)
    , "True if we want to use upsteam QEMU"
    )
  ; ( "default-vbd-backend-kind"
    , Arg.Set_string default_vbd_backend_kind
    , (fun () -> !default_vbd_backend_kind)
    , "Default backend for VBDs"
    )
  ; ( "ca-140252-workaround"
    , Arg.Bool (fun x -> ca_140252_workaround := x)
    , (fun () -> string_of_bool !ca_140252_workaround)
    , "Workaround for evtchn misalignment for legacy PV tools"
    )
  ; ( "additional-ballooning-timeout"
    , Arg.Set_float additional_ballooning_timeout
    , (fun () -> string_of_float !additional_ballooning_timeout)
    , "Time we allow the guests to do additional memory ballooning before live \
       migration"
    )
  ; ( "domain_shutdown_ack_timeout"
    , Arg.Set_float Xenops_server.domain_shutdown_ack_timeout
    , (fun () -> string_of_float !Xenops_server.domain_shutdown_ack_timeout)
    , "Time to wait for in-guest PV drivers to acknowledge a shutdown request \
       before we conclude that the drivers have failed"
    )
  ; ( "vif-ready-for-igmp-query-timeout"
    , Arg.Set_int vif_ready_for_igmp_query_timeout
    , (fun () -> string_of_int !vif_ready_for_igmp_query_timeout)
    , "Time before we assume vif has connected"
    )
  ; ( "action-after-qemu-crash"
    , Arg.String
        (fun x -> action_after_qemu_crash := if x = "" then None else Some x)
    , (fun () -> match !action_after_qemu_crash with None -> "" | Some x -> x)
    , "Action to take for VMs if QEMU crashes or dies unexpectedly: pause, \
       poweroff. Otherwise, no action (default)."
    )
  ; ( "feature-flags-path"
    , Arg.Set_string feature_flags_path
    , (fun () -> !feature_flags_path)
    , "Directory of experimental feature flags"
    )
  ; ( "pvinpvh-xen-cmdline"
    , Arg.Set_string pvinpvh_xen_cmdline
    , (fun () -> !pvinpvh_xen_cmdline)
    , "Command line for the inner-xen for PV-in-PVH guests"
    )
  ; ( "numa-placement"
    , Arg.Bool (fun _ -> ())
    , (fun () ->
        string_of_bool
          (!Xenops_server.default_numa_affinity_policy = Best_effort)
      )
    , "NUMA-aware placement of VMs (deprecated, use XAPI setting)"
    )
  ; ( "pci-quarantine"
    , Arg.Bool (fun b -> pci_quarantine := b)
    , (fun () -> string_of_bool !pci_quarantine)
    , "True if IOMMU contexts of PCI devices are needed to be placed in \
       quarantine"
    )
  ; ( "vm-guest-agent-xenstore-quota"
    , Arg.String
        (fun s ->
          if s <> "N/A" then
            vm_guest_agent_xenstore_quota_bytes :=
              max_bytes_of_xenstore_entries (int_of_string s)
        )
    , (fun () -> "N/A")
    , "(Deprecated, use vm-xenstore-data-quota-bytes instead)"
    )
  ; ( "vm-guest-agent-xenstore-quota-warn-interval"
    , Arg.Set_int vm_guest_agent_xenstore_quota_warn_interval
    , (fun () -> string_of_int !vm_guest_agent_xenstore_quota_warn_interval)
    , "How often to warn that a VM is still over its xenstore quota"
    )
  ; ( "oxenstored-conf"
    , Arg.Set_string oxenstored_conf
    , (fun () -> !oxenstored_conf)
    , "Path to oxenstored conf (for reading quotas)"
    )
  ; ( "vm-guest-agent-xenstore-quota-bytes"
    , Arg.Set_int vm_guest_agent_xenstore_quota_bytes
    , (fun () -> string_of_int !vm_guest_agent_xenstore_quota_bytes)
    , "Maximum size in bytes of VM xenstore-data field, and guest metrics \
       copied from guest's vm-data/ and data/ xenstore tree"
    )
  ; ( "test-open"
    , Arg.Set_int test_open
    , (fun () -> string_of_int !test_open)
    , "TESTING only: open N file descriptors"
    )
  ; ( "xenopsd-vbd-plug-unplug-legacy"
    , Arg.Bool (fun x -> Xenops_server.xenopsd_vbd_plug_unplug_legacy := x)
    , (fun () -> string_of_bool !Xenops_server.xenopsd_vbd_plug_unplug_legacy)
    , "False if we want to split the plug atomic into attach/activate"
    )
  ]

let path () = Filename.concat !sockets_path "xenopsd"

let forwarded_path () = path () ^ ".forwarded"

(* receive an authenticated fd from xapi *)

let json_path () = path () ^ ".json"

let rpc_fn call =
  (* Upgrade import_metadata API call *)
  let call', call_name, span_parent =
    match (call.Rpc.name, call.Rpc.params) with
    | ("VM.import_metadata" as call_name), [Rpc.String debug_info; metadata] ->
        debug "Upgrading VM.import_metadata" ;
        let span_parent =
          let di = debug_info |> Debug_info.of_string in
          di.tracing
        in
        ( Rpc.
            {
              name= "VM.import_metadata"
            ; params=
                [
                  Rpc.Dict
                    [
                      ("debug_info", Rpc.String debug_info)
                    ; ("metadata", metadata)
                    ]
                ]
            ; is_notification= false
            }
        , call_name
        , span_parent
        )
    | ("query" as call_name), [Rpc.String debug_info; unit_p] ->
        debug "Upgrading query" ;
        let span_parent =
          let di = debug_info |> Debug_info.of_string in
          di.tracing
        in
        ( Rpc.
            {
              name= "query"
            ; params=
                [
                  Rpc.Dict
                    [("debug_info", Rpc.String debug_info); ("unit", unit_p)]
                ]
            ; is_notification= false
            }
        , call_name
        , span_parent
        )
    | call_name, [Rpc.Dict kv_list] ->
        let span_parent =
          kv_list
          |> List.find_map (function
               | "debug_info", Rpc.String debug_info ->
                   let di = debug_info |> Debug_info.of_string in
                   di.tracing
               | _ ->
                   None
               )
        in
        (call, call_name, span_parent)
    | call_name, _ ->
        (call, call_name, None)
  in
  Tracing.with_tracing
    ~attributes:
      [
        ("messaging.operation.name", "process")
      ; ("messaging.system", "message-switch")
      ; ("messaging.destination.name", !Xenops_interface.queue_name)
      ]
    ~span_kind:Tracing.SpanKind.Consumer ~parent:span_parent
    ~name:("process" ^ " " ^ call_name)
  @@ fun _ -> Idl.Exn.server Xenops_server.Server.implementation call'

let handle_received_fd this_connection =
  let msg_size = 16384 in
  let buf = Bytes.make msg_size '\000' in
  debug "Calling recv_fd()" ;
  let len, _, received_fd =
    Fd_send_recv.recv_fd this_connection buf 0 msg_size []
  in
  debug "recv_fd ok (len = %d)" len ;
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
      let req =
        Bytes.sub_string buf 0 len
        |> Jsonrpc.of_string
        |> Xenops_migrate.Forwarded_http_request.t_of_rpc
      in
      debug "Received request = [%s]\n%!"
        (req
        |> Xenops_migrate.Forwarded_http_request.rpc_of_t
        |> Jsonrpc.to_string
        ) ;
      let common_prefix = "/services/xenops/" in
      let memory_prefix = common_prefix ^ "memory/" in
      let migrate_vgpu_prefix = common_prefix ^ "migrate-vgpu/" in
      (* below we define new routes used for the new handshake protocol *)
      let migrate = "/services/xenops/migrate/" in
      let migrate_vm = migrate ^ "vm" in
      let migrate_mem = migrate ^ "mem" in
      let migrate_vgpu = migrate ^ "vgpu" in

      let has_prefix str prefix =
        String.length prefix <= String.length str
        && String.sub str 0 (String.length prefix) = prefix
      in
      let do_receive fn =
        let context = {Xenops_server.transferred_fd= Some received_fd} in
        let uri = Uri.of_string req.Xenops_migrate.Forwarded_http_request.uri in
        let traceparent =
          List.assoc_opt "traceparent"
            req.Xenops_migrate.Forwarded_http_request.additional_headers
        in
        fn uri req.Xenops_migrate.Forwarded_http_request.cookie traceparent
          this_connection context
      in
      let uri = req.Xenops_migrate.Forwarded_http_request.uri in
      if has_prefix uri memory_prefix then
        do_receive Xenops_server.VM.receive_memory
      else if has_prefix uri migrate_vgpu_prefix then
        do_receive Xenops_server.VM.receive_vgpu
      (* new routes but using same handlers *)
      else if has_prefix uri migrate_vm then
        do_receive Xenops_server.VM.receive_memory
      else if has_prefix uri migrate_mem then
        do_receive Xenops_server.VM.receive_mem
      else if has_prefix uri migrate_vgpu then
        do_receive Xenops_server.VM.receive_vgpu
      else (
        error "Expected URI prefix %s or %s, got %s" memory_prefix
          migrate_vgpu_prefix uri ;
        let module Response =
          Cohttp.Response.Make (Cohttp_posix_io.Unbuffered_IO) in
        let headers = Cohttp.Header.of_list [("User-agent", "xenopsd")] in
        let response =
          Cohttp.Response.make ~version:`HTTP_1_1 ~status:`Not_found ~headers ()
        in
        Response.write (fun _ -> ()) response this_connection
      )
    )
    (fun () -> Unix.close received_fd)

let doc =
  String.concat "\n"
    [
      "This is the xapi toolstack domain management daemon."
    ; ""
    ; "Xenopsd looks after a set of Xen domains, performing lifecycle \
       operations including start/shutdown/migrate. A system may run multiple \
       xenopsds, each looking after a different set of VMs. Xenopsd will \
       always ignore domains that it hasn't been asked to manage. There are \
       multiple xenopsd *backends*, including 'xc': which uses libxc directly \
       and 'xenlight': which uses the new Xen libxl library (recommended)."
    ]

let configure ?(specific_options = []) ?(specific_essential_paths = [])
    ?(specific_nonessential_paths = []) () =
  Debug.set_facility Syslog.Local5 ;
  debug "xenopsd version %s starting, pid: %d" Xapi_version.version
    (Unix.getpid ()) ;
  let options = options @ specific_options in
  let resources =
    Resources.make_resources
      ~essentials:(Resources.essentials @ specific_essential_paths)
      ~nonessentials:(Resources.nonessentials @ specific_nonessential_paths)
  in
  Xcp_service.configure2
    ~name:(Filename.basename Sys.argv.(0))
    ~version:Xapi_version.version ~doc ~options ~resources ()

let log_raw_backtrace bt =
  Option.iter
    (fun slots ->
      Array.iteri
        (fun i slot ->
          Printexc.Slot.format i slot
          |> Option.iter (fun s -> error "%d. %s" i s)
        )
        slots
    )
    (Printexc.backtrace_slots bt)

let log_uncaught_exception e bt =
  error "xenopsd exitted with an uncaught exception: %s" (Printexc.to_string e) ;
  log_raw_backtrace bt

let test_open () =
  let count = !test_open in
  if count > 0 then (
    debug "TEST: opening %d file descriptors" count ;
    Xapi_stdext_unix.Unixext.test_open count ;
    debug "TEST: opened %d file descriptors" count
  )

let main backend =
  Printexc.record_backtrace true ;
  Printexc.set_uncaught_exception_handler log_uncaught_exception ;
  test_open () ;
  (* Set service name for Tracing *)
  Tracing_export.set_service_name "xenopsd" ;
  (* Listen for transferred file descriptors *)
  let forwarded_server =
    Xcp_service.make_socket_server (forwarded_path ()) handle_received_fd
  in

  (* TODO: this should be indirected through the switch *)

  (* Listen for regular API calls *)
  let xml_server =
    Xcp_service.make ~path:(path ())
      ~queue_name:!Xenops_interface.queue_name
      ~rpc_fn ()
  in
  (* we need to catch this to make sure at_exit handlers are triggered. In
     particular, triggers for the bisect_ppx coverage profiling *)
  let signal_handler n =
    debug "caught signal %a" Debug.Pp.signal n ;
    exit 0
  in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  Sys.set_signal Sys.sigterm (Sys.Signal_handle signal_handler) ;
  Xenops_utils.set_fs_backend
    (Some
       ( if !persist then
           (module Xenops_utils.FileFS : Xenops_utils.FS)
         else
           (module Xenops_utils.MemFS : Xenops_utils.FS)
       )
    ) ;
  Xenops_server.register_objects () ;
  Xenops_server.set_backend (Some backend) ;
  Xenops_server.upgrade_internal_state_of_running_vms () ;
  Debug.with_thread_associated "main"
    (fun () ->
      let (_ : Thread.t) =
        Thread.create (fun () -> Xcp_service.serve_forever forwarded_server) ()
      in
      let (_ : Thread.t) =
        Thread.create (fun () -> Xcp_service.serve_forever xml_server) ()
      in
      ()
    )
    () ;
  Xenops_server.WorkerPool.start !worker_pool_size ;
  while true do
    try Thread.delay 60.
    with e -> debug "Thread.delay caught: %s" (Printexc.to_string e)
  done

(* Verify the signature matches *)
module S : Xenops_server_plugin.S = Xenops_server_skeleton
