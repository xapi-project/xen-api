(* Copyright (C) Citrix Systems Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
*)

module D = Debug.Make (struct let name = "service" end)

open! D
module Unixext = Xapi_stdext_unix.Unixext
module Xenops_task = Xenops_task.Xenops_task
module Chroot = Xenops_sandbox.Chroot
module Path = Chroot.Path
module Xs = Ezxenstore_core.Xenstore.Xs
module Socket = Xenops_utils.Socket

let defer f g = Xapi_stdext_pervasives.Pervasiveext.finally g f

exception Service_failed of (string * string)

let () =
  Printexc.register_printer (function
    | Service_failed (service, reason) ->
        Some
          (Printf.sprintf "Service_failed(service=%s,reason=%s)" service reason)
    | _ ->
        None
    )

type t = {
    name: string
  ; domid: Xenctrl.domid
  ; exec_path: string
  ; pid_filename: string
  ; chroot: Chroot.t
  ; timeout_seconds: int
  ; args: string list
  ; execute:
      path:string -> args:string list -> domid:Xenctrl.domid -> unit -> string
}

let alive service _ =
  let is_active = Fe_systemctl.is_active ~service in
  ( if not is_active then
      let status = Fe_systemctl.show ~service in
      error
        "%s: unexpected termination \
         (Result=%s,ExecMainPID=%d,ExecMainStatus=%d,ActiveState=%s)"
        service status.result status.exec_main_pid status.exec_main_status
        status.active_state
  ) ;
  is_active

type watch_trigger = Created | Cancelled | Waiting

let fold_events ~init f events =
  events
  |> List.to_seq
  |> Seq.flat_map (fun (_, events, _, fnameopt) ->
         List.to_seq events |> Seq.map (fun event -> (event, fnameopt))
     )
  |> Seq.fold_left f init

exception ECancelled of Xenops_task.task_handle

let raise_e = function
  | ECancelled t ->
      Xenops_task.raise_cancelled t
  | e ->
      raise e

let with_inotify f =
  let fd = Inotify.create () in
  defer (fun () -> Unix.close fd) (fun () -> f fd)

let with_watch notifd dir f =
  let open Inotify in
  let flags = [S_Create; S_Delete; S_Delete_self; S_Onlydir] in
  let watch = Inotify.add_watch notifd dir flags in
  defer (fun () -> Inotify.rm_watch notifd watch) (fun () -> f watch)

let with_monitor watch_fd f =
  let fd = Polly.create () in
  Polly.add fd watch_fd Polly.Events.inp ;
  defer (fun () -> Polly.del fd watch_fd ; Polly.close fd) (fun () -> f fd)

let start_and_wait_for_readyness ~task ~service =
  let sandbox_path p =
    Chroot.absolute_path_outside service.chroot (Path.of_string ~relative:p)
  in

  let cancel_name =
    Printf.sprintf "%s-%s.cancel" service.name (Xenops_task.get_dbg task)
  in

  let cancel_path = sandbox_path cancel_name in

  let cancel () =
    (* create an empty file to trigger the watch and delete it
       immediately *)
    Unixext.touch_file cancel_path ;
    Unixext.unlink_safe cancel_path
  in
  (* create watches for pidfile and task cancellation *)
  with_inotify @@ fun notifd ->
  with_watch notifd service.chroot.root @@ fun _ ->
  with_monitor notifd @@ fun pollfd ->
  let wait ~amount ~service_name =
    let from_time = Mtime_clock.counter () in
    let poll_period_ms = 1000 in
    let collect_watches acc (event, file) =
      match (acc, event, file) with
      (* treat deleted directory or pidfile as cancelling *)
      | Cancelled, _, _ | _, (Inotify.Ignored | Inotify.Delete_self), _ ->
          Cancelled
      | _, Inotify.Delete, Some name when name = service.pid_filename ->
          Cancelled
      | _, Inotify.Create, Some name when name = cancel_name ->
          Cancelled
      | _, Inotify.Create, Some name when name = service.pid_filename ->
          Created
      | _, _, _ ->
          acc
    in

    let cancellable_watch () =
      let event = ref Waiting in
      let rec poll_loop () =
        try
          ignore
          @@ Polly.wait pollfd 1 poll_period_ms (fun _ fd events ->
                 if Polly.Events.(test events inp) then
                   event :=
                     fold_events ~init:!event collect_watches (Inotify.read fd)
             ) ;

          let elapsed = Mtime_clock.count from_time in

          match !event with
          | Waiting when Mtime.Span.compare elapsed amount < 0 ->
              poll_loop ()
          | Created ->
              Ok ()
          | Cancelled ->
              Error (ECancelled task)
          | Waiting ->
              let err_msg =
                if alive service_name () then
                  "Timeout reached while starting service"
                else
                  "Service exited unexpectedly"
              in
              Error (Service_failed (service_name, err_msg))
        with e ->
          let err_msg =
            Printf.sprintf
              "Exception while waiting for service %s to be ready: %s"
              service_name (Printexc.to_string e)
          in
          Error (Service_failed (service_name, err_msg))
      in

      Xenops_task.with_cancel task cancel poll_loop
    in
    cancellable_watch ()
  in

  (* start systemd service *)
  let syslog_key =
    service.execute ~path:service.exec_path ~args:service.args
      ~domid:service.domid ()
  in

  Xenops_task.check_cancelling task ;

  let amount = Mtime.Span.(service.timeout_seconds * s) in
  (* wait for pidfile to appear *)
  Result.iter_error raise_e (wait ~amount ~service_name:syslog_key) ;

  debug "Service %s initialized" syslog_key

(* Waits for a daemon to signal startup by writing to a xenstore path
   (optionally with a given value) If this doesn't happen in the timeout then
   an exception is raised *)
let wait_path ~pidalive ~task ~name ~domid ~xs ~ready_path ~timeout ~cancel _ =
  let syslog_key = Printf.sprintf "%s-%d" name domid in
  let watch =
    Ezxenstore_core.Watch.(value_to_appear ready_path |> map (fun _ -> ()))
  in
  Xenops_task.check_cancelling task ;
  ( try
      let (_ : bool) =
        Cancel_utils.cancellable_watch cancel [watch] [] task ~xs ~timeout ()
      in
      ()
    with Ezxenstore_core.Watch.Timeout _ ->
      if pidalive name then
        raise (Service_failed (name, "Timeout reached while starting daemon")) ;
      raise (Service_failed (name, "Daemon exited unexpectedly"))
  ) ;
  debug "Daemon initialised: %s" syslog_key

let pid_alive pid name =
  match Forkhelpers.waitpid_nohang pid with
  | 0, Unix.WEXITED 0 ->
      true
  | _, Unix.WEXITED n ->
      error "%s: unexpected exit with code: %d" name n ;
      false
  | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) ->
      error "%s: unexpected signal: %d" name n ;
      false

let pidfile_path root daemon_name domid =
  Printf.sprintf "%s/%s-%d.pid" root daemon_name domid

let pidfile_path_tmpfs daemon_name domid =
  pidfile_path Device_common.var_run_xen_path daemon_name domid

module Pid = struct
  type both = {key: int -> string; file: int -> string}

  type path =
    | Xenstore of (int -> string)
    | File of (int -> string)
    | Path_of of both
end

module type DAEMONPIDPATH = sig
  val name : string

  val pid_location : Pid.path

  (* To check if a pid is valid, look up its process name, and some commandline
     arg containing domid if domid is not part of its process name, check that
     they are contained in /proc/<pid>/cmdline.
     The expected cmdline items are set by (expected_cmdline_items domid) for
     each service. Note that the parameter "domid" here is required as
     otherwise we can not distinguish between the process instances of the same
     service for different domains. *)
  val expected_cmdline_items : domid:int -> string list
end

module DaemonMgmt (D : DAEMONPIDPATH) = struct
  module SignalMask = struct
    module H = Hashtbl

    type t = (int, bool) H.t

    let create () = H.create 16

    let set tbl key = H.replace tbl key true

    let unset tbl key = H.remove tbl key

    let has tbl key = H.mem tbl key
  end

  let signal_mask = SignalMask.create ()

  let name = D.name

  (* For process id, look up its process name, and some commandline arg
     containing domid if domid is not part of its process name, check that
     they are contained in /proc/<pid>/cmdline. *)
  let is_cmdline_valid ~pid ~pid_source expected_args =
    try
      let cmdline_str =
        Printf.sprintf "/proc/%d/cmdline" pid |> Unixext.string_of_file
      in
      match cmdline_str with
      | "" ->
          (* from man proc:
             /proc/[pid]/cmdline
             This read-only file holds the complete command line for the process,
             unless the process is a zombie. In the latter case, there is nothing
             in this file: that is, a read on this file will return 0 characters.
             This applies when the VM is being shut down. *)
          false
      | _ ->
          let cmdline = Astring.String.cuts ~sep:"\000" cmdline_str in
          let valid =
            List.for_all (fun arg -> List.mem arg cmdline) expected_args
          in
          if not valid then
            error "%s: pid read from %s not valid (pid = %d)" D.name pid_source
              pid ;
          valid
    with _ -> false

  let pid ~xs domid =
    let ( let* ) = Option.bind in
    let* pid, pid_source =
      try
        match D.pid_location with
        | (File file | Path_of {file; _}) when Sys.file_exists (file domid) ->
            let path = file domid in
            let* pid = Unixext.pidfile_read path in
            Unixext.with_file path [Unix.O_RDONLY] 0 (fun fd ->
                try
                  Unix.lockf fd Unix.F_TRLOCK 0 ;
                  (* we succeeded taking the lock: original process is dead.
                   * some other process might've reused its pid *)
                  None
                with Unix.Unix_error (Unix.EAGAIN, _, _) ->
                  (* cannot obtain lock: process is alive *)
                  Some (pid, "pidfile")
            )
        | Xenstore key | Path_of {key; _} ->
            (* case 1: backward compatibility during update installation: only has
                 xenstore pid available.
               case 2: pidfile(file domid) got removed unexpectly *)
            let pid = xs.Xs.read (key domid) in
            Some (int_of_string pid, "xenstore")
        | _ ->
            None
      with _ -> None
    in
    if is_cmdline_valid ~pid ~pid_source (D.expected_cmdline_items ~domid) then
      Some pid
    else
      None

  let is_running ~xs domid =
    match pid ~xs domid with
    | None ->
        false
    | Some p -> (
      try Unix.kill p 0 ; (* This checks the existence of pid p *)
                          true
      with _ -> false
    )

  let stop ~xs domid =
    match pid ~xs domid with
    | None ->
        ()
    | Some pid -> (
        let best_effort = Xenops_utils.best_effort in
        let really_kill = Xenops_utils.really_kill in
        debug "%s: stopping %s with SIGTERM (domid = %d pid = %d)" D.name D.name
          domid pid ;
        best_effort (Printf.sprintf "killing %s" D.name) (fun () ->
            really_kill pid
        ) ;
        let remove_key key =
          best_effort (Printf.sprintf "removing XS key %s" key) (fun () ->
              xs.Xs.rm key
          )
        in
        let remove_file path =
          best_effort (Printf.sprintf "removing %s" path) (fun () ->
              Unix.unlink path
          )
        in
        match D.pid_location with
        | Xenstore key ->
            remove_key (key domid)
        | File file ->
            remove_file (file domid)
        | Path_of {key; file} ->
            remove_key (key domid) ;
            remove_file (file domid)
      )

  let syslog_key ~domid = Printf.sprintf "%s-%d" D.name domid

  let start ~fds ~syslog_key path args =
    let syslog_stdout = Forkhelpers.Syslog_WithKey syslog_key in
    let redirect_stderr_to_stdout = true in
    let pid =
      Forkhelpers.safe_close_and_exec None None None fds ~syslog_stdout
        ~redirect_stderr_to_stdout path args
    in
    debug
      "%s: should be running in the background (stdout -> syslog); (fd,pid) = \
       %s"
      D.name
      (Forkhelpers.string_of_pidty pid) ;
    pid

  (* Forks a daemon and then returns the pid. *)
  let start_daemon ~path ~args ~domid ?(fds = []) () =
    let syslog_key = syslog_key ~domid in
    debug "Starting daemon: %s with args [%s]" path (String.concat "; " args) ;
    let pid = start ~fds ~syslog_key path args in
    debug "Daemon started: %s" syslog_key ;
    pid
end

module Qemu = struct
  let name = "qemu-dm"

  let pidfile_path = pidfile_path_tmpfs name

  let pidxenstore_path domid = Printf.sprintf "/local/domain/%d/qemu-pid" domid

  let pidxenstore_path_signal domid = pidxenstore_path domid ^ "-signal"

  module D = DaemonMgmt (struct
    let name = name

    let pid_location = Pid.Path_of {key= pidxenstore_path; file= pidfile_path}

    let expected_cmdline_items ~domid = [Printf.sprintf "qemu-dm-%d" domid]
  end)

  module SignalMask = D.SignalMask

  let signal_mask = D.signal_mask

  let start_daemon = D.start_daemon

  let pid = D.pid

  let is_running = D.is_running

  let stop ~xs ~qemu_domid domid =
    let file_path = pidfile_path domid in
    match pid ~xs domid with
    | None ->
        Unixext.unlink_safe file_path
    | Some pid ->
        let xenstore_path = pidxenstore_path domid in
        let best_effort = Xenops_utils.best_effort in
        let really_kill = Xenops_utils.really_kill in
        debug "qemu-dm: stopping qemu-dm with SIGTERM (domid = %d)" domid ;
        best_effort
          "signalling that qemu is ending as expected, mask further signals"
          (fun () -> SignalMask.set signal_mask domid
        ) ;
        best_effort "killing qemu-dm" (fun () -> really_kill pid) ;
        best_effort "removing qemu-pid from xenstore" (fun () ->
            xs.Xs.rm xenstore_path
        ) ;
        best_effort "unmasking signals, qemu-pid is already gone from xenstore"
          (fun () -> SignalMask.unset signal_mask domid
        ) ;
        best_effort "removing device model path from xenstore" (fun () ->
            xs.Xs.rm (Device_common.device_model_path ~qemu_domid domid)
        ) ;
        best_effort (Printf.sprintf "removing %s" file_path) (fun () ->
            Unix.unlink file_path
        )
end

module Vgpu = struct
  let domain_arg domid = Printf.sprintf "--domain=%d" domid

  module D = DaemonMgmt (struct
    let name = "vgpu"

    let pid_path domid = Printf.sprintf "/local/domain/%d/vgpu-pid" domid

    let pid_location = Pid.Xenstore pid_path

    let expected_cmdline_items ~domid = [!Xc_resources.vgpu; domain_arg domid]
  end)

  (** An NVidia Virtual Compute Service vGPU has a class attribute
     "Compute". Recognise this here *)
  let is_compute_vgpu vgpu =
    let open Xenops_interface.Vgpu in
    match vgpu with
    | {implementation= Nvidia {vclass= Some "Compute"; _}; _} ->
        true
    | _ ->
        false

  let vgpu_args_of_nvidia domid vcpus vgpus restore =
    let open Xenops_interface.Vgpu in
    let virtual_pci_address_compare vgpu1 vgpu2 =
      match (vgpu1, vgpu2) with
      | ( {implementation= Nvidia {virtual_pci_address= pci1; _}; _}
        , {implementation= Nvidia {virtual_pci_address= pci2; _}; _} ) ->
          Stdlib.compare pci1.dev pci2.dev
      | other1, other2 ->
          Stdlib.compare other1 other2
    in
    let device_args =
      let make addr args =
        Printf.sprintf "--device=%s"
          (Xcp_pci.string_of_address addr :: args
          |> List.filter (fun str -> str <> "")
          |> String.concat ","
          )
      in
      vgpus
      |> List.sort virtual_pci_address_compare
      |> List.map (fun vgpu ->
             let addr =
               match vgpu.virtual_pci_address with
               | Some pci ->
                   pci (* pass VF in case of SRIOV *)
               | None ->
                   vgpu.physical_pci_address
             in
             (* pass PF otherwise *)
             match vgpu.implementation with
             (* 1. Upgrade case, migrate from a old host with old vGPU having
                config_path 2. Legency case, run with old Nvidia host driver *)
             | Nvidia
                 {
                   virtual_pci_address
                 ; config_file= Some config_file
                 ; extra_args
                 ; _
                 } ->
                 (* The VGPU UUID is not available. Create a fresh one; xapi
                    will deal with it. *)
                 let uuid = Uuidx.(to_string (make ())) in
                 debug "NVidia vGPU config: using config file %s and uuid %s"
                   config_file uuid ;
                 make addr
                   [
                     config_file
                   ; Xcp_pci.string_of_address virtual_pci_address
                   ; uuid
                   ; extra_args
                   ]
             | Nvidia
                 {
                   virtual_pci_address
                 ; type_id= Some type_id
                 ; uuid= Some uuid
                 ; extra_args
                 ; _
                 } ->
                 debug "NVidia vGPU config: using type id %s and uuid: %s"
                   type_id uuid ;
                 make addr
                   [
                     type_id
                   ; Xcp_pci.string_of_address virtual_pci_address
                   ; uuid
                   ; extra_args
                   ]
             | Nvidia {type_id= None; config_file= None; _} ->
                 (* No type_id _and_ no config_file: something is wrong *)
                 raise
                   (Xenops_interface.Xenopsd_error
                      (Internal_error
                         (Printf.sprintf "NVidia vGPU metadata incomplete (%s)"
                            __LOC__
                         )
                      )
                   )
             | _ ->
                 ""
         )
    in
    let suspend_file = Printf.sprintf Device_common.demu_save_path domid in
    let base_args =
      [
        domain_arg domid
      ; "--vcpus=" ^ string_of_int vcpus
      ; "--suspend=" ^ suspend_file
      ]
      @ device_args
    in
    let fd_arg = if restore then ["--resume"] else [] in
    (* support for NVidia VCS (compute) vGPUs *)
    let no_console =
      match List.for_all is_compute_vgpu vgpus with
      | true ->
          ["--noconsole"]
      | false ->
          []
    in
    List.concat [base_args; no_console; fd_arg]

  let state_path domid = Printf.sprintf "/local/domain/%d/vgpu/state" domid

  let cancel_key domid = Cancel_utils.Vgpu domid

  let start ~xs ~vcpus ~vgpus ~restore task domid =
    let args = vgpu_args_of_nvidia domid vcpus vgpus restore in
    let cancel = cancel_key domid in
    let pid = D.start_daemon ~path:!Xc_resources.vgpu ~args ~domid ~fds:[] () in
    wait_path ~pidalive:(pid_alive pid) ~task ~name:D.name ~domid ~xs
      ~ready_path:(state_path domid)
      ~timeout:!Xenopsd.vgpu_ready_timeout
      ~cancel () ;
    Forkhelpers.dontwaitpid pid

  let pid = D.pid

  let is_running = D.is_running

  let stop = D.stop
end

module SystemdDaemonMgmt (D : DAEMONPIDPATH) = struct
  (* backward compat: for daemons running during an update *)
  module Compat = DaemonMgmt (D)

  let of_domid domid =
    let key = Compat.syslog_key ~domid in
    if Fe_systemctl.exists ~service:key then
      Some key
    else
      None

  let stop ~xs domid =
    match of_domid domid with
    | None when Compat.is_running ~xs domid ->
        Compat.stop ~xs domid
    | None ->
        info "Not trying to stop %s for domid %i since it's not running" D.name
          domid
    | Some service ->
        (* call even when not running for clean up *)
        let (_ : Fe_systemctl.status) = Fe_systemctl.stop ~service in
        ()

  let start_daemon ~path ~args ~domid () =
    debug "Starting daemon: %s with args [%s]" path (String.concat "; " args) ;
    let service = Compat.syslog_key ~domid in
    let properties =
      let remove_key path = ("ExecStopPost", "-/usr/bin/xenstore-rm " ^ path) in
      let remove_file path = ("ExecStopPost", "-/bin/rm -f " ^ path) in
      match D.pid_location with
      | Xenstore get_path ->
          let key_path = get_path domid in
          [remove_key key_path]
      | File get_path ->
          let file_path = get_path domid in
          [remove_file file_path]
      | Path_of {key; file} ->
          let key_path = key domid in
          let file_path = file domid in
          [remove_key key_path; remove_file file_path]
    in
    Fe_systemctl.start_transient ~properties ~service path args ;
    debug "Daemon started: %s" service ;
    service
end

module Varstored = struct
  let name = "varstored"

  let pidxenstore_path domid =
    Printf.sprintf "/local/domain/%d/varstored-pid" domid

  let pidfile_path = pidfile_path_tmpfs name

  module D = SystemdDaemonMgmt (struct
    let name = name

    let pid_location = Pid.Path_of {key= pidxenstore_path; file= pidfile_path}

    let expected_cmdline_items ~domid =
      [!Xc_resources.varstored; pidfile_path domid]
  end)

  let efivars_resume_path =
    Xenops_sandbox.Chroot.Path.of_string ~relative:"efi-vars-resume.dat"

  let efivars_save_path =
    Xenops_sandbox.Chroot.Path.of_string ~relative:"efi-vars-save.dat"

  let start ~xs ~nvram ?(restore = false) task domid =
    let open Xenops_types in
    debug "Preparing to start varstored for UEFI boot (domid=%d)" domid ;
    let path = !Xc_resources.varstored in
    let vm_uuid = Xenops_helpers.uuid_of_domid ~xs domid |> Uuidx.to_string in
    let reset_on_boot = Nvram_uefi_variables.(nvram.on_boot = Reset) in
    let backend = nvram.Nvram_uefi_variables.backend in
    let chroot, socket_path =
      Xenops_sandbox.Varstore_guard.start (Xenops_task.get_dbg task) ~vm_uuid
        ~domid ~paths:[efivars_save_path]
    in
    let open Fe_argv in
    let argf fmt = Printf.ksprintf (fun s -> ["--arg"; s]) fmt in
    let on cond value = if cond then value else return () in
    let args =
      Add.many
        [
          "--domain"
        ; string_of_int domid
        ; "--chroot"
        ; chroot.root
        ; "--depriv"
        ; "--uid"
        ; string_of_int chroot.uid
        ; "--gid"
        ; string_of_int chroot.gid
        ; "--backend"
        ; backend
        ; "--arg"
        ; Printf.sprintf "socket:%s" socket_path
        ; "--pidfile"
        ; pidfile_path domid
        ]
      >>= fun () ->
      Add.many @@ argf "uuid:%s" vm_uuid >>= fun () ->
      on reset_on_boot @@ Add.arg "--nonpersistent" >>= fun () ->
      on restore @@ Add.arg "--resume" >>= fun () ->
      on restore
      @@ Add.many
      @@ argf "resume:%s"
           (Xenops_sandbox.Chroot.chroot_path_inside efivars_resume_path)
      >>= fun () ->
      Add.many
      @@ argf "save:%s"
           (Xenops_sandbox.Chroot.chroot_path_inside efivars_save_path)
    in
    let args = Fe_argv.run args |> snd |> Fe_argv.argv in
    let service = D.start_daemon ~path ~args ~domid () in
    let ready_path = pidxenstore_path domid in
    wait_path ~pidalive:(alive service) ~task ~name ~domid ~xs ~ready_path
      ~timeout:!Xenopsd.varstored_ready_timeout
      ~cancel:(Cancel_utils.Varstored domid) ()

  let stop = D.stop
end

(* TODO: struct and include and uri to uri mapper, etc.
   also xapi needs default backend set
*)
module Swtpm = struct
  let pidfile_path domid = Printf.sprintf "swtpm-%d.pid" domid

  module D = SystemdDaemonMgmt (struct
    let name = "swtpm"

    let pid_location = Pid.File pidfile_path

    let expected_cmdline_items ~domid = [Printf.sprintf "swtpm-%d" domid]
  end)

  let xs_path ~domid = Device_common.get_private_path domid ^ "/vtpm"

  let restore dbg ~domid ~vtpm_uuid state =
    if String.length state > 0 then (
      Xapi_idl_guard_privileged.Client.vtpm_set_contents dbg vtpm_uuid state ;
      debug "Restored vTPM for domid %d: %d bytes, digest %s" domid
        (String.length state)
        (state |> Digest.string |> Digest.to_hex)
    ) else
      debug "vTPM state for domid %d is empty: not restoring" domid

  let check_state_needs_init task vtpm_uuid =
    let dbg = Xenops_task.get_dbg task in
    let contents =
      Xapi_idl_guard_privileged.Client.vtpm_get_contents dbg vtpm_uuid
    in
    String.length contents = 0

  let start ~xs ~vtpm_uuid ~index task domid =
    debug "Preparing to start swtpm-wrapper to provide a vTPM (domid=%d)" domid ;
    let name = "swtpm" in
    let exec_path = !Resources.swtpm_wrapper in
    let pid_filename = pidfile_path domid in
    let vm_uuid = Xenops_helpers.uuid_of_domid ~xs domid |> Uuidx.to_string in

    let chroot, socket_path =
      Xenops_sandbox.Swtpm_guard.start (Xenops_task.get_dbg task) ~vm_uuid
        ~domid ~paths:[]
    in
    let tpm_root =
      Xenops_sandbox.Chroot.(absolute_path_outside chroot Path.root)
    in
    (* the uri here is relative to the chroot path,
       if chrooting is disabled then swtpm-wrapper should chdir accordingly.
       There are three modes: dir://, file:// and unix+http://.
       file:// indicates a linear file storage, and allows further permissions to be restricted.
       xenopsd needs to be in charge of choosing the scheme according to the backend.

       unix+http is the new backend, and dir:// is used during initial
       manufacture.
    *)
    let args needs_init =
      let state_uri =
        if needs_init then
          "dir://." (* exactly 2 //, we can't create this with Uri *)
        else
          (* drop leading / so that the URI works both inside and outside a
             chroot *)
          let host = String.sub socket_path 1 (String.length socket_path - 1) in
          Uri.make ~scheme:"unix+http" ~host () |> Uri.to_string
      in
      Fe_argv.Add.many
        [string_of_int domid; tpm_root; state_uri; string_of_bool needs_init]
      |> Fe_argv.run
      |> snd
      |> Fe_argv.argv
    in
    (* fetch it here instead of swtpm-wrapper: better error reporting,
       swtpm-wrapper runs as a service and getting the exact error back is
       difficult. *)
    let needs_init = check_state_needs_init task vtpm_uuid in
    let timeout_seconds = !Xenopsd.swtpm_ready_timeout in
    if needs_init then (
      debug "vTPM %s is empty, needs to be created" (Uuidm.to_string vtpm_uuid) ;
      let key = Printf.sprintf "%s-%d" (Filename.basename exec_path) domid in
      let _, _ =
        Forkhelpers.execute_command_get_output
          ~syslog_stdout:(Forkhelpers.Syslog_WithKey key)
          ~redirect_stderr_to_stdout:true
          ~timeout:(float_of_int timeout_seconds)
          exec_path (args true)
      in
      let state_file = Filename.concat tpm_root "tpm2-00.permall" in
      let state = Unixext.string_of_file state_file |> Base64.encode_exn in
      let dbg = Xenops_task.get_dbg task in
      debug "storing newly manufactured vTPM state" ;
      Xapi_idl_guard_privileged.Client.vtpm_set_contents dbg vtpm_uuid state ;
      Unixext.unlink_safe state_file
    ) ;
    let execute = D.start_daemon in
    let service =
      {
        name
      ; domid
      ; exec_path
      ; pid_filename
      ; chroot
      ; args= args false
      ; execute
      ; timeout_seconds
      }
    in

    let vtpm_path = xs_path ~domid in

    xs.Xs.write
      (Filename.concat vtpm_path @@ string_of_int index)
      (Uuidm.to_string vtpm_uuid) ;

    start_and_wait_for_readyness ~task ~service ;
    (* return the socket path so qemu can have a reference to it*)
    Xenops_sandbox.Chroot.(
      absolute_path_outside chroot (Path.of_string ~relative:"swtpm-sock")
    )

  let suspend dbg ~xs ~domid ~vtpm_uuid =
    D.stop ~xs domid ;
    Xapi_idl_guard_privileged.Client.vtpm_get_contents dbg vtpm_uuid

  let stop dbg ~xs ~domid ~vm_uuid ~vtpm_uuid =
    debug "About to stop vTPM (%s) for domain %d (%s)"
      (Uuidm.to_string vtpm_uuid)
      domid vm_uuid ;
    D.stop ~xs domid ;
    Xenops_sandbox.Swtpm_guard.stop dbg ~domid ~vm_uuid
end

module PV_Vnc = struct
  let pidxenstore_path domid =
    Printf.sprintf "/local/domain/%d/vncterm-pid" domid

  let vnc_console_path domid = Printf.sprintf "/local/domain/%d/console" domid

  module D = DaemonMgmt (struct
    let name = "vncterm"

    let pid_location = Pid.Xenstore pidxenstore_path

    let expected_cmdline_items ~domid =
      [
        !Xc_resources.vncterm (* vncterm binary path *)
      ; vnc_console_path domid (* xenstore console path *)
      ]
  end)

  let vnc_port_path domid =
    Printf.sprintf "/local/domain/%d/console/vnc-port" domid

  let tc_port_path domid =
    Printf.sprintf "/local/domain/%d/console/tc-port" domid

  let pid ~xs domid = D.pid ~xs domid

  let get_vnc_port ~xs domid =
    if not (D.is_running ~xs domid) then
      None
    else
      try Some (Socket.Port (int_of_string (xs.Xs.read (vnc_port_path domid))))
      with _ -> None

  let get_tc_port ~xs domid =
    if not (D.is_running ~xs domid) then
      None
    else
      try Some (int_of_string (xs.Xs.read (tc_port_path domid)))
      with _ -> None

  let load_args = function
    | None ->
        []
    | Some filename ->
        if Sys.file_exists filename then
          ["-l"; filename]
        else
          []

  exception Failed_to_start

  let vncterm_statefile pid =
    Printf.sprintf "/var/xen/vncterm/%d/vncterm.statefile" pid

  let get_statefile ~xs domid =
    match pid ~xs domid with
    | None ->
        None
    | Some pid ->
        let filename = vncterm_statefile pid in
        if Sys.file_exists filename then
          Some filename
        else
          None

  let save ~xs domid =
    match pid ~xs domid with
    | Some pid ->
        Unix.kill pid Sys.sigusr1 ;
        let filename = vncterm_statefile pid in
        let delay = 10. in
        let start_time = Unix.time () in
        (* wait at most ten seconds *)
        while
          (not (Sys.file_exists filename)) || Unix.time () -. start_time > delay
        do
          debug "Device.PV_Vnc.save: waiting for %s to appear" filename ;
          Thread.delay 1.
        done ;
        if Unix.time () -. start_time > delay then
          debug "Device.PV_Vnc.save: timeout while waiting for %s to appear"
            filename
        else
          debug "Device.PV_Vnc.save: %s has appeared" filename
    | None ->
        ()

  let start ?statefile ~xs ?(ip = "127.0.0.1") domid =
    debug "In PV_Vnc.start" ;
    let l =
      [
        "-x"
      ; vnc_console_path domid
      ; "-T"
      ; (* listen for raw connections *)
        "-v"
      ; ip ^ ":1"
      ]
      @ load_args statefile
    in
    (* Now add the close fds wrapper *)
    let pid = D.start_daemon ~path:!Xc_resources.vncterm ~args:l ~domid () in
    let path = pidxenstore_path domid in
    xs.Xs.write path (string_of_int (Forkhelpers.getpid pid)) ;
    Forkhelpers.dontwaitpid pid

  let stop ~xs domid = D.stop ~xs domid
end
