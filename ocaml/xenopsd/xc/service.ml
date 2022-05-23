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
module Xs = Xenstore.Xs
module Socket = Xenops_utils.Socket

let defer f g = Xapi_stdext_pervasives.Pervasiveext.finally g f

exception Service_failed of (string * string)

type t = {
    name: string
  ; domid: Xenctrl.domid
  ; exec_path: string
  ; chroot: Chroot.t
  ; timeout_seconds: float
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

  let pid_name = Printf.sprintf "%s-%d.pid" service.name service.domid in
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
  let wait ~for_s ~service_name =
    let start_time = Mtime_clock.elapsed () in
    let poll_period_ms = 1000 in
    let collect_watches acc (event, file) =
      match (acc, event, file) with
      (* treat deleted directory or pidfile as cancelling *)
      | Cancelled, _, _ | _, (Inotify.Ignored | Inotify.Delete_self), _ ->
          Cancelled
      | _, Inotify.Delete, Some name when name = pid_name ->
          Cancelled
      | _, Inotify.Create, Some name when name = cancel_name ->
          Cancelled
      | _, Inotify.Create, Some name when name = pid_name ->
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

          let current_time = Mtime_clock.elapsed () in
          let elapsed_time =
            Mtime.Span.(to_s (abs_diff start_time current_time))
          in

          match !event with
          | Waiting when elapsed_time < for_s ->
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

  (* wait for pidfile to appear *)
  Result.iter_error raise_e
    (wait ~for_s:service.timeout_seconds ~service_name:syslog_key) ;

  debug "Service %s initialized" syslog_key

(* Waits for a daemon to signal startup by writing to a xenstore path
   (optionally with a given value) If this doesn't happen in the timeout then
   an exception is raised *)
let wait_path ~pidalive ~task ~name ~domid ~xs ~ready_path ~timeout ~cancel _ =
  let syslog_key = Printf.sprintf "%s-%d" name domid in
  let watch = Watch.value_to_appear ready_path |> Watch.map (fun _ -> ()) in
  Xenops_task.check_cancelling task ;
  ( try
      let (_ : bool) =
        Cancel_utils.cancellable_watch cancel [watch] [] task ~xs ~timeout ()
      in
      ()
    with Watch.Timeout _ ->
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
    (* | File of (int -> string) *)
    | Path_of of both
end

module type DAEMONPIDPATH = sig
  val name : string

  val pid_location : Pid.path
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

  let pid ~xs domid =
    try
      match D.pid_location with
      | Path_of {file; _} when Sys.file_exists (file domid) ->
          let path = file domid in
          let pid =
            path |> Unixext.string_of_file |> String.trim |> int_of_string
          in
          Unixext.with_file path [Unix.O_RDONLY] 0 (fun fd ->
              try
                Unix.lockf fd Unix.F_TRLOCK 0 ;
                (* we succeeded taking the lock: original process is dead.
                 * some other process might've reused its pid *)
                None
              with Unix.Unix_error (Unix.EAGAIN, _, _) ->
                (* cannot obtain lock: process is alive *)
                Some pid
          )
      | Xenstore key | Path_of {key; _} ->
          (* backward compatibility during update installation: only has
             xenstore pid available *)
          let pid = xs.Xs.read (key domid) in
          Some (int_of_string pid)
    with _ -> None

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
  end)

  module SignalMask = D.SignalMask

  let signal_mask = D.signal_mask

  let start_daemon = D.start_daemon

  let pid = D.pid

  let is_running = D.is_running

  let stop ~xs ~qemu_domid domid =
    match pid ~xs domid with
    | None ->
        () (* nothing to do *)
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
        let file_path = pidfile_path domid in
        best_effort (Printf.sprintf "removing %s" file_path) (fun () ->
            Unix.unlink file_path
        )
end

module Vgpu = struct
  module D = DaemonMgmt (struct
    let name = "vgpu"

    let pid_path domid = Printf.sprintf "/local/domain/%d/vgpu-pid" domid

    let pid_location = Pid.Xenstore pid_path
  end)

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
                 let uuid = Uuidm.to_string (Uuidm.create `V4) in
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
        "--domain=" ^ string_of_int domid
      ; "--vcpus=" ^ string_of_int vcpus
      ; "--suspend=" ^ suspend_file
      ]
      @ device_args
    in
    let fd_arg = if restore then ["--resume"] else [] in
    List.concat [base_args; fd_arg]

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

  let is_running ~xs domid =
    match of_domid domid with
    | None ->
        Compat.is_running ~xs domid
    | Some key ->
        Fe_systemctl.is_active ~service:key

  let stop ~xs domid =
    match (of_domid domid, is_running ~xs domid) with
    | None, true ->
        Compat.stop ~xs domid
    | Some service, true ->
        (* xenstore cleanup is done by systemd unit file *)
        let (_ : Fe_systemctl.status) = Fe_systemctl.stop ~service in
        ()
    | Some service, false ->
        info "Not trying to stop %s since it's not running" service
    | None, false ->
        info "Not trying to stop %s for domid %i since it's not running" D.name
          domid

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
  end)

  let efivars_resume_path =
    Xenops_sandbox.Chroot.Path.of_string ~relative:"efi-vars-resume.dat"

  let efivars_save_path =
    Xenops_sandbox.Chroot.Path.of_string ~relative:"efi-vars-save.dat"

  let start ~xs ~nvram ?(restore = false) task domid =
    let open Xenops_types in
    debug "Preparing to start varstored for UEFI boot (domid=%d)" domid ;
    let path = !Xc_resources.varstored in
    let vm_uuid = Xenops_helpers.uuid_of_domid ~xs domid |> Uuid.to_string in
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
  module D = SystemdDaemonMgmt (struct
    let name = "swtpm-wrapper"

    let pid_path domid = Printf.sprintf "/local/domain/%d/varstored-pid" domid

    (* XXX: the xenstore key is not used, the daemon should define the pidfile *)
    let pid_location = Pid.Xenstore pid_path
  end)

  let xs_path ~domid = Device_common.get_private_path domid ^ "/vtpm"

  let state_path =
    (* for easier compat with dir:// mode, but can be anything.
       If we implement VDI state storage this could be a block device
    *)
    Xenops_sandbox.Chroot.Path.of_string ~relative:"tpm2-00.permall"

  let restore ~domid ~vm_uuid state =
    if String.length state > 0 then (
      let path = Xenops_sandbox.Swtpm_guard.create ~domid ~vm_uuid state_path in
      debug "Restored vTPM for domid %d: %d bytes, digest %s" domid
        (String.length state)
        (state |> Digest.string |> Digest.to_hex) ;
      Unixext.write_string_to_file path state
    ) else
      debug "vTPM state for domid %d is empty: not restoring" domid

  let start ~xs ~vtpm_uuid ~index task domid =
    debug "Preparing to start swtpm-wrapper to provide a vTPM (domid=%d)" domid ;
    let exec_path = !Resources.swtpm_wrapper in
    let name = "swtpm" in
    let vm_uuid = Xenops_helpers.uuid_of_domid ~xs domid |> Uuid.to_string in

    let chroot, _socket_path =
      Xenops_sandbox.Swtpm_guard.start (Xenops_task.get_dbg task) ~vm_uuid
        ~domid ~paths:[]
    in
    let tpm_root =
      Xenops_sandbox.Chroot.(absolute_path_outside chroot Path.root)
    in
    (* the uri here is relative to the chroot path, if chrooting is disabled then
       swtpm-wrapper should modify the uri accordingly.
       xenopsd needs to be in charge of choosing the scheme according to the backend
    *)
    let state_uri =
      Filename.concat "file://"
      @@ Xenops_sandbox.Chroot.chroot_path_inside state_path
    in
    let args = Fe_argv.Add.many [string_of_int domid; tpm_root; state_uri] in
    let args = Fe_argv.run args |> snd |> Fe_argv.argv in
    let timeout_seconds = !Xenopsd.swtpm_ready_timeout in
    let execute = D.start_daemon in
    let service =
      {name; domid; exec_path; chroot; args; execute; timeout_seconds}
    in

    let dbg = Xenops_task.get_dbg task in
    let state =
      Varstore_privileged_client.Client.vtpm_get_contents dbg vtpm_uuid
      |> Base64.decode_exn
    in

    let abs_path =
      Xenops_sandbox.Chroot.absolute_path_outside chroot state_path
    in
    if Sys.file_exists abs_path then
      debug "Not restoring vTPM: %s already exists" abs_path
    else
      restore ~domid ~vm_uuid state ;
    let vtpm_path = xs_path ~domid in

    xs.Xs.write
      (Filename.concat vtpm_path @@ string_of_int index)
      (Uuidm.to_string vtpm_uuid) ;

    start_and_wait_for_readyness ~task ~service ;
    (* return the socket path so qemu can have a reference to it*)
    Xenops_sandbox.Chroot.(
      absolute_path_outside chroot (Path.of_string ~relative:"swtpm-sock")
    )

  let suspend ~xs ~domid ~vm_uuid =
    D.stop ~xs domid ;
    Xenops_sandbox.Swtpm_guard.read ~domid ~vm_uuid state_path

  let stop dbg ~xs ~domid ~vm_uuid ~vtpm_uuid =
    debug "About to stop vTPM (%s) for domain %d (%s)"
      (Uuidm.to_string vtpm_uuid)
      domid vm_uuid ;
    let contents = suspend ~xs ~domid ~vm_uuid in
    let length = String.length contents in
    if length > 0 then (
      debug "Storing vTPM state of %d bytes" length ;
      Varstore_privileged_client.Client.vtpm_set_contents dbg vtpm_uuid
        (Base64.encode_string contents)
    ) else
      debug "vTPM state is empty: not storing" ;
    (* needed to save contents before wiping the chroot *)
    Xenops_sandbox.Swtpm_guard.stop dbg ~domid ~vm_uuid
end

module PV_Vnc = struct
  let pidxenstore_path domid =
    Printf.sprintf "/local/domain/%d/vncterm-pid" domid

  module D = DaemonMgmt (struct
    let name = "vncterm"

    let pid_location = Pid.Xenstore pidxenstore_path
  end)

  let vnc_console_path domid = Printf.sprintf "/local/domain/%d/console" domid

  let vnc_port_path domid =
    Printf.sprintf "/local/domain/%d/console/vnc-port" domid

  let tc_port_path domid =
    Printf.sprintf "/local/domain/%d/console/tc-port" domid

  let pid ~xs domid = D.pid ~xs domid

  (* Look up the commandline args for the vncterm pid; *)
  (* Check that they include the vncterm binary path and the xenstore console
     path for the supplied domid. *)
  let is_cmdline_valid domid pid =
    try
      let cmdline =
        Printf.sprintf "/proc/%d/cmdline" pid
        |> Unixext.string_of_file
        |> Astring.String.cuts ~sep:"\000"
      in
      List.mem !Xc_resources.vncterm cmdline
      && List.mem (vnc_console_path domid) cmdline
    with _ -> false

  let is_vncterm_running ~xs domid =
    match pid ~xs domid with
    | None ->
        false
    | Some p ->
        D.is_running ~xs domid && is_cmdline_valid domid p

  let get_vnc_port ~xs domid =
    if not (is_vncterm_running ~xs domid) then
      None
    else
      try Some (Socket.Port (int_of_string (xs.Xs.read (vnc_port_path domid))))
      with _ -> None

  let get_tc_port ~xs domid =
    if not (is_vncterm_running ~xs domid) then
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

  let start ?statefile ~xs ?ip domid =
    debug "In PV_Vnc.start" ;
    let ip = Option.value ~default:"127.0.0.1" ip in
    let l =
      [
        "-x"
      ; Printf.sprintf "/local/domain/%d/console" domid
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
