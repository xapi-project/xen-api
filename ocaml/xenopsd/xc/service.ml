module D = Debug.Make (struct let name = "service" end)

open! D
module Unixext = Xapi_stdext_unix.Unixext
module Xenops_task = Xenops_task.Xenops_task
module Chroot = Xenops_sandbox.Chroot
module Path = Chroot.Path

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

let alive service =
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
                if alive service_name then
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
