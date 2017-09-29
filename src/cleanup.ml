open Lwt.Infix

module Xen_api = Xen_api_lwt_unix

module Local_xapi_session = struct
  let wait_for_xapi_and_login () =
    let rpc = Xen_api.make Consts.xapi_unix_domain_socket_uri in
    let rec loop () =
      Lwt.catch
        (fun () -> Xen_api.Session.login_with_password ~rpc ~uname:"" ~pwd:"" ~version:"1.0" ~originator:"xapi-nbd")
        (fun e ->
           Lwt_log.warning_f "Failed to log in via xapi's Unix domain socket: %s; retrying in %f seconds" (Printexc.to_string e) Consts.wait_for_xapi_retry_delay_seconds >>= fun () ->
           Lwt_unix.sleep Consts.wait_for_xapi_retry_delay_seconds >>= fun () ->
           loop ()
        )
    in

    let timeout () =
      let timeout_s = Consts.wait_for_xapi_timeout_seconds in
      Lwt_unix.sleep timeout_s >>= fun () ->
      let msg = Printf.sprintf "Failed to log in via xapi's Unix domain socket in %f seconds" timeout_s in
      Lwt_log.fatal msg >>= fun () ->
      Lwt.fail_with msg
    in

    Lwt_log.notice_f "Will try to log in via xapi's Unix domain socket for %f seconds" Consts.wait_for_xapi_timeout_seconds >>= fun () ->
    Lwt.pick [loop (); timeout ()] >|= fun session_id ->
    (rpc, session_id)

  (** [with_session f] logs in as the local superuser via xapi's local Unix
      domain socket, and takes care to close the session when [f] finishes. It
      keeps retrying the login requests up to
      {!Consts.wait_for_xapi_timeout_seconds} seconds. If it does not manage to
      log in before this timeout, it fails with an exception. It waits for
      {!Consts.wait_for_xapi_retry_delay_seconds} seconds between subsequent
      login attempts. *)
  let with_session f =
    wait_for_xapi_and_login () >>= fun (rpc, session_id) ->
    Lwt.finalize
      (fun () -> f rpc session_id)
      (fun () -> Xen_api.Session.logout ~rpc ~session_id)
end

let ignore_exn_log_error msg t = Lwt.catch t (fun e -> Lwt_log.error (msg ^ ": " ^ (Printexc.to_string e)))

module VBD = struct
  module StringSet = Set.Make(String)

  let cleanup_vbd rpc session_id vbd =
    Xen_api.VBD.unplug ~rpc ~session_id ~self:vbd >>= fun () ->
    Xen_api.VBD.destroy ~rpc ~session_id ~self:vbd

  module Runtime = struct
    let vbds_to_clean_up = ref StringSet.empty
    let vbds_to_clean_up_mutex = Lwt_mutex.create ()

    let with_tracking rpc session_id vbd f =
      Lwt_mutex.with_lock vbds_to_clean_up_mutex (fun () -> Lwt.wrap (fun () ->
          vbds_to_clean_up := StringSet.add vbd !vbds_to_clean_up))
      >>= fun () ->
      f ()
      >>= fun () ->
      Lwt_mutex.with_lock vbds_to_clean_up_mutex (fun () ->
          vbds_to_clean_up := StringSet.remove vbd !vbds_to_clean_up; Lwt.return_unit)

    (* Currently when the program is interrupted with a SIGNAL, we don't
       remove the VBDs that we clean up from the persistent VBD list. However,
       this does not cause a problem, because we ignore exceptions that happen
       during the cleanups. *)
    let cleanup rpc session_id =
      Lwt_log.notice_f "Checking if there are any VBDs to clean up that leaked during runtime" >>= fun () ->
      StringSet.elements !vbds_to_clean_up
      |> Lwt_list.iter_s (fun vbd ->
          ignore_exn_log_error (Printf.sprintf "Caught exception while cleaning up VBD with ref %s" vbd) (fun () ->
              Lwt_log.warning_f "Cleaning up VBD with ref %s" vbd >>= fun () ->
              cleanup_vbd rpc session_id vbd)
        )
  end

  module Persistent = struct

    (* [with_tracking rpc session_id vbd f] guarantees to clean up the VBD
       as long as [vbd] is unplugged and [f] also guarantees the same. *)
    let with_tracking rpc session_id vbd f =
      (* Destroy the VBD and exit with the original exception if we fail in the beginning. *)
      let (>>*=) a b =
        Lwt.try_bind
          (fun () -> a)
          b
          (fun e ->
             Xen_api.VBD.destroy ~rpc ~session_id ~self:vbd >>= fun () ->
             Lwt.fail e)
      in
      Xen_api.VBD.get_uuid ~rpc ~session_id ~self:vbd >>*= fun vbd_uuid ->
      Vbd_store.add vbd_uuid >>*= fun () ->
      Lwt.finalize
        f
        (fun () -> Vbd_store.remove vbd_uuid)

    let cleanup rpc session_id =
      Lwt_log.notice_f "Checking if there are any VBDs to clean up that leaked during the previous run" >>= fun () ->
      Vbd_store.get_all () >>= fun vbd_uuids ->
      Lwt_list.iter_s
        (fun uuid ->
           ignore_exn_log_error (Printf.sprintf "Caught exception while cleaning up VBD with UUID %s" uuid) (fun () ->
               Lwt_log.warning_f "Cleaning up VBD with UUID %s" uuid >>= fun () ->
               Lwt.catch
                 (fun () ->
                    Xen_api.VBD.get_by_uuid ~rpc ~session_id ~uuid >>= fun vbd ->
                    cleanup_vbd rpc session_id vbd)
                 (function
                   | Api_errors.Server_error (e, _) when e = Api_errors.uuid_invalid ->
                     (* This VBD has already been cleaned up, maybe by the signal handler *)
                     Lwt.return_unit
                   | e -> Lwt.fail e)
               >>= fun () ->
               Vbd_store.remove uuid
             )
        )
        vbd_uuids
  end

  let with_vbd ~vDI ~vM ~mode ~rpc ~session_id f =
    Xen_api.VBD.create ~rpc ~session_id ~vM ~vDI ~userdevice:"autodetect" ~bootable:false ~mode ~_type:`Disk ~unpluggable:true ~empty:false ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]
    >>= fun vbd ->
    Persistent.with_tracking rpc session_id vbd (fun () ->
        Runtime.with_tracking rpc session_id vbd (fun () ->
            Lwt.finalize
              (fun () ->
                 Lwt_log.notice_f "Plugging VBD %s" vbd >>= fun () ->
                 Xen_api.VBD.plug ~rpc ~session_id ~self:vbd >>= fun () ->
                 Lwt.finalize
                   (fun () -> f vbd)
                   (fun () ->
                      Lwt_log.notice_f "Unplugging VBD %s" vbd >>= fun () ->
                      Xen_api.VBD.unplug ~rpc ~session_id ~self:vbd)
              )
              (fun () ->
                 Lwt_log.notice_f "Destroying VBD %s" vbd >>= fun () ->
                 Xen_api.VBD.destroy ~rpc ~session_id ~self:vbd
              )
          )
      )
end

module Block = struct
  module Runtime = struct
    let blocks_to_close = Hashtbl.create 1
    let blocks_to_close_mutex = Lwt_mutex.create ()

    let with_tracking b f =
      let block_uuid = Uuidm.v `V4 |> Uuidm.to_string in
      Lwt_mutex.with_lock blocks_to_close_mutex (fun () ->
          Hashtbl.add blocks_to_close block_uuid b; Lwt.return_unit)
      >>= fun () ->
      Lwt.finalize
        f
        (fun () ->
           Lwt_mutex.with_lock blocks_to_close_mutex (fun () ->
               Hashtbl.remove blocks_to_close block_uuid; Lwt.return_unit)
        )

    let cleanup () =
      let cleanup b = ignore_exn_log_error "Caught exception while closing open block device" (fun () ->
          Lwt_log.warning_f "Disconnecting from block device" >>= fun () ->
          Block.disconnect b)
      in
      let blocks_to_close = Hashtbl.fold (fun _ b l -> b::l) blocks_to_close [] in
      Lwt_list.iter_s cleanup blocks_to_close
  end

  let with_block filename f =
    Block.connect filename
    >>= function
    | `Error e -> Lwt.fail_with (Printf.sprintf "Unable to read %s: %s" filename (Nbd.Block_error_printer.to_string e))
    | `Ok b ->
      Runtime.with_tracking b (fun () ->
          Lwt.finalize
            (fun () -> f b)
            (fun () -> Block.disconnect b)
        )
end

module Runtime = struct
  let cleanup_resources signal =
    let cleanup () =
      Lwt_log.warning_f "Caught signal %d, cleaning up" signal >>= fun () ->
      (* First we have to close the open file descriptors corresponding to the
         VDIs we plugged to dom0. Otherwise the VDI.unplug call would hang. *)
      ignore_exn_log_error "Caught exception while closing open block devices" (fun () ->
          Block.Runtime.cleanup ())
      >>= fun () ->
      ignore_exn_log_error "Caught exception while cleaning up VBDs" (fun () ->
          Local_xapi_session.with_session VBD.Runtime.cleanup
        )
    in

    Lwt_main.run (cleanup ());
    failwith (Printf.sprintf "Caught signal %d" signal)

  let register_signal_handler () =
    let signals = [ Sys.sigint; Sys.sigterm ] in
    List.iter
      (fun s -> Lwt_unix.on_signal s cleanup_resources |> ignore)
      signals
end

module Persistent = struct
  let cleanup () =
    Local_xapi_session.with_session VBD.Persistent.cleanup
end
