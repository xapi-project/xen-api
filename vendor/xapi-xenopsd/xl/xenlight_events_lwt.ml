open Xenlight
open Xenops_utils

module D = Debug.Make(struct let name = "libxl_events_lwt" end)
open D

let xl_async_callback result wakener =
  Lwt.wakeup wakener result

let device_nic_add_async ctx nic domid =
  let t, u = Lwt.wait () in
  Device_nic.add ctx ~async:u nic domid;
  t

let event_loop_lwt ctx =
  let make_watchers fd events for_libxl =
    let watchers = List.flatten (
        List.map (function
            | POLLIN ->
              Lwt_engine.on_readable fd (fun _ ->
                  osevent_occurred_fd ctx for_libxl fd [POLLIN] [POLLIN]
                ) :: []
            | POLLOUT ->
              Lwt_engine.on_writable fd (fun _ ->
                  osevent_occurred_fd ctx for_libxl fd [POLLOUT] [POLLOUT]
                ) :: []
            | _ -> []
          ) events
      ) in
    fd, watchers, for_libxl
  in

  let watch = ref [] in
  let fd_register user fd events for_libxl =
    let watchers = make_watchers fd events for_libxl in
    watch := watchers :: !watch
  in
  let fd_modify user fd events =
    let rec replace = function
      | [] -> []
      | (fd', ev, for_libxl) :: tl when fd' = fd ->
        List.iter Lwt_engine.stop_event ev;
        let watchers = make_watchers fd events for_libxl in
        watchers :: tl
      | hd :: tl -> hd :: replace tl
    in
    watch := replace !watch
  in
  let fd_deregister user fd =
    let rec remove = function
      | [] -> []
      | (fd', ev, for_libxl) :: tl when fd' = fd ->
        List.iter Lwt_engine.stop_event ev;
        tl
      | hd :: tl -> hd :: remove tl
    in
    watch := remove !watch
  in

  Callback.register "xl_async_callback" xl_async_callback;
  Callback.register "xl_event_occurs_callback" xl_event_occurs_callback;
  Callback.register "xl_event_disaster_callback" xl_event_disaster_callback;
  Callback.register "fd_register" fd_register;
  Callback.register "fd_modify" fd_modify;
  Callback.register "fd_deregister" fd_deregister;
  let _ = osevent_register_hooks ctx "userob" in (* the last parameter comes back into the "user" param above *)
  let _ = event_register_callbacks ctx "userob2" in
  ()

