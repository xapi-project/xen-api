open Core
open Import

let create ?message ?close_on_exec ?unlink_on_exit path =
  In_thread.run (fun () ->
    Core.Lock_file.create ?message ?close_on_exec ?unlink_on_exit path)
;;

let create_exn ?message ?close_on_exec ?unlink_on_exit path =
  create ?message ?close_on_exec ?unlink_on_exit path
  >>| fun b ->
  if not b then failwiths "Lock_file.create" path [%sexp_of: string]
;;

let random = lazy (Random.State.make_self_init ())
;;

let repeat_with_abort ~abort ~f =
  Deferred.repeat_until_finished () (fun () ->
    f ()
    >>= function
    | true  -> return (`Finished `Ok)
    | false ->
      let delay = sec (Random.State.float (Lazy.force random) 0.3) in
      choose [ choice (after delay) (fun () -> `Repeat)
             ; choice abort         (fun () -> `Abort)
             ]
      >>| function
      | `Abort -> `Finished `Aborted
      | `Repeat -> `Repeat ())
;;

let fail_on_abort path = function
  | `Ok -> ()
  | `Aborted ->
    failwiths "Lock_file timed out waiting for existing lock" path [%sexp_of: string]
;;

let waiting_create
      ?(abort = Deferred.never ()) ?message ?close_on_exec ?unlink_on_exit path =
  repeat_with_abort ~abort
    ~f:(fun () -> create ?message ?close_on_exec ?unlink_on_exit path)
  >>| fail_on_abort path
;;

let is_locked path = In_thread.run (fun () -> Core.Lock_file.is_locked path)

module Nfs = struct
  let get_hostname_and_pid path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.get_hostname_and_pid path)
  ;;

  let get_message path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.get_message path)
  ;;

  let unlock_exn path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.unlock_exn path)
  ;;

  let unlock path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.unlock path)
  ;;

  let create ?message path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.create ?message path)
  ;;

  let create_exn ?message path =
    In_thread.run (fun () -> Core.Lock_file.Nfs.create_exn ?message path)
  ;;

  let waiting_create ?(abort = Deferred.never ()) ?message path =
    repeat_with_abort ~abort ~f:(fun () ->
      create ?message path
      >>| function
      | Ok ()   -> true
      | Error _ ->
        false)
    >>| fail_on_abort path
  ;;

  let critical_section ?message path ~abort ~f =
    waiting_create ~abort ?message path
    >>= fun () ->
    Monitor.protect f ~finally:(fun () -> unlock_exn path)
  ;;
end
