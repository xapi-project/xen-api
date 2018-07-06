open! Core
open! Import

include Core.Weak_hashtbl

let create ?growth_allowed ?size hashable =
  let t = create ?growth_allowed ?size hashable in
  (* To avoid having keys around that should be cleaned, we must ensure that after any
     call to [thread_safe_f], there is a call to
     [reclaim_space_for_keys_with_unused_data].  We do this via [reclaim_will_happen],
     which, if [true], guarantees that a call to [reclaim_space_for_keys_with_unused_data]
     will happen in the future.  It is OK if we have multiple reclaims extant
     simultaneously, since they are async jobs. *)
  let reclaim_will_happen = ref false in
  let reclaim () =
    reclaim_will_happen := false;
    reclaim_space_for_keys_with_unused_data t;
  in
  set_run_when_unused_data t ~thread_safe_f:(fun () ->
    if not !reclaim_will_happen then begin
      reclaim_will_happen := true;
      let module Scheduler = Kernel_scheduler in
      let scheduler = Scheduler.t () in
      Scheduler.thread_safe_enqueue_external_job scheduler
        Scheduler.main_execution_context reclaim ();
    end);
  t
;;

let remove_keys_with_unused_data `Do_not_use = assert false
let set_run_when_unused_data     `Do_not_use = assert false

let%test_unit _ = (* automatic reclamation, multiple times *)
  Thread_safe.block_on_async_exn (fun () ->
    let t = create (module Int) in
    let heap_block i = Heap_block.create_exn (ref i) in
    let b1 = heap_block 1 in
    let key1 = 13 in
    let key2 = 14 in
    ignore (find_or_add t key1 ~default:(fun () -> b1) : _ Heap_block.t);
    ignore (find_or_add t key2 ~default:(fun () -> heap_block 2) : _ Heap_block.t);
    Gc.full_major ();
    after (sec 0.) (* let a cycle happen, to do the reclamation *)
    >>= fun () ->
    assert (phys_equal (Option.value_exn (find t key1)) b1);
    assert (Option.is_none (find t key2));
    assert (not (key_is_using_space t key2));
    let key3 = 15 in
    ignore (find_or_add t key3 ~default:(fun () -> heap_block 3) : _ Heap_block.t);
    Gc.full_major ();
    after (sec 0.) (* let a cycle happen, to do the reclamation *)
    >>= fun () ->
    assert (Option.is_none (find t key3));
    assert (not (key_is_using_space t key3));
    return ())
;;
