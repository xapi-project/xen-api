(* Test the Updates module *)
open OUnit

(* See xen/xenops_interface.ml for a real example *)
module TestInterface = struct
  let service_name = "test_updates"

  module Dynamic = struct
    type id = Foo of string | Bar of string
    [@@deriving rpc]
  end
end

let scheduler = Scheduler.make ()
let update_a = TestInterface.Dynamic.Foo "a"
let update_b = TestInterface.Dynamic.Foo "b"
let update_c = TestInterface.Dynamic.Foo "c"

module M = Updates.Updates(TestInterface)

(* Tests adding and getting an update *)
let test_add () =
  let u = M.empty scheduler in
  M.add update_a u;
  let (_barriers,updates,_id) = M.get "dbg" None (Some 0) u in
  assert_bool "Update returned" (List.length updates = 1 && List.exists (fun x -> x=update_a) updates)

(* Tests that no updates are returned if none exist *)
let test_noadd () =
  let u = M.empty scheduler in
  let (_barriers,updates,_id) = M.get "dbg" None (Some 0) u in
  assert_bool "Update returned" (List.length updates = 0)

(* Tests that we can remove an update, and that it's not then returned by 'get' *)
let test_remove () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.remove update_a u;
  let (_barriers,updates,_id) = M.get "dbg" None (Some 0) u in
  assert_bool "Update returned" (List.length updates = 0)

(* Tests that, if we specify a timeout, the 'get' call returns the empty
   list after that timeout. *)
let test_timeout () =
  let u = M.empty scheduler in
  let before = Unix.gettimeofday () in
  let (_,l,_) = M.get "dbg" None (Some 1) u in
  let duration = Unix.gettimeofday () -. before in
  assert_bool "Duration greater than 1 sec" (duration > 1.0 && duration < 2.0);
  assert_bool "Returned list was empty" (List.length l = 0)

(* Checks that if we add an event after a blocking 'get' call that the call
   is unblocked. Verifies that the call returns immediately and that the
   correct update was returned. *)
let test_add_after_get () =
  let u = M.empty scheduler in
  let ok = ref false in
  let before = Unix.gettimeofday () in
  let th = Thread.create (fun () ->
      let (_,updates,_) = M.get "dbg" None (Some 0) u in
      ok := List.length updates = 1 && List.exists (fun x -> x=update_a) updates) () in
  M.add update_a u;
  Thread.join th;
  let duration = Unix.gettimeofday () -. before in
  assert_bool "Update returned" (!ok && duration < 0.1)

(* Test injecting a barrier. Adds two updates before injecting a barrier,
   then adds two more events. Checks that the barrier is returned and contains
   the expected 2 updates from before the barrier was injected. Also checks
   that the updates returned from the 'get' contain all 3 updates in the
   correct order *)
let test_inject_barrier () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.add update_b u;
  M.inject_barrier 1 (fun _ _ -> true) u;
  M.add update_a u;
  M.add update_c u;
  let (barriers,updates,_id) = M.get "dbg" None (Some 1) u in
  assert_bool "Barrier returned" (List.length barriers = 1);
  assert_bool "Barriers contains our barrier" (List.exists (fun x -> fst x = 1) barriers);
  let our_barrier = List.hd barriers in
  let barrier_contains u = List.mem u (snd our_barrier) in
  assert_bool "Our barrier contains Foo 'a' and Foo 'b'"
    (barrier_contains update_a && barrier_contains update_b);
  assert_bool "Updates contain all updates"
    (List.nth updates 0 = update_b &&
     List.nth updates 1 = update_a &&
     List.nth updates 2 = update_c)

(* Test the removal of a barrier. Adds a barrier as above, then removes
   it and makes sure it doesn't show up in a subsequent 'get' *)
let test_remove_barrier () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.add update_b u;
  M.inject_barrier 1 (fun _ _ -> true) u;
  M.add update_a u;
  M.add update_c u;
  M.remove_barrier 1 u;
  let (barriers,updates,_id) = M.get "dbg" None (Some 1) u in
  assert_bool "Barrier returned" (List.length barriers = 0);
  assert_bool "Updates contain all updates"
    (List.nth updates 0 = update_b &&
     List.nth updates 1 = update_a &&
     List.nth updates 2 = update_c)

(* This is a similar check to the above, but round-trips through an
   Rpc.t to verify the hand-written t_of_rpc and rpc_of_t functions are
   correct *)
let test_inject_barrier_rpc () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.add update_b u;
  M.inject_barrier 1 (fun _ _ -> true) u;
  M.add update_a u;
  M.add update_c u;
  let (barriers,updates,_id) = M.get "dbg" None (Some 1) u in
  assert_bool "Barrier returned" (List.length barriers = 1);
  assert_bool "Barriers contains our barrier" (List.exists (fun x -> fst x = 1) barriers);
  let our_barrier = List.hd barriers in
  let barrier_contains u = List.mem u (snd our_barrier) in
  assert_bool "Our barrier contains Foo 'a' and Foo 'b'"
    (barrier_contains update_a && barrier_contains update_b);
  assert_bool "Updates contain all updates"
    (List.nth updates 0 = update_b &&
     List.nth updates 1 = update_a &&
     List.nth updates 2 = update_c)

(* Check that the token returned by the first 'get' can be passed to a
   subsequent 'get' invocation. This second one should return only events
   that happend after the first 'get' *)
let test_multiple_gets () =
  let u = M.empty scheduler in
  M.add update_a u;
  let (_,updates1,id) = M.get "dbg" None (Some 1) u in
  M.add update_b u;
  let (_,updates2,_) = M.get "dbg" (Some id) (Some 1) u in
  assert_bool "Updates contain correct updates"
    (List.nth updates1 0 = update_a &&
     List.nth updates2 0 = update_b &&
     List.length updates1 = 1 &&
     List.length updates2 = 1)

(* Test the filter function for filtering out events. Here we add events
   'a' and 'b', then filter out everything but 'a', and check that a
   subsequent 'get' call returns only update 'a' *)
let test_filter () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.add update_b u;
  M.filter (function | Foo "a" -> true | _ -> false) u;
  let (_,updates1,_id) = M.get "dbg" None (Some 1) u in
  assert_bool "Updates contain correct updates"
    (List.nth updates1 0 = update_a &&
     List.length updates1 = 1)

(* Check that a dumped updates.t value has the correct Rpc representation.
   Note that the dumped Rpc contains embedded strings containing json... *)
let test_dump () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.inject_barrier 1 (fun _ _ -> true) u;
  let dump = M.Dump.make u in
  let dumped_rpc = M.Dump.rpc_of_dump dump in
  let expected_rpc = Rpc.Dict
      [("updates",
        Rpc.Enum
          [Rpc.Dict [("id", Rpc.Int 1L); ("v", Rpc.String "[\"Foo\",\"a\"]")]]);
       ("barriers",
        Rpc.Enum
          [Rpc.Enum
             [Rpc.Int 1L; Rpc.Int 2L;
              Rpc.Enum
                [Rpc.Dict
                   [ ("id", Rpc.Int 1L)
                   ; ("v", Rpc.String "[\"Foo\",\"a\"]")
                   ]]]]
       )]
  in
  assert_equal dumped_rpc expected_rpc

(* Test that last_id returns a token that can be passed to 'get'. This get call should
   then only return events that were added _after_ the call to 'last_id' *)
let test_last_id () =
  let u = M.empty scheduler in
  M.add update_a u;
  M.add update_b u;
  let id = M.last_id "dbg" u in
  M.add update_c u;
  let (_,updates,_) = M.get "dbg" (Some id) (Some 1) u in
  assert_bool "Only later events returned"
    (List.length updates = 1 && List.exists (fun x -> x=update_c) updates)

let tests =
  "updates" >:::
  [
    "Test no add" >:: test_noadd;
    "Test add" >:: test_add;
    "Test remove" >:: test_remove;
    "Test timeout" >:: test_timeout;
    "Test add after get" >:: test_add_after_get;
    "Test inject barrier" >:: test_inject_barrier;
    "Test remove barrier" >:: test_remove_barrier;
    "Test inject barrier with rpc" >:: test_inject_barrier_rpc;
    "Test multiple gets" >:: test_multiple_gets;
    "Test filter" >:: test_filter;
    "Test dump" >:: test_dump;
    "Test last_id" >:: test_last_id;
  ]
