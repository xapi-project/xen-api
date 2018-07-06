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
open Xenstore_server
open OUnit

let ( |> ) a b = b a
let ( ++ ) a b x = a (b x)
let id x = x

let empty_store () = Store.create ()

let none = Transaction.none

let success f reply =
	match Xs_protocol.get_ty reply with
		| Xs_protocol.Op.Error ->
			failwith (Printf.sprintf "Error: %s" (Xs_protocol.get_data reply))
		| _ -> f reply

let failure f reply =
	match Xs_protocol.get_ty reply with
		| Xs_protocol.Op.Error -> f reply
		| _ ->
			failwith (Printf.sprintf "Expected failure, got success: %s" (Junk.hexify(Bytes.to_string @@ Xs_protocol.to_bytes reply)))

let list f reply = match Xs_protocol.Unmarshal.list reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal string list"

let string f reply = match Xs_protocol.Unmarshal.string reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal string"

let acl f reply = match Xs_protocol.Unmarshal.acl reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal acl"

let int32 f reply = match Xs_protocol.Unmarshal.int32 reply with
	| Some x -> f x
	| None -> failwith "Failed to unmarshal int32"

let equals expected got =
	if expected <> got
	then failwith (Printf.sprintf "Expected %s got %s" expected got)

type result =
	| OK
	| Err of string
	| String of string
	| StringList of (string list -> unit)
	| Perms of (Xs_protocol.ACL.t -> unit)
	| Tid of (int32 -> unit)

let check_result reply = function
	| OK ->
		success ignore reply
	| String which ->
		(success ++ string ++ equals) which reply
	| Err which ->
		(failure ++ string ++ equals) which reply
	| StringList f ->
		(success ++ list) f reply
	| Perms f ->
		(success ++ acl) f reply
	| Tid f ->
		(success ++ int32) f reply

let rpc store c tid payload =
	let request = Xs_protocol.Request.print payload tid 0l in
	Call.reply store c request

let run store (payloads: (Connection.t * int32 * Xs_protocol.Request.payload * result) list) =
	List.iter
		(fun (c, tid, payload, expected_result) ->
			check_result (rpc store c tid payload) expected_result
		) payloads

let test_implicit_create () =
	(* Write a path and check the parent nodes can be read *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let domU = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		(* If a node doesn't exist, everyone gets ENOENT: *)
		dom0, none, PathOp("/a", Read), Err "ENOENT";
		domU, none, PathOp("/a", Read), Err "ENOENT";
		(* If dom0 makes a node, suddenly domU gets EACCES: *)
		dom0, none, PathOp("/a/b", Write "hello"), OK;
		domU, none, PathOp("/a/b", Read), Err "EACCES";
		(* dom0 can also see the implicit path created: *)
		dom0, none, PathOp("/a", Read), OK;
		(* domU gets EACCES: *)
		domU, none, PathOp("/a", Read), Err "EACCES";
	]

let test_directory_order () =
	(* Create nodes in a particular order and check 'directory'
	   preserves the ordering *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/a/1", Write ""), OK;
		dom0, none, PathOp("/a/2/foo", Write ""), OK;
		dom0, none, PathOp("/a/3", Write ""), OK;
		dom0, none, PathOp("/a", Directory), StringList (fun x -> assert_equal ~msg:"directory /a" ~printer:(String.concat ", ") ["1"; "2"; "3"] x);
	]

let example_acl =
	let open Xs_protocol.ACL in
    { owner = 5; other = READ; acl = [ 2, WRITE; 3, RDWR ] }

let test_setperms_getperms () =
	(* Check that getperms(setperms(x)) = x *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write ""), OK;
		dom0, none, PathOp("/foo", Setperms example_acl), OK;
		dom0, none, PathOp("/foo", Getperms), Perms (fun x -> assert_equal ~msg:"perms /foo" ~printer:Xs_protocol.ACL.to_string x example_acl);
	]

let test_setperms_owner () =
	(* Check that only the owner of a node can setperms even
	   if another domain has read/write access *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom2 = Connection.create (Xs_protocol.Domain 2) None in
	let dom5 = Connection.create (Xs_protocol.Domain 5) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write ""), OK;
		dom0, none, PathOp("/foo", Setperms example_acl), OK;
		(* owned by dom5, so dom2 can't setperms *)
		dom2, none, PathOp("/foo", Setperms { example_acl with Xs_protocol.ACL.owner = 2 }), Err "EACCES";
		(* dom5 sets the owner to dom2 *)
		dom5, none, PathOp("/foo", Setperms { example_acl with Xs_protocol.ACL.owner = 2 }), OK;
		(* dom2 sets the owner back to dom5 *)
		dom2, none, PathOp("/foo", Setperms { example_acl with Xs_protocol.ACL.owner = 5 }), OK;
	]

let test_mkdir () =
	(* Check that mkdir creates usable nodes *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/a/b", Read), Err "ENOENT";
		dom0, none, PathOp("/a", Read), Err "ENOENT";
	];
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, PathOp("/bench/local/domain/0", Mkdir), OK;
		dom0, tid, PathOp("/bench/local/domain/0", Setperms example_acl), OK;
		dom0, tid, PathOp("/bench/local/domain/0", Read), OK;
		dom0, tid, Transaction_end true, OK;
	]

let test_empty () =
	(* Check that I can read an empty value *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/a", Write ""), OK;
		dom0, none, PathOp("/a", Read), OK;
	]

let test_directory () =
	()

let test_rm () =
	(* rm of a missing node from an existing parent should succeed *)
	(* rm of a missing node from a missing parent should ENOENT *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/a/b", Rm), Err "ENOENT";
		dom0, none, PathOp("/a", Write "hello"), OK;
		dom0, none, PathOp("/a/b", Rm), OK;
	]

let test_restrict () =
	(* Check that only dom0 can restrict to another domain
	   and that it loses access to dom0-only nodes. *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom3 = Connection.create (Xs_protocol.Domain 3) None in
	let dom7 = Connection.create (Xs_protocol.Domain 7) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write "bar"), OK;
		dom0, none, PathOp("/foo", Setperms example_acl), OK;
		dom3, none, PathOp("/foo", Write "bar"), OK;
		dom7, none, PathOp("/foo", Write "bar"), Err "EACCES";
		dom0, none, Restrict 7, OK;
		dom0, none, PathOp("/foo", Write "bar"), Err "EACCES";
	]

let test_set_target () =
	(* Check that dom0 can grant dom1 access to dom2's nodes,
	   without which it wouldn't have access. *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom7 = Connection.create (Xs_protocol.Domain 7) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/foo", Write "bar"), OK;
		dom0, none, PathOp("/foo", Setperms example_acl), OK;
		dom7, none, PathOp("/foo", Write "bar"), Err "EACCES";
		dom0, none, Set_target(7, 5), OK;
		dom7, none, PathOp("/foo", Write "bar"), OK;
	]

let test_transactions_are_isolated () =
	(* Check that other connections cannot see the nodes created
	   within an uncommitted transaction *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in

	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in

	run store [
		dom0, tid, PathOp("/foo", Write "bar"), OK;
		dom0, none, PathOp("/foo", Read), Err "ENOENT";
		dom0, tid, Transaction_end true, OK;
		dom0, none, PathOp("/foo", Read), OK;
	]

let test_independent_transactions_coalesce () =
	(* Check that two parallel, unrelated transactions can be
	   coalesced properly *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in

	run store [
		dom0, none, PathOp("/a/b", Mkdir), OK;
		dom0, none, PathOp("/1/2", Mkdir), OK;
	];
	let tid_1 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	let tid_2 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid_1, PathOp("/a/b", Write "foo"), OK;
		dom0, tid_2, PathOp("/1/2", Write "foo"), OK;
		dom0, tid_1, Transaction_end true, OK;
		dom0, tid_2, Transaction_end true, OK;
		dom0, none, PathOp("/a/b", Read), String "foo";
		dom0, none, PathOp("/1/2", Read), String "foo";
	]

let test_device_create_coalesce () =
	(* Check that two parallel, device-creating transactions can coalesce *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/local/domain/0/backend/vbd", Mkdir), OK;
		dom0, none, PathOp("/local/domain/1/device/vbd", Mkdir), OK;
		dom0, none, PathOp("/local/domain/2/device/vbd", Mkdir), OK;
	];
	let tid_1 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	let tid_2 = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid_1, PathOp("/local/domain/0/backend/vbd/1/51712", Write "hello"), OK;
		dom0, tid_1, PathOp("/local/domain/1/device/vbd/51712", Write "there"), OK;
		dom0, tid_2, PathOp("/local/domain/0/backend/vbd/2/51712", Write "hello"), OK;
		dom0, tid_2, PathOp("/local/domain/2/device/vbd/51712", Write "there"), OK;
		dom0, tid_1, Transaction_end true, OK;
		dom0, tid_2, Transaction_end true, OK;
		dom0, none, PathOp("/local/domain/0/backend/vbd/1/51712", Read), String "hello";
		dom0, none, PathOp("/local/domain/0/backend/vbd/2/51712", Read), String "hello";
	]

let test_transactions_really_do_conflict () =
	(* Check that transactions that really can't interleave are aborted *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/a", Mkdir), OK;
	];
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, PathOp("/a", Directory), OK;
		dom0, none, PathOp("/a/b", Write "hello"), OK;
		dom0, tid, PathOp("/a/b", Write "there"), OK;
		dom0, tid, Transaction_end true, Err "EAGAIN";
		dom0, none, PathOp("/a/b", Read), String "hello"
	]


let string_of_watch_events watch_events =
	String.concat "; " (List.map (fun (k, v) -> k ^ ", " ^ v) watch_events)

let assert_watches c expected =
	let got = List.rev (Queue.fold (fun acc x -> x :: acc) [] c.Connection.watch_events) in
	assert_equal ~msg:"watches" ~printer:string_of_watch_events expected got

let test_watch_event_quota () =
	(* Check that we can't exceed the per-domain watch event quota *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/number-of-queued-watch-events/1", Write "1"), OK;
		dom0, none, PathOp("/a", Mkdir), OK;
		dom0, none, PathOp("/a", Setperms Xs_protocol.ACL.({ owner = 0; other = RDWR; acl = []})), OK;
	];
	assert_watches dom1 [];
	run store [
		dom1, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom1 [ ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 0 dom1.Connection.nb_dropped_watches;
	(* This watch will be dropped *)
	run store [
		dom0, none, PathOp("/a", Write "hello"), OK;
	];
	assert_watches dom1 [ ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 1 dom1.Connection.nb_dropped_watches;
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/number-of-queued-watch-events/1", Write "2"), OK;
		dom0, none, PathOp("/a", Write "there"), OK;
	];
	assert_watches dom1 [ ("/a", "token"); ("/a", "token") ];
	assert_equal ~msg:"nb_dropped_watches" ~printer:string_of_int 1 dom1.Connection.nb_dropped_watches

let test_simple_watches () =
	(* Check that writes generate watches and reads do not *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/a", Mkdir), OK;
		dom0, none, PathOp("/a", Setperms Xs_protocol.ACL.({ owner = 0; other = RDWR; acl = []})), OK;
	];
	assert_watches dom0 [];
	run store [
		dom0, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* dom0 can see its own write via watches *)
	run store [
		dom0, none, PathOp("/a", Write "foo"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* dom0 can see dom1's writes via watches *)
	run store [
		dom1, none, PathOp("/a", Write "foo"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* reads don't generate watches *)
	run store [
		dom0, none, PathOp("/a", Read), OK;
		dom0, none, PathOp("/a/1", Read), Err "ENOENT";
		dom1, none, PathOp("/a", Read), OK;
		dom1, none, PathOp("/a/1", Read), Err "ENOENT";
	];
	assert_watches dom0 []

let test_relative_watches () =
	(* Check that watches for relative paths *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	(* No watch events are generated without registering *)
	run store [
		dom0, none, PathOp("/local/domain/0/name", Write ""), OK;
		dom0, none, PathOp("/local/domain/0/device", Write ""), OK;
		dom0, none, Watch("device", "token"), OK;
	];
	assert_watches dom0 [ "device", "token" ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	run store [
		dom0, none, PathOp("/local/domain/0/device/vbd", Write "hello"), OK;
	];
	assert_watches dom0 [ "device/vbd", "token" ]

let test_watches_read_perm () =
	(* Check that a connection only receives a watch if it
       can read the node that was modified. *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom1, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom1 [ ("/a", "token") ];
	Queue.clear dom1.Connection.watch_events;
	assert_watches dom1 [];
	run store [
		dom0, none, PathOp("/a", Write "hello"), OK;
		dom1, none, PathOp("/a", Read), Err "EACCES";
	];
	assert_watches dom1 []

let test_transaction_watches () =
	(* Check that watches only appear on transaction commit
	   and not at all in the case of abort *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, Watch ("/a", "token"), OK;
	];
	assert_watches dom0 [ ("/a", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	(* PathOp( Writes in a transaction don't generate watches immediately *)
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, PathOp("/a", Write "hello"), OK;
	];
	assert_watches dom0 [];
	(* If the transaction is aborted then no watches are generated *)
	run store [
		dom0, tid, Transaction_end false, OK
	];
	assert_watches dom0 [];
	(* If the transaction successfully commits then the watches appear *)
	let tid = (success ++ int32) id (rpc store dom0 none Transaction_start) in
	run store [
		dom0, tid, PathOp("/a", Write "hello"), OK;
		dom0, tid, Transaction_end true, OK
	];
	assert_watches dom0 [ ("/a", "token") ]

let test_introduce_watches () =
	(* Check that @introduceDomain watches appear on introduce *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, Watch ("@introduceDomain", "token"), OK;
	];
	assert_watches dom0 [ ("@introduceDomain", "token") ];
	Queue.clear dom0.Connection.watch_events;
	assert_watches dom0 [];
	run store [
		dom0, none, Introduce(5, 5n, 5), OK;
	];
	assert_watches dom0 [ ("@introduceDomain", "token") ]

let test_release_watches () =
	(* Check that @releaseDomain watches appear on introduce *)
	()

let test_recursive_rm_watch () =
	(* Check that rm generates recursive watches *)
	()

let test_no_watch_no_error () =
	(* Check that a write failure doesn't generate a watch *)
	()

let test_bounded_watch_events () =
	(* Check that the per-connection watch event queue is bounded *)
	()

let _test_quota () =
	(* Check that node creation and destruction changes a quota *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	let start = ref 0 in
	let expect n x =
		assert_equal ~msg:"quota" ~printer:string_of_int (!start + n) (int_of_string (List.hd x)) in

	run store [
(*		dom0, none, PathOp("/quota/entries-per-domain/0", Read), StringList (fun x -> start := int_of_string (List.hd x)); *)
		dom0, none, PathOp("/a", Write "hello"), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 1);
		(* Implicit creation of 2 elements *)
		dom0, none, PathOp("/a/b/c", Write "hello"), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 3);
		(* Remove one element *)
		dom0, none, PathOp("/a/b/c", Rm), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 2);
		(* Recursive remove of 2 elements *)
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 0);
		(* Remove an already removed element *)
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 0);
		(* Remove a missing element: *)
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/a", Rm), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 0);
		(* Removing the root node is forbidden *)
		dom0, none, PathOp("/", Rm), Err "EINVAL";
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/0", Read), StringList (expect 0);
	]

let _test_quota_transaction () =
	(* Check that node creation and destruction changes a quota *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let dom2 = Connection.create (Xs_protocol.Domain 2) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	let start = ref 0 in
	let expect n x =
		assert_equal ~msg:"quota" ~printer:string_of_int (!start + n) (int_of_string (List.hd x)) in

	run store [
		dom0, none, PathOp("/local/domain/1", Write ""), OK;
		dom0, none, PathOp("/local/domain/1", Setperms { example_acl with Xs_protocol.ACL.owner = 1 }), OK;
		dom0, none, PathOp("/local/domain/2", Write ""), OK;
		dom0, none, PathOp("/local/domain/2", Setperms { example_acl with Xs_protocol.ACL.owner = 2 }), OK;
		dom1, none, PathOp("/local/domain/1/data/test", Write ""), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), StringList (expect 2);
		dom1, none, PathOp("/local/domain/1/data/test/node0", Write "node0"), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), StringList (expect 3);
		dom2, none, PathOp("/local/domain/2/data/test", Write ""), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), StringList (expect 2);
	];
	let tid = (success ++ int32) id (rpc store dom1 none Transaction_start) in
	run store [
		dom1, tid, PathOp("/local/domain/1/data/test", Rm), OK;
		dom2, none, PathOp("/local/domain/2/data/test/node0", Write "node0"), OK;
		dom1, tid, Transaction_end true, OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), StringList (expect 1);
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), StringList (expect 3);
	]

let _test_quota_setperms () =
	(* Check that one connection cannot exhaust another's quota *)
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	let dom1_quota = ref 0 in
	let dom2_quota = ref 0 in
	let expect quota n x =
		assert_equal ~msg:"quota" ~printer:string_of_int (!quota + n) (int_of_string (List.hd x)) in
	run store [
		dom0, none, PathOp("/local/domain/1", Mkdir), OK;
		dom0, none, PathOp("/local/domain/1", Setperms Xs_protocol.ACL.({owner = 1; other = NONE; acl = []})), OK;
		dom1, none, PathOp("/local/domain/1/private", Mkdir), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), StringList (fun x -> dom1_quota := int_of_string (List.hd x));
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), StringList (fun x -> dom2_quota := int_of_string (List.hd x));
		dom1, none, PathOp("/local/domain/1/private/foo", Write "hello"), OK;
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/1", Read), StringList (expect dom1_quota 1);
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), StringList (expect dom2_quota 0);
		(* Hand this node to domain 2 (who doesn't want it) *)
		dom1, none, PathOp("/local/domain/1/private/foo", Setperms Xs_protocol.ACL.({owner = 2; other = NONE; acl = []})), OK;
		(* Domain 2's quota shouldn't be affected: *)
		dom0, none, PathOp("/tool/xenstored/quota/entries-per-domain/2", Read), StringList (expect dom2_quota 0);
	]

let test_quota_maxsize () =
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom0, none, PathOp("/tool/xenstored/quota/default/entry-length", Write "5"), OK;
		dom0, none, PathOp("/a", Write "hello"), OK;
		dom0, none, PathOp("/a", Write "hello2"), Err "E2BIG";
		dom0, none, PathOp("/tool/xenstored/quota/default/entry-length", Write "6"), OK;
		dom0, none, PathOp("/a", Write "hello2"), OK;
	]

let _test_quota_maxent () =
	let dom0 = Connection.create (Xs_protocol.Domain 0) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		(* Side effect creates the quota entry *)
		dom0, none, PathOp("/first", Write "post"), OK;
		dom0, none, PathOp("/tool/xenstored/quota/default/number-of-entries", Write "1"), OK;
		dom0, none, PathOp("/a", Write "hello"), Err "EQUOTA";
		dom0, none, PathOp("/tool/xenstored/quota/number-of-entries/0", Write "2"), OK;
		dom0, none, PathOp("/a", Write "hello"), OK;
		dom0, none, PathOp("/a", Write "there"), OK;
		dom0, none, PathOp("/b", Write "hello"), Err "EQUOTA";
	]

let test_control_perms () =
	let dom1 = Connection.create (Xs_protocol.Domain 1) None in
	let store = empty_store () in
	let open Xs_protocol.Request in
	run store [
		dom1, none, PathOp("/quota/default/number-of-entries", Write "1"), Err "EACCES";
		dom1, none, PathOp("/tool/xenstored/log/reply-err/ENOENT", Write "1"), Err "EACCES";
	]

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test xenstore server code";

  let suite = "xenstore" >:::
    [

		"test_implicit_create" >:: test_implicit_create;
		"test_directory_order" >:: test_directory_order;
		"getperms(setperms)" >:: test_setperms_getperms;
		"test_setperms_owner" >:: test_setperms_owner;
		"test_mkdir" >:: test_mkdir;
		"test_empty" >:: test_empty;
		"test_rm" >:: test_rm;
		"test_restrict" >:: test_restrict;
		"test_set_target" >:: test_set_target;
		"transactions_are_isolated" >:: test_transactions_are_isolated;
		"independent_transactions_coalesce" >:: test_independent_transactions_coalesce;
		"device_create_coalesce" >:: test_device_create_coalesce;
		"test_transactions_really_do_conflict" >:: test_transactions_really_do_conflict;
		"test_simple_watches" >:: test_simple_watches;
		"test_relative_watches" >:: test_relative_watches;
(*		"test_watches_read_perm" >:: test_watches_read_perm; *)
		"test_transaction_watches" >:: test_transaction_watches;
		"test_introduce_watches" >:: test_introduce_watches;
		(* Currently broken quota tests:
		"test_quota" >:: test_quota;
		"test_quota_transaction" >:: test_quota_transaction;
		"test_quota_setperms" >:: test_quota_setperms;
		"test_quota_maxent" >:: test_quota_maxent;
    *)
		"test_quota_maxsize" >:: test_quota_maxsize;
		"test_watch_event_quota" >:: test_watch_event_quota;
		"test_control_perms" >:: test_control_perms;
	] in
  run_test_tt ~verbose:!verbose suite
