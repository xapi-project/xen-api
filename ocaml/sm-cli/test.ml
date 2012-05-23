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
(**
 * @group Storage
 *)

open OUnit

open Listext
open Stringext

open Xmlrpc_client


let default_storage_path = Filename.concat Fhs.vardir "storage"
let default_xapi_path = Filename.concat Fhs.vardir "xapi"
let default_remote_storage_path = Filename.concat Fhs.vardir "storage"
let default_remote_xapi_path = Filename.concat Fhs.vardir "xapi"

let transport = ref (Unix default_storage_path)
let xtransport = ref (Unix default_xapi_path)
let rtransport = ref (Unix default_remote_storage_path)
let rxtransport = ref (Unix default_remote_xapi_path)
let session = ref ""
let rsession = ref ""

module I=struct
        type t=int64 with rpc
        let add=Int64.add
        let sub=Int64.sub
		let zero=Int64.zero
end
module Int64extentlist = ExtentlistSet.ExtentlistSet(I)

type junk_t = (Int64extentlist.t * char) list

let storage_rpc transport session call =
	XMLRPC_protocol.rpc ~srcstr:"sm-cli-test" ~dststr:"smapi2_via_xapi" ~transport
		~http:(xmlrpc ~version:"1.0" ~query:["session_id",session] "/services/SM") call

let xapi_rpc transport call =
	XML_protocol.rpc ~srcstr:"sm-cli-test" ~dststr:"xapi" ~transport
		~http:(xmlrpc ~version:"1.0" "/") call

let rpc = fun call -> storage_rpc !transport !session call
let xrpc = fun call -> xapi_rpc !xtransport call
let rrpc = fun call -> storage_rpc !rtransport !rsession call
let rxrpc = fun call -> xapi_rpc !rxtransport call

let do_nbd url sr vdi dp f =
	let url = Http.Url.of_string url in
 	let path = (Printf.sprintf "/services/SM/nbd/%s/%s/%s" sr vdi dp) in
	let dest_url = Http.Url.set_uri url path in
	let request = Http.Request.make ~query:(Http.Url.get_query_params url) 
		~user_agent:"smtest" Http.Put path in
	let transport = Xmlrpc_client.transport_of_url dest_url in
	with_transport transport (with_http request (fun (response, s) -> f s))

exception Bad_junk

(* Helpers *)
let write_char fd c start len =
	Printf.printf "write_char: %c sector:%Ld nsectors:%d\n%!" c start len;
	if len*512 > Sys.max_string_length then failwith "len too large";
	let s = String.make (len*512) c in
	ignore(Nbd.write fd s (Int64.mul 512L start))
		
let write_junk fd size n current_junk =
	let maxsize = (1024*2) (*Sys.max_string_length*) in
	let maxsizeL = Int64.of_int maxsize in
	let rec inner m cur =
        if m=n then cur else
            let char = Char.chr (Random.int 255) in
            let start = Random.int64 (Int64.sub size maxsizeL) in
            let len = Random.int maxsize in
            write_char fd char start len;
            let myextentlist = Int64extentlist.of_list [(start,Int64.of_int len)] in
            inner (m+1) ((myextentlist,char)::(List.map (fun (extlist,c) -> (Int64extentlist.difference extlist myextentlist, c)) cur))
	in
	inner 0 current_junk
		
let check_junk fd junk =
    let rec inner j =
        match j with 
            | (extentlist,c)::rest ->
                List.iter (fun (start,len64) ->
                    let len = 512 * (Int64.to_int len64) in
                    let s = match Nbd.read fd (Int64.mul start 512L) (Int32.of_int len) with Some s -> s | None -> failwith "Failed to read" in
                    let check = String.make len c in
					Printf.printf "Checking %d at sector offset %Ld, size %Ld sectors\n" (Char.code c) start len64;
                    if String.compare s check <> 0 then raise Bad_junk)
					(Int64extentlist.to_list extentlist);
                inner rest
            | _ -> ()
    in
    inner junk


		
(**open Storage_interface*)
module SMClient = Storage_interface.Client(struct let rpc = rpc end)
module RSMClient = Storage_interface.Client(struct let rpc = rrpc end)
module XapiClient = Client.Client

open Storage_interface

let dbg = "sm-test"

let mib = Int64.mul 1024L 1024L

let usage_and_exit () =
	Printf.fprintf stderr "Usage:\n";
	Printf.fprintf stderr "  %s <SR>" Sys.argv.(0);
	exit 1

let find_vdi_in_scan sr vdi =
	let results = SMClient.SR.scan ~dbg ~sr in
	try
		Some (List.find (fun x -> x.vdi = vdi) results)
	with Not_found ->
		None

let test_query sr _ = let (_: query_result) = SMClient.Query.query ~dbg in ()

let missing_vdi = "missing"

let test_scan_missing_vdi sr _ =
	match find_vdi_in_scan sr missing_vdi with
		| Some vdi -> failwith (Printf.sprintf "SR.scan found a VDI that was supposed to be missing: %s" (string_of_vdi_info vdi))
		| None -> ()
			
let test_destroy_missing_vdi sr _ =
	try
		SMClient.VDI.destroy ~dbg ~sr ~vdi:missing_vdi;
		failwith "VDI.destroy unexpectedly succeeded"
	with 
		| Vdi_does_not_exist -> ()
		| x -> failwith (Printf.sprintf "Unexpected result from VDI.destroy: %s\n" (Printexc.to_string x))

let vdi_info_assert_equal vdi_info vdi_info' =
	assert_equal ~msg:"name_label" ~printer:(fun x -> x) vdi_info.name_label vdi_info'.name_label;
	assert_equal ~msg:"name_description" ~printer:(fun x -> x) vdi_info.name_description vdi_info'.name_description;
	assert_equal ~msg:"ty" ~printer:(fun x -> x) (String.lowercase vdi_info.ty) (String.lowercase vdi_info'.ty);
	assert_equal ~msg:"metadata_of_pool" ~printer:(fun x -> x) vdi_info.metadata_of_pool vdi_info'.metadata_of_pool;
	assert_equal ~msg:"is_a_snapshot" ~printer:string_of_bool vdi_info.is_a_snapshot vdi_info'.is_a_snapshot;
	assert_equal ~msg:"snapshot_time" ~printer:(fun x -> x) vdi_info.snapshot_time vdi_info'.snapshot_time;
	assert_equal ~msg:"snapshot_of" ~printer:(fun x -> x) vdi_info.snapshot_of vdi_info'.snapshot_of;
	assert_equal ~msg:"read_only" ~printer:string_of_bool vdi_info.read_only vdi_info'.read_only

let example_vdi_info =
	let name_label = "test_name_label" in
	let name_description = "test_name_description" in
	let ty = "ephemeral" in
	let metadata_of_pool = "mop" in
	let is_a_snapshot = true in
	let snapshot_time = "19700101T00:00:00Z" in
	let snapshot_of = "sof" in
	let read_only = false in
	let virtual_size = Int64.mul 8L mib in
	let physical_utilisation = 0L in
	{
		vdi = "";
		sr = "";
		content_id = "";
		name_label = name_label;
		name_description = name_description;
		ty = ty;
		metadata_of_pool = metadata_of_pool;
		is_a_snapshot = is_a_snapshot;
		snapshot_time = snapshot_time;
		snapshot_of = snapshot_of;
		read_only = read_only;
		virtual_size = virtual_size;
		physical_utilisation = physical_utilisation;
	}

let test_create_destroy sr _ =
	let vdi_info = example_vdi_info in
	let vdi_info' = 
		let vdi_info' = SMClient.VDI.create ~dbg ~sr ~vdi_info ~params:[] in
		vdi_info_assert_equal vdi_info vdi_info';
		vdi_info'
	in
	begin match find_vdi_in_scan sr vdi_info'.vdi with
		| None -> failwith (Printf.sprintf "SR.scan failed to find vdi: %s" (string_of_vdi_info vdi_info'))
		| Some vdi_info'' -> vdi_info_assert_equal vdi_info' vdi_info''
	end;
	SMClient.VDI.destroy ~dbg ~sr ~vdi:vdi_info'.vdi;
	begin match find_vdi_in_scan sr vdi_info'.vdi with
		| Some vdi_info''' -> failwith (Printf.sprintf "SR.scan found a VDI that was just deleted: %s" (string_of_vdi_info vdi_info'''))
		| None -> ()
	end

let test_attach_activate url sr _ =
	let vdi_info = SMClient.VDI.create ~dbg ~sr ~vdi_info:example_vdi_info ~params:[] in
	let dp = "test_attach_activate" in
	let _ = SMClient.VDI.attach ~dbg ~sr ~dp ~vdi:vdi_info.vdi ~read_write:true in
	SMClient.VDI.activate ~dbg ~sr ~dp ~vdi:vdi_info.vdi;
	do_nbd url sr vdi_info.vdi dp (fun s ->
		let (size,_) = Nbd.negotiate s in
		Printf.printf "size=%Ld\n" size;
		let secsize = Int64.div size 512L in
		let junk = write_junk s secsize 10 [(Int64extentlist.of_list [(0L,secsize)],Char.chr 0)] in
		check_junk s junk;
		let junk2 = 
			let (e,c) = List.hd junk in 
			let newc = if c='x' then 'y' else 'x' in
			List.rev ((e,newc)::List.tl junk)
		in
		let success = try check_junk s junk2; false with _ -> true in
		if success 
		then Printf.printf "OK\n"
		else Printf.printf "Not OK!\n"
	);
	SMClient.VDI.destroy ~dbg ~sr ~vdi:vdi_info.vdi

let test_mirror_1 url sr rurl rsr _ =
	let vdi_info = SMClient.VDI.create ~dbg ~sr ~vdi_info:example_vdi_info ~params:[] in
	let dp = "test_attach_activate" in
	ignore(SMClient.VDI.attach ~dbg ~sr ~dp ~vdi:vdi_info.vdi ~read_write:true);
	SMClient.VDI.activate ~dbg ~sr ~dp ~vdi:vdi_info.vdi;
	let junk = do_nbd url sr vdi_info.vdi dp (fun s ->
		let (size,_) = Nbd.negotiate s in
		Printf.printf "size=%Ld\n" size;
		let secsize = Int64.div size 512L in
		let junk = write_junk s secsize 10 [(Int64extentlist.of_list [(0L,secsize)],Char.chr 0)] in
		check_junk s junk;
		let junk2 = 
			let (e,c) = List.hd junk in 
			let newc = if c='x' then 'y' else 'x' in
			List.rev ((e,newc)::List.tl junk)
		in
		let success = try check_junk s junk2; false with _ -> true in
		(if success 
		then Printf.printf "OK\n"
		else Printf.printf "Not OK!\n");
		junk		
	) in

	(* At this point, we have a VDI containing data with which we can mirror *)

	let task = SMClient.DATA.MIRROR.start ~dbg ~sr ~vdi:vdi_info.vdi ~dp ~url:rurl ~dest:rsr in
	
	let finished = function 
		| Task.Pending _ -> false
		| _ -> true
	in

	while not (finished (SMClient.TASK.stat ~dbg ~task).Task.state) do
		Thread.delay 5.0;
	done;

	let task = SMClient.TASK.stat ~dbg ~task in
	let mirror_id = match task.Task.state with
		| Task.Completed { Task.result=Some (Mirror_id m) } -> m
		| _ -> failwith "Mirror failed" 
	in
	
	let mirror = SMClient.DATA.MIRROR.stat ~dbg ~id:mirror_id in
	let remote_vdi = mirror.Mirror.dest_vdi in

	let junk = do_nbd url sr vdi_info.vdi dp (fun s ->
		let (size,_) = Nbd.negotiate s in
		Printf.printf "size=%Ld\n" size;
		let secsize = Int64.div size 512L in
		let junk = write_junk s secsize 10 junk in
		check_junk s junk;
		let junk2 = 
			let (e,c) = List.hd junk in 
			let newc = if c='x' then 'y' else 'x' in
			List.rev ((e,newc)::List.tl junk)
		in
		let success = try check_junk s junk2; false with _ -> true in
		(if success 
		then Printf.printf "OK\n"
		else Printf.printf "Not OK!\n");
		junk		
	) in


	
	(*  *)

	SMClient.VDI.deactivate ~dbg ~sr ~dp ~vdi:vdi_info.vdi;
	SMClient.VDI.detach ~dbg ~sr ~dp ~vdi:vdi_info.vdi;
	SMClient.VDI.destroy ~dbg ~sr ~vdi:vdi_info.vdi;
	ignore(RSMClient.VDI.attach ~dbg ~sr:rsr ~dp ~vdi:remote_vdi ~read_write:true);
	RSMClient.VDI.activate ~dbg ~sr:rsr ~dp ~vdi:remote_vdi;
	do_nbd rurl rsr remote_vdi dp (fun s ->
		let (size,_) = Nbd.negotiate s in
		Printf.printf "Mirror VDI: size=%Ld\n" size;
		check_junk s junk;
		let junk2 = 
			let (e,c) = List.hd junk in 
			let newc = if c='x' then 'y' else 'x' in
			List.rev ((e,newc)::List.tl junk)
		in
		let success = try check_junk s junk2; false with _ -> true in
		if success 
		then Printf.printf "Mirror VDI: OK\n"
		else Printf.printf "Mirror VDI: Not OK!\n"
	);
	RSMClient.VDI.destroy ~dbg ~sr:rsr ~vdi:remote_vdi

let _ =
	let verbose = ref false in
	let sr = ref "" in
	let rsr = ref "" in
	let host = ref "localhost" in
	let host2 = ref "localhost" in
	let username = ref "root" in
	let password = ref "xenroot" in

	Arg.parse [
		"-sr", Arg.Set_string sr, "Specify SR";
		"-rsr", Arg.Set_string rsr, "Specify remote SR";
		"-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
		"-host", Arg.Set_string host, "Host to connect to (defaults to localhost)";
		"-host2", Arg.Set_string host2, "Second host to connect to for mirroring (can be the same as host1)";
		"-username", Arg.Set_string username, "Xapi username";
		"-password", Arg.Set_string password, "Xapi password";
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Test via storage backend";

	if !sr = "" then failwith "Please supply -sr argument";

	rxtransport := (TCP (!host2, 80));
	rtransport := (TCP (!host2, 80));

	let localsession = XapiClient.Session.login_with_password xrpc !username !password "1.0" in
	session := Ref.string_of localsession;

	let remotesession = XapiClient.Session.login_with_password rxrpc !username !password "1.0" in
	rsession := Ref.string_of remotesession;

	let url = Printf.sprintf "http://%s/services/SM?session_id=%s" !host !session in
	let rurl = Printf.sprintf "http://%s/services/SM?session_id=%s" !host2 !rsession in

	Pervasiveext.finally (fun () -> 
		let suite = "Storage test" >::: 
			[
				"test_query" >:: (test_query !sr);
				"test_scan_missing_vdi" >:: (test_scan_missing_vdi !sr);
				"test_destroy_missing_vdi" >:: (test_destroy_missing_vdi !sr);
				"test_create_destroy" >:: (test_create_destroy !sr);
				"test_attach_activate" >:: (test_attach_activate url !sr);
				"test_mirror" >:: (test_mirror_1 url !sr rurl !rsr);
			] in
		
		run_test_tt ~verbose:!verbose suite
			) (fun () -> ())

