(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* Test case 8700 - concurrency testing of coalesce *)

open Client
open Threadext
open Lvhdrt_exceptions

(* Introduce long delays in the critical parts of the coalesce process
   via FIST points, and while the coalesce process is proceeding try
   other operations to ensure they are blocked. The operations that
   should be blocked are VDI.create, VDI.resize, VDI.clone and SR.scan
   according to the concurrency doc on the Vancouver wiki. VBD
   pause/unpause aren't currently mentioned, need to verify whether
   they're allowed or not.

   Test for a particular op for a delay in a particular point:

   1. Create VDI and attach to dom0
   2. Write pattern 'A' to the disk
   3. Detach VBD
   4. Clone the VDI
   5. Attach the VDI to dom 0
   6. Write pattern 'B' to the disk
   7. Detach the VBD
   8. Clone the VDI
   9. Reattach the VBD
   10. Write pattern 'C' to the disk
   11. Delete the second clone
   12. Kick off coalesce with a SR.scan, with FIST induced delay
   13. Attempt to perform op, ensure it is blocked.
   14. After the FIST induced delay verify that the blocked operation completes properly

   This test is run for the matrix of the operations listed above and
   the FIST points in the various parts of the coalesce process.
*)

let fist_induced_delay_time = 60.0

let fist_name_of_int i =
  match i with
    | 0 -> Fists.finding_suitable_pair
    | 1 -> Fists.inflating_parent
    | 2 -> Fists.resizing_while_paused
    | 3 -> Fists.coalescing_data
    | 4 -> Fists.relinking_granchildren
    | 5 -> Fists.die
    | _ -> raise (Test_error "Unknown fist point!")

let wait_for_fist rpc session sr fist_num =
  let fist = fist_name_of_int fist_num in
  Utils.wait_for_fist rpc session sr fist

let fist rpc session host create i = 
  let name = fist_name_of_int i in
  let fn = if create then "mkfistpoint" else "rmfistpoint" in
  let (_: string) = Client.Host.call_plugin ~rpc ~session_id:session ~host ~plugin:Globs.helper_plugin ~fn
    ~args:[("fist_point",name)] in
  ()
    
let remove_fist_keys rpc session sr =
  for i=0 to 5 do
    Client.SR.remove_from_other_config rpc session sr (fist_name_of_int i)
  done



type ops = 
    | SrScan
    | VbdPlug
    | VdiClone
    | VdiResize
    | VdiCreate
    | VdiSnapshot

let string_of_op = function
  | SrScan -> "SrScan"
  | VbdPlug -> "VbdPlug"
  | VdiClone -> "VdiClone"
  | VdiResize -> "VdiResize"
  | VdiCreate -> "VdiCreate"
  | VdiSnapshot -> "VdiSnapshot"

let blocked_ops_to_check = 
  [ (0, []);
    (1, [SrScan; VdiClone; VdiResize; VdiCreate; VdiSnapshot]);
    (2, [(*SrScan; VdiClone; VdiResize; VdiCreate; VdiSnapshot; VbdPlug*)]);
    (3, []); 
    (4, [SrScan; VdiClone; VdiResize; VdiCreate; VdiSnapshot]) ]


let do_op rpc session vdi vdi3 host op =
  match op with 
    | SrScan ->
	(* Assume VDI and VDI3 are in the same SR...!*)
	let sr = Client.VDI.get_SR rpc session vdi in
	let (_,time) = Utils.time (fun () -> Client.SR.scan rpc session sr) () in
	Utils.debug "Time taken: %f" time;
	time > fist_induced_delay_time
    | VdiClone ->
	let (res,time) = Utils.time (fun () -> Client.VDI.clone rpc session vdi []) () in
	Client.VDI.destroy rpc session res;
	Utils.debug "Time taken: %f" time;
	time > fist_induced_delay_time
    | VdiResize ->
	let (res,time) = Utils.time (fun () -> Client.VDI.resize rpc session vdi Globs.thirtytwo_megs) () in
	Utils.debug "Time taken: %f" time;
	time > fist_induced_delay_time
    | VdiCreate ->
	(* Assume VDI and VDI3 are in the same SR...!*)
	let sr = Client.VDI.get_SR rpc session vdi in
	let (res,time) = Utils.time (fun () -> Client.VDI.create ~rpc ~session_id:session ~name_label:"LVHD test" 
	  ~name_description:"Test for coalescing purposes" ~sR:sr ~virtual_size:Globs.four_megs ~_type:`ephemeral (*!*)
	  ~sharable:false ~read_only:false ~other_config:[] ~xenstore_data:[] ~sm_config:[] ~tags:[]) () in
	Client.VDI.destroy rpc session res;
	Utils.debug "Time taken: %f" time;
	time > fist_induced_delay_time
    | VdiSnapshot ->
	let (res,time) = Utils.time (fun () -> Client.VDI.snapshot rpc session vdi []) () in
	Client.VDI.destroy rpc session res;
	Utils.debug "Time taken: %f" time;
	time > fist_induced_delay_time
    | VbdPlug ->
	let time1 = ref 0.0 in
	let time2 = ref 0.0 in
	let t1 = Thread.create (fun () -> 
	  let (_,time) = Utils.time (fun () ->
	    Utils.with_attached_vdi rpc session vdi (fun real_device vbd -> Utils.debug "I've attached as device: %s" real_device)) ()
	  in time1 := time) () 
	in
				
	let t2 = Thread.create (fun () -> 
	  let (_,time) = Utils.time (fun () ->
	    Utils.with_attached_vdi rpc session vdi3 (fun real_device vbd -> Utils.debug "I've attached as device: %s" real_device)) ()
	  in time2 := time) () 
	in
    
	Thread.join t1;
	Thread.join t2;

	Utils.debug "Times taken: %f,%f" !time1 !time2;

	!time1 > fist_induced_delay_time || !time2 > fist_induced_delay_time

let check_pattern rpc session vdi master =
  Utils.with_attached_vdi rpc session vdi (fun real_device vbd -> 
    let check = Client.Host.call_plugin ~rpc ~session_id:session ~host:master ~plugin:Globs.helper_plugin ~fn:"pattern" 
      ~args:[("dev","/dev/"^real_device); ("size",(Int64.to_string Globs.eight_megs)); ("action","read"); ("type","1"); ("variant","2")];
    in
    check <> "FAIL"
  )

let do_check_1 rpc session sr fist_num op =
  Utils.debug "Checking that activating fistpt: %s blocks operation: %s" (fist_name_of_int fist_num) (string_of_op op);

  let vdis_before = Client.SR.get_VDIs rpc session sr in

  let master = Utils.get_master rpc session in

  Utils.debug "Setting up initial disks";

  let (vdi,vdi2,vdi3) = Utils.create_vdi_tree rpc session sr "LVHD test" Globs.eight_megs Globs.eight_megs ~resize:Globs.sixteen_megs in
  
  Utils.debug "Delaying to ensure we're in a stable state....";

  Thread.delay (fist_induced_delay_time *. 1.0);

  Utils.debug "Setting fist point";

  fist rpc session master true fist_num;

  Utils.debug "Destroying VDI2 in order to create coalescable VHD";

  Client.VDI.destroy ~rpc ~session_id:session ~self:vdi2;

  Utils.debug "Waiting for fist point to fire";
  let (_: bool) = wait_for_fist rpc session sr fist_num in
  Utils.debug "Got it!";

  let success = do_op rpc session vdi vdi3 master op in

  Utils.debug "Removing the FIST point ";

  fist rpc session master false fist_num;

  Utils.debug "Checking that the pattern is correct";

  let success = success && check_pattern rpc session vdi master in

  Utils.debug "Success=%b" success;

  (* Tidy up - destroy the VDIs *)
  Utils.debug "Tidying up...";
  Client.VDI.destroy rpc session vdi;
  Client.VDI.destroy rpc session vdi3;
  Client.SR.scan ~rpc ~session_id:session ~sr;

  Thread.delay (fist_induced_delay_time *. 1.0);

  let vdis_after = Client.SR.get_VDIs rpc session sr in
  if List.length vdis_after <> List.length vdis_before then failwith "Some VDIs left over!";

  success


let do_check_2 rpc session sr =
  (* Construct some VDIs for coalescing, but have fist-points that will cause the coalese to fail multiple times *)
  Utils.debug "Setting up initial disks";

  let master = Utils.get_master rpc session in

  let (vdi,vdi2,vdi3) = Utils.create_vdi_tree rpc session sr "LVHD test" Globs.eight_megs Globs.eight_megs ~resize:Globs.sixteen_megs in

  remove_fist_keys rpc session sr;

  fist rpc session master true 4; (* Set the 4th fist point - make sure we don't complete the coalesce *)
  fist rpc session master true 5; (* Set the 5th fist point - die on seeing fist point *)

  Utils.debug "Destroying VDI2 in order to create coalescable VHD";

  fist rpc session master true 0;

  Client.VDI.destroy ~rpc ~session_id:session ~self:vdi2;

  let rec doit n =
    if n>20 then () else begin
      let fist_num = n mod 4 in (* Nb, we'll often fall through to the fourth fist point... *)
      let old_fist_num = (n+3) mod 4 in

      Utils.debug "Setting fist pt %d" fist_num;

      fist rpc session master true fist_num;
      fist rpc session master false old_fist_num;
            
      Client.SR.scan rpc session sr; (* Occasionally this thinks a scan is still in progress... odd *)
      
      if not (wait_for_fist rpc session sr fist_num) then Utils.debug "fist point didn't activate!";
      
      Utils.debug "Coalesce aborted!";
      
      doit (n+1)
    end
  in 
  
  doit 0;

  Utils.debug "Finished...";

  let vdis_before = Client.SR.get_VDIs rpc session sr in

  remove_fist_keys rpc session sr;
  fist rpc session master false 0;
  fist rpc session master false 1;
  fist rpc session master false 2;
  fist rpc session master false 3;
  fist rpc session master false 4;
  fist rpc session master false 5;

  Client.SR.scan rpc session sr;

  Thread.delay 100.0;

  let success = check_pattern rpc session vdi master in
  
  let vdis_after = Client.SR.get_VDIs rpc session sr in

  let vdis_gone = List.length vdis_before - List.length vdis_after in

  Utils.debug "VDIs gone: %d" vdis_gone;

  vdis_gone=1 && success

let run rpc session =
  (* Find an LVHD SR *)
  let sr = Utils.find_lvhd_sr rpc session in
  let master = Utils.get_master rpc session in

  Pervasiveext.finally (fun () -> 
    Utils.debug "Found LVHD SR";
    Utils.debug "Creating first VDI";
    
    List.iter (fun (fist,ops) ->
      List.iter (fun op -> 
	let success = do_check_1 rpc session sr fist op in 
	if success then 
	  Utils.debug "SUCCESS"
	else begin
	  Utils.debug "FAILED";
	  failwith "Failure!"
	end    
      ) ops
    ) blocked_ops_to_check;
    
    let success = do_check_2 rpc session sr in
    
    if not success then failwith "Failed")
    (fun () ->
      fist rpc session master false 0;
      fist rpc session master false 1;
      fist rpc session master false 2;
      fist rpc session master false 3;
      fist rpc session master false 4;
      fist rpc session master false 5)
