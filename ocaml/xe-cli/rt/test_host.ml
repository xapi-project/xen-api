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
(* Util to test various aspects of the new control stack *)

(* Tests:
 * 
 * powerstate - cycle through various power states of the vms 
 * disk       - add and remove disks lots while vm is down
 * vif        - add and remove vifs lots while vm is down
 * param      - set and reset parameters, making sure things are consistent
 *)

let alltests = [
  ("clone",Tests.clone_test);
  ("powerstate",Tests.powerstate);
  ("offlinedisk",Tests.offline_disk);
  ("diskguestverified",Tests.disk_guest_verified);
(*  ("vif",Tests.vif);
  ("onlinevif",Tests.online_vif); *)
  ("importexport",Tests.importexport);
  ("param",Tests.param);
  ("cd",Tests.cd_guest_verified);
(*  ("net", Tests.offline_network); *)]
   
let _ =
  let tests = ref "" in
  let vms = ref "" in
  let all = ref false in
  Arg.parse [
  ("-t",Arg.Set_string tests,"Comma separated lists of tests (no spaces!) tests are: "^(String.concat "," (List.map fst alltests)));
  ("-v",Arg.Set_string vms,"Comma separated list of VMS to run the tests on (no spaces!)");
  ("-i",Arg.Set_string Cliops.iface,"Interface on which to listen for IPs");
  ("-a",Arg.Set all,"Run all the tests") ]
    (fun _ -> raise (Failure "Invalid argument! (try -help for help)"))
    "VM testing utility";
  let cli : Util.t_cli = Util.cli_onhost in 
  let version = Cliops.get_version cli in
  let short_version = Cliops.get_short_version cli in
  let tests = Parsers.explode !tests ',' in
  let tests = if !all then (List.map fst alltests) else tests in
  let vms = if (!all && !vms="") then "debian,debian-pv,windowsxp,windowsxp-pv" else !vms in
  let vms = Parsers.explode vms ',' in
  let vms = List.map (fun vm -> Cliops.get_uuid cli vm) vms in
  (try
      List.iter 
	(fun test -> 
	  let testfn = (List.assoc test alltests) cli in
	  List.iter (fun vm -> if not !Tests.fatal_error then testfn vm) vms) tests;
    with _ -> ());
  Testlog.output_html version "test_log.html";
  Testlog.output_html version (Printf.sprintf "test_log_%s.html" short_version);
  Testlog.output_txt "test_log.txt";
  Testlog.output_xenrt "test_log.xml"

   
    
      
