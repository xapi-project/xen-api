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
(* Performance testing *)

open Client
open Perfutil
open Testtypes
open Perfdebug

let xenrtfname = ref "perftest-xenrt.log"

let marshall_xenrt pool metadata results =
  let oc = open_out !xenrtfname in
  Printf.fprintf oc "<testrun>\n";
  Printf.fprintf oc "%s\n" (Scenario.xml_of_scenario pool);
  Printf.fprintf oc " <meta>\n";
  List.iter (fun (k,v) -> Printf.fprintf oc "  <key>%s</key><value>%s</value>\n" k v) metadata;
  Printf.fprintf oc " </meta>\n <tests>\n";
  List.iter (fun r ->
      Printf.fprintf oc "  <test name=\"%s\" subtest=\"%s\">%f</test>\n" r.resultname r.subtest r.xenrtresult) results;
  Printf.fprintf oc " </tests>\n</testrun>";
  close_out oc

let rawfname = ref ""

let marshall_raw (raw_results:Testtypes.result list) =
  if !rawfname <> "" then
    let oc = open_out !rawfname in
    Printf.fprintf oc "%s" (Testtypes.to_string raw_results);
    close_out oc

let marshall pool metadata results =
  marshall_raw results;
  marshall_xenrt pool metadata results

let string_of_set l = Printf.sprintf "{%s}" (String.concat ", " l)

let get_metadata rpc session_id =
  let pool = List.hd (Client.Pool.get_all rpc session_id) in
  let master = Client.Pool.get_master rpc session_id pool in
  let sv = Client.Host.get_software_version rpc session_id master in
  sv

let _ =
  let template_name = ref "sdk-gold" in
  let key = ref "" in
  let scenario = ref "xendesktop" in
  let ipbase = ref 0 in
  let mode = ref "" in
  let run_all = ref false in
  let iter = ref 1 in
  let possible_modes = [ "initpool"; "destroypool"; "run"; "describe"; ] in

  Arg.parse (Arg.align
               [ "-template", Arg.Set_string template_name, Printf.sprintf " Clone VMs from named base template (default is %s)" !template_name;
                 "-scenario", Arg.Set_string scenario,      Printf.sprintf " Choose scenario (default is %s; possibilities are %s"  !scenario (string_of_set (Scenario.get_all ()));
                 "-key", Arg.Set_string key,                               " Key name to identify the Pool instance";
                 "-ipbase", Arg.Set_int ipbase,             Printf.sprintf " Choose base IP address (default is %d for 192.168.%d.1)" !ipbase !ipbase;
                 "-xenrtoutput", Arg.Set_string xenrtfname,                " Set output filename for xenrt (defaults to perftest-xenrt.log)";
                 "-rawoutput", Arg.Set_string rawfname,                    " Set output filename for raw results (by default, do not output results)";
                 "-runall", Arg.Set run_all,                Printf.sprintf " Run tests %s (tests run by default are %s)" (string_of_set Tests.testnames) (string_of_set Tests.runtestnames);
                 "-iter", Arg.Set_int iter,                 Printf.sprintf " Number of iterations (default is %i)" !iter;
               ]) (fun x -> if !mode = "" then mode := x else debug ~out:stderr "Ignoring unexpected argument: %s" x)
    (Printf.sprintf "Configure and run a simulated test\nUsage: %s -key <pool_name> %s" Sys.argv.(0) (string_of_set possible_modes));

  if not(List.mem !mode possible_modes) then begin
    debug ~out:stderr "Unknown mode: \"%s\" (possibilities are %s)" !mode (string_of_set possible_modes);
    exit 1;
  end;
  if not(List.mem !scenario (Scenario.get_all ())) then begin
    debug ~out:stderr "Unknown scenario: \"%s\" (possibilities are %s)" !scenario (string_of_set (Scenario.get_all ()));
    exit 1;
  end;
  if !key = "" then begin
    debug ~out:stderr "Must set a -key to identify the Pool instance";
    exit 1;
  end;

  try
    match !mode with
    | "describe" ->
      let lines = Createpool.describe_pool !template_name !scenario !key in
      List.iter (fun x -> debug "* %s" x) lines
    | _ ->

      let session = Client.Session.login_with_password rpc "root" "xenroot" "1.2" "perftest" in
      let (_: API.string_to_string_map) = get_metadata rpc session in
      let open Xapi_stdext_pervasives in
      Pervasiveext.finally
        (fun () ->
           let pool = Scenario.get !scenario in
           match !mode with
           | "initpool" when pool.Scenario.sdk ->
             Createpool.create_sdk_pool session !template_name !scenario !key !ipbase
           | "initpool" ->
             Createpool.create_pool session !template_name !scenario !key !ipbase
           | "destroypool" when pool.Scenario.sdk ->
             Createpool.destroy_sdk_pool session !template_name !key
           | "destroypool" ->
             debug ~out:stderr "Not yet implemented ... ";
           | "run" ->
             let newrpc = if pool.Scenario.sdk then remoterpc (Printf.sprintf "192.168.%d.1" !ipbase) else rpc in
             let session = if pool.Scenario.sdk then Client.Session.login_with_password newrpc "root" "xensource" "1.2" "perftest" else session in
             Pervasiveext.finally
               (fun () -> marshall pool (get_metadata newrpc session) (Tests.run newrpc session !key !run_all !iter))
               (fun () -> if pool.Scenario.sdk then Client.Session.logout newrpc session)
           | _ -> failwith (Printf.sprintf "unknown mode: %s" !mode)
        ) (fun () -> Client.Session.logout rpc session)
  with Api_errors.Server_error(code, params) ->
    debug ~out:stderr "Caught API error: %s [ %s ]" code (String.concat "; " params)

