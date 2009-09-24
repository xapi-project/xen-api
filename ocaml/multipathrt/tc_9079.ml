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
(* Path fail-over time on FC multipath *)

open Listext
open Pervasiveext
open Client
open Utils
open Hba_utils

let max_failover_time = 50. (* seconds *)

let run_fc rpc session =
  Random.self_init();

  let master = get_master rpc session in
  if not(is_multipathing_enabled rpc session master) then enable_multipathing rpc session master;

  let sr = get_fc_sr rpc session in
  debug "Found FibreChannel SR: %s" (Ref.string_of sr);
  let scsiid = get_fc_scsiid rpc session sr in
  debug "SCSIid is %s" scsiid;

  let num_paths = get_num_paths rpc session sr scsiid in
  debug "PBD.other-config indicates that there is a maximum of %d paths to this SR" num_paths;

  debug "Creating VDI in FibreChannel SR...";
  let vdi = create_test_vdi rpc session sr in
  finally
    (fun () ->
      debug "Attaching VDI to dom0...";
      let vbd = plug_vdi_to_dom0 rpc session vdi in

      let time1 = time_data_transfer rpc session master vbd in
      debug " ** Duration of dd (with %d of %d paths) was %f seconds **" num_paths num_paths time1;

      finally
        (fun () ->
          modify_fc_paths_manual rpc session master true;

          finally
            (fun () ->
              let time2 = time_data_transfer rpc session master vbd in
              debug " ** Duration of dd (with 1 of %d paths) was %f seconds **" num_paths time2;
              let failover_time = time2 -. time1 in
              debug "So failover time was about %.1f seconds" failover_time;
              if failover_time > max_failover_time then failwith (Printf.sprintf "It took too long to failover between paths. It took %.1f secs, tolerance is %.1f secs" failover_time max_failover_time)
            )
            (fun () ->
              debug "Reinstating removed paths...";
              modify_fc_paths_manual rpc session master false
            )
        )
        (fun () ->
          debug "Unplugging VBD...";
          Client.VBD.unplug ~rpc ~session_id:session ~self:vbd
        )
    )
    (fun () ->
      debug "Removing test VDI...";
      Client.VDI.destroy ~rpc ~session_id:session ~self:vdi
    )
