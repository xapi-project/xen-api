(*
 * Copyright (C) 2006-2013 Citrix Systems Inc.
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

module D = Debug.Make(struct let name="xapi" end)
open D

let systemctl = "/usr/bin/systemctl"
let gpumon = "xcp-rrdd-gpumon"

module Gpumon = Daemon_manager.Make(struct
    let check = Daemon_manager.Function (fun () ->
        try
          ignore
            (Forkhelpers.execute_command_get_output systemctl
               ["is-active"; "-q"; gpumon]);
          true
        with _ -> false)

    let start () =
      debug "Starting %s" gpumon;
      ignore (Forkhelpers.execute_command_get_output systemctl ["start"; gpumon])

    let stop () =
      debug "Stopping %s" gpumon;
      ignore (Forkhelpers.execute_command_get_output systemctl ["stop"; gpumon])
  end)

let with_gpumon_stopped = Gpumon.with_daemon_stopped

module Nvidia = struct
  let key = "nvidia"

  (* N.B. the pgpu must be in the local host where this function runs *)
  let get_pgpu_compatibility_metadata ~__context ~pgpu =
    let get = Gpumon_client.Client.Nvidia.get_pgpu_metadata in
    let pci = Db.PGPU.get_PCI ~__context ~self:pgpu in
    let address = Db.PCI.get_pci_id ~__context ~self:pci in
    [key, get "xapi_gpumon" address]

end (* Nvidia *)
