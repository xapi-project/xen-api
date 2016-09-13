(*
 * Copyright (C) 2009 Citrix Systems Inc.
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
 * @group Redo-log
*)

(** Runs forever waiting for the redo log's status to change i.e. for
    	it to fail or to recover, generating alerts on transitions if
    Pool.other_config:metadata_lun_alerts is set to "true" *)
val loop: unit -> unit
