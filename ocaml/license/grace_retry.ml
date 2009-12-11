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
(** Helper to keep trying to get a "real" license after a "grace" license was checked out.
 *  @group Licensing *)

(** Schedule a timer to call [Host.apply_edition] again after an hour. Call this
 *  after getting a "grace" license in order to check whether the license server
 *  happened to come back. If so, a "real" license will be checked out.
 *  Note: the LPE already does a similar thing, but does not notify the product (xapi)
 *  if it succeeds to check out a "real" license! *)
let retry_periodically host edition =
	let period = 
		if Xapi_fist.reduce_grace_retry_period () then
			300.	(* 1h *)
		else
			3600.	(* 5min *)
	in
	let schedule = Xapi_periodic_scheduler.OneShot in
	let retry_fn () = Server_helpers.exec_with_new_task "grace_retry"
		(fun __context ->
			Helpers.call_api_functions ~__context
			(fun rpc session_id -> Client.Client.Host.apply_edition rpc session_id host edition)
		)
	in
	Xapi_periodic_scheduler.add_to_queue "retry after obtaining grace license" schedule period retry_fn
	
