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

type guidance = RebootHost | RestartToolstack | EvacuateHost | RestartDeviceModel

val introduce :
     __context:Context.t
  -> name_label:string
  -> name_description:string
  -> binary_url:string
  -> source_url:string
  -> [`Repository] API.Ref.t

val forget :
    __context:Context.t
  -> self:[`Repository] API.Ref.t
  -> unit

val get_enabled_repository :
     __context:Context.t
  -> [`Repository] API.Ref.t

val cleanup_pool_repo  : unit -> unit

val with_reposync_lock : (unit -> 'a) -> 'a

val sync:
     __context:Context.t
  -> self:[`Repository] API.Ref.t
  -> unit

val create_pool_repository :
     __context:Context.t
  -> self:[`Repository] API.Ref.t
  -> string

val get_repository_handler :
     Http.Request.t
  -> Unix.file_descr
  -> 'a
  -> unit

val get_host_updates_in_json :
     __context:Context.t
  -> installed:bool
  -> Yojson.Basic.t

val get_pool_updates_in_json :
     __context:Context.t
  -> hosts:[`host] API.Ref.t list
  -> Yojson.Basic.t

val apply :
     __context:Context.t
  -> host:[`host] API.Ref.t
  -> unit

val apply_updates :
     __context:Context.t
  -> host:[`host] API.Ref.t
  -> hash:string
  -> guidance list

val apply_immediate_guidances :
     __context:Context.t
  -> host:[`host] API.Ref.t
  -> guidances:guidance list
  -> unit
