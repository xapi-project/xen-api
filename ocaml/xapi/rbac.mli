(*
 * Copyright (c) Cloud Software Group, Inc.
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

val is_access_allowed :
     __context:Context.t
  -> session_id:[`session] Ref.t
  -> permission:string
  -> bool
(** Determines whether the session associated with the provided
    context has the specified permission. The permission set is cached
    (on the coordinator only) to benefit successive queries for the
    same session. *)

val check :
     ?extra_dmsg:string
  -> ?extra_msg:string
  -> ?args:(string * Rpc.t) list
  -> ?keys:string list
  -> __context:Context.t
  -> fn:(unit -> 'a)
  -> [`session] Ref.t
  -> string
  -> 'a
(** [check] executes a function associated with an action if the
    session associated with the provided context is authorised to
    perform the action.

    The [?extra_dmsg] and [?extra_msg] parameters allow for extra
    information in debugging and error messages.

    The [?keys] parameter specifies which fields of a (string -> _)
    map are RBAC-protected. It is primarily associated with
    auto-generated methods such as add_to_other_config. However, if
    [?keys] is non-empty, then [?args] must also be consulted as the
    related methods that require this protection specify their key
    name as a parameter. Otherwise, [?args] is mostly used to log
    calls within the RBAC audit log. *)

val check_with_new_task :
     ?extra_dmsg:string
  -> ?extra_msg:string
  -> ?task_desc:string
  -> ?args:(string * Rpc.t) list
  -> fn:(unit -> 'a)
  -> [`session] Ref.t
  -> string
  -> 'a
(** Defined in terms of [check] but using a context associated with a
    freshly-created task. *)

val assert_permission_name : __context:Context.t -> permission:string -> unit
(** Performs a dry run of the [check] function with a no-op action
    guarded by the provided permission (as a name). *)

val assert_permission :
  __context:Context.t -> permission:Db_actions.role_t -> unit
(** Performs a dry run of the [check] function with a no-op action
    guarded by the provided permission (as a database role). *)

val has_permission : __context:Context.t -> permission:Db_actions.role_t -> bool
(** [has_permission ctx p] determines if the session associated with
    the context [ctx] is authorised to perform a specific action.

    [p] is of the type defined by the generated [Db_actions] module,
    as [Xapi_role] simulates a database for the checking of static
    role sets (as emitted in [Rbac_static]) and only appeals to the
    xapi DB for additional roles. *)

val is_rbac_enabled_for_http_action : string -> bool
(** [is_rbac_enabled_for_http_action route] determines whether RBAC
    checks should be applied to the provided HTTP [route].

    Some routes are precluded from RBAC checks because they are
    assumed to only be used by code paths where RBAC has already been
    checked or will be checked internally (e.g. /post_cli). *)

val permission_of_action :
  ?args:(string * Rpc.t) list -> keys:string list -> string -> string
(** Constructs the name of a permission associated with using an
    RBAC-protected key with a specified action.

    For example, if [keys] specifies "folder" as a protected key name
    for the action SR.remove_from_other_config, the permission name
    associated with that is "SR.remove_from_other_config/key:folder"
    - which is consistent with the format that [Rbac_static] contains. *)

val nofn : unit -> unit
(** Named function that does nothing, e.g. (fun _ -> ()).
    Used as a dummy action for RBAC checking. *)

val destroy_session_permissions_tbl : session_id:[`session] Ref.t -> unit
(** Removes any cached permission set for the given session. This is
    called when xapi destroys the DB entry for a session. *)
