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
(**
 * @group Access Control
*)

open Auth_signature
open Extauth

(* PRECONDITION: All of these additional calls require a valid session to be presented.*)
(* ==> the session validity is already checked in every server.ml call by using Session_check.check *)

let get_subject_identifier ~__context ~subject_name =
  call_with_exception_handler (fun () ->
      (Ext_auth.d ()).get_subject_identifier ~__context subject_name
  )

let get_group_membership ~__context ~subject_identifier =
  call_with_exception_handler (fun () ->
      (Ext_auth.d ()).query_group_membership ~__context subject_identifier
  )

let get_subject_information_from_identifier ~__context ~subject_identifier =
  call_with_exception_handler (fun () ->
      try
        (* Query from xapi db first *)
        Xapi_subject.query_subject_information_from_db ~__context
          subject_identifier
      with Auth_signature.Subject_cannot_be_resolved ->
        (* Not found, fall back to query AD *)
        Xapi_subject.query_subject_information_from_AD ~__context
          subject_identifier
  )
