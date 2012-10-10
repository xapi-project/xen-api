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
type suspend_flags = Debug | Live

type handle
type domid = int

(** open a xenguest handle *)
external init : unit -> handle = "stub_xenguest_init"

(** close a xenguest handle *)
external close : handle -> unit = "stub_xenguest_close"

(** build a linux domain *)
external linux_build : handle -> domid -> int -> int -> string ->
                       string option -> string -> string -> int ->
                       int -> int -> (nativeint * nativeint * string)
	= "stub_xc_linux_build_bytecode" "stub_xc_linux_build_native"

(** build a hvm domain *)
external hvm_build : handle -> domid -> int -> int -> string -> int -> int ->
	(nativeint * nativeint)
	= "stub_xc_hvm_build_bytecode" "stub_xc_hvm_build_native"

(** resume an uncooperative domain *)
external domain_resume_slow : handle -> domid -> unit
                            = "stub_xc_domain_resume_slow"

(** restore a domain *)
external domain_restore : handle -> Unix.file_descr -> domid
                       -> int -> int -> bool
                       -> nativeint * nativeint
       = "stub_xc_domain_restore_bytecode" "stub_xc_domain_restore"

(** save a domain *)
external domain_save : handle -> Unix.file_descr -> domid
                    -> int -> int -> suspend_flags list -> bool
                    -> unit
       = "stub_xc_domain_save_bytecode" "stub_xc_domain_save"

(** opensource xc dumpcore *)
external dumpcore : handle -> domid -> string -> unit
       = "stub_xc_domain_dumpcore"
