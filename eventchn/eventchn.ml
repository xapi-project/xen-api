(*
 * Copyright (c) 2006-2007 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 *)

exception Error of string

external init: unit -> Unix.file_descr = "stub_eventchn_init"
external notify: Unix.file_descr -> int -> unit = "stub_eventchn_notify"
external bind_interdomain: Unix.file_descr -> int -> int -> int = "stub_eventchn_bind_interdomain"
external bind_virq: Unix.file_descr -> int = "stub_eventchn_bind_virq"
external unbind: Unix.file_descr -> int -> unit = "stub_eventchn_unbind"
external read_port: Unix.file_descr -> int = "stub_eventchn_read_port"
external write_port: Unix.file_descr -> int -> unit = "stub_eventchn_write_port"

let _ = Callback.register_exception "eventchn.error" (Error "register_callback")
