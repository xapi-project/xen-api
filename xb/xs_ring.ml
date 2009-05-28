(*
 * Copyright (c) 2006 XenSource Inc.
 * Author Vincent Hanquez <vincent@xensource.com>
 *
 * All rights reserved.
 *)

external read: Mmap.mmap_interface -> string -> int -> int = "ml_interface_read"
external write: Mmap.mmap_interface -> string -> int -> int = "ml_interface_write"
