(* Copyright (c) 2005-2006 XenSource Inc. *)


(** *)


type suspend_flags = Debug | Live

type handle
type domid = int

(** open a xenguest handle *)
external init : unit -> handle = "stub_xenguest_init"

(** close a xenguest handle *)
external close : handle -> unit = "stub_xenguest_close"

(** build a linux domain *)
external linux_build : handle -> domid -> int -> string
                    -> string option -> string -> string -> int
                    -> int -> int -> (nativeint * nativeint * string)
       = "stub_xc_linux_build_bytecode" "stub_xc_linux_build_native"

(** build a hvm domain *)
external hvm_build : handle -> domid -> int -> string -> int
                  -> bool -> bool -> bool -> bool -> bool -> int -> nativeint
       = "stub_xc_hvm_build_bytecode" "stub_xc_hvm_build_native"

(** build a hvm domain from memory *)
external hvm_build_mem : handle -> domid -> int -> string -> nativeint
                      -> int -> bool -> bool -> bool -> bool -> bool -> int -> nativeint
       = "stub_xc_hvm_build_mem_bytecode" "stub_xc_hvm_build_mem_native"

(** resume an uncooperative domain *)
external domain_resume_slow : handle -> domid -> unit
                            = "stub_xc_domain_resume_slow"

(** restore a domain *)
external domain_restore : handle -> Unix.file_descr -> domid
                       -> int -> int -> bool -> bool -> bool
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
