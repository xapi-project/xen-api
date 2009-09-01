(* We need to cope with multiple threads attempting to allocate chunks of memory to start
   or resume VMs.

   The convention is:
   * memory is allocated for a particular domain id (a domain is also xen's unit of memory allocation)
   * a domain which is paused, not shutdown and has clocked up no CPU cycles (ie has never run)
     has an 'initial-reservation' key which indicates how much memory from the current total should be reserved.   
*)

(** Allocate [initial_allocation_kib] and associate it with a given domid *)
val allocate_memory_for_domain: __context:Context.t -> xc:Xc.handle -> xs:Xs.xsh -> initial_reservation_kib:int64 -> Domain.domid -> unit

(** After some domain destruction event (or possibly other memory-changing event), rebalance memory allocations *)
val balance_memory: xc:Xc.handle -> xs:Xs.xsh -> unit
