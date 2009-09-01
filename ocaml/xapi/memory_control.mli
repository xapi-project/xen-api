(* We need to cope with multiple threads attempting to allocate chunks of memory to start
   or resume VMs.

   The convention is:
   * memory is initially reserved and associated with a reservation_id
   * assuming the domain create succeeds, the reservation is tranferred to the domain
   * a domain which is paused, not shutdown and has clocked up no CPU cycles (ie has never run)
     has an 'initial-reservation' key which indicates how much memory from the current total should be reserved.   
*)

(** reserve [kib] KiB of memory for some undisclosed purpose, return a reservation_id *)
val reserve_memory: __context:Context.t -> xc:Xc.handle -> xs:Xs.xsh -> kib:int64 -> string

(** reserve between [min] and [max] KiB of memory for some undisclosed purpose, return the actual amount plus reservation_id *)
val reserve_memory_range: __context:Context.t -> xc:Xc.handle -> xs:Xs.xsh -> min:int64 -> max:int64 -> int64 * string

(** Transfer the memory reserved by [reservation_id] to domain [domid] *)
val transfer_reservation_to_domain: __context:Context.t -> xs:Xs.xsh -> reservation_id:string -> domid:int -> unit

(** Delete the reservation given by [reservation_id] *)
val delete_reservation: __context:Context.t -> xs:Xs.xsh -> reservation_id:string -> unit

(** After some domain destruction event (or possibly other memory-changing event), rebalance memory allocations *)
val balance_memory: __context:Context.t -> xc:Xc.handle -> xs:Xs.xsh -> unit
