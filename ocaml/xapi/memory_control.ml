module D = Debug.Debugger(struct let name="memory_control" end)
open D

open Pervasiveext
open Threadext

(** Protect all calls to the squeezer with a mutex to prevent chaos *)
let squeeze_m = Mutex.create ()

let allocate_memory_for_domain ~__context ~xc ~xs ~initial_reservation_kib domid =
  Mutex.execute squeeze_m
    (fun () ->
       try
	 Squeeze_xen.free_memory ~xc ~xs initial_reservation_kib;
	 (* Allocate this memory to the domain *)
	 xs.Xs.write (xs.Xs.getdomainpath domid ^ "/memory/initial-reservation") (Int64.to_string initial_reservation_kib)
       with 
       | Squeeze_xen.Cannot_free_this_much_memory _ ->
	   let free = Memory.get_free_memory_kib ~xc in
	   raise (Api_errors.Server_error (Api_errors.host_not_enough_free_memory, [ Int64.to_string free; Int64.to_string initial_reservation_kib ]))
       | Squeeze_xen.Domains_refused_to_cooperate domids ->
	   let vms = List.concat (List.map (fun domid -> 
					      try [ Vmopshelpers.vm_of_domid ~__context domid ]
					      with _ -> warn "When reporting ballooning error, failed to resolve domid %d to a VM" domid; []
					   ) domids) in
	   raise (Api_errors.Server_error(Api_errors.vms_failed_to_cooperate, List.map Ref.string_of vms))
    )

(** After an event which frees memory (eg a domain destruction), perform a one-off memory rebalance *)
let balance_memory ~xc ~xs =
  Mutex.execute squeeze_m 
    (fun () -> 
       debug "rebalance_memory";
       Squeeze_xen.balance_memory ~xc ~xs;
    )
