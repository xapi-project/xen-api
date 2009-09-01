(**
	The Pool master's view of the total memory and memory consumers on a host.
	This doesn't take into account dynamic changes i.e. those caused by
	ballooning. Therefore if we ask a question like, 'is there <x> amount of
	memory free to boot VM <y>' we will get one of 3 different answers:
	1. yes:
		the sum of the static_max's of all VMs with domains + the request
		is less than the total free.
	2. maybe:
		depending on the behaviour of the balloon drivers in the guest we
		may be able to free the memory.
	3. no:
		the sum of the dynamic_min's of all the VMs with domains + the
		request is more than the total free.
*)
type host_memory_summary = {
	(** Amount of free memory sampled after booting Xen + crash kernel *)
	host_free_memory_total: int64;
	(** list of VMs which have a domain running here *)
	resident: API.ref_VM list;
	(** list of VMs which are in the process of having a domain created here *)
	scheduled: API.ref_VM list;
}

(** Different users will wish to use a different VM accounting policy, depending
    on how conservative they are. *)
type accounting_policy = 
	| Static_max
		(** use static_max: super conservative, useful for HA *)
	| Dynamic_target
		(** upper-bound on the VMs dynamic memory usage from the master's PoV *)

(** Return a host's memory summary from live database contents. *)
val get_host_memory_summary : __context:Context.t -> host:API.ref_host ->
	host_memory_summary

(**
	Given a host's memory summary and a policy flag (i.e. whether to only
	consider static_max or to consider dynamic balloon data) it returns the
	amount of free memory on the host.
*)
val compute_free_memory : __context:Context.t -> host_memory_summary ->
	accounting_policy -> int64

val vm_compute_start_memory : __context:Context.t ->
	?policy:accounting_policy -> API.vM_t -> int64 * int64

val vm_compute_used_memory : __context:Context.t -> accounting_policy ->
	[`VM] Ref.t -> int64 * int64

val vm_compute_resume_memory : __context:Context.t -> [`VM] Ref.t -> int64

(**
	Compute, from our managed data, how much memory is available on a host; this
	takes into account both VMs that are resident_on the host and also VMs that
	are scheduled_to_be_resident_on the host.

	If ignore_scheduled_vm is set then we do not consider this VM as having any
	resources allocated via the scheduled_to_be_resident_on mechanism. This is
	used to ensure that, when we're executing this function with a view to
	starting a VM, v, and further that v is scheduled_to_be_resident on the
	specified host, that we do not count the resources required for v twice.

	If 'dump_stats=true' then we write to the debug log where we think the
	memory is being used.
*)
val host_compute_free_memory : ?dump_stats:bool -> __context:Context.t ->
	host:[`host] Ref.t -> [`VM] Ref.t option -> int64
