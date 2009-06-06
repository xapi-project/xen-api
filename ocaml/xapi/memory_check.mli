val vm_compute_start_memory : __context:Context.t -> API.vM_t -> int64 * int64
val vm_compute_used_memory : __context:Context.t -> [`VM] Ref.t -> int64 * int64
val host_compute_free_memory : ?dump_stats:bool -> __context:Context.t -> host:[`host] Ref.t -> [`VM] Ref.t option -> int64