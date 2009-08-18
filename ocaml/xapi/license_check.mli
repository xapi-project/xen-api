val vm : __context:'a -> API.ref_VM -> unit

val with_vm_license_check : __context:'a -> [`VM] Ref.t -> (unit -> 'b) -> 'b 
