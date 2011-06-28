
(* [maybe_add_lease __context vif]: if [vif] is on the Host internal
   management network then configure a DHCP lease via udhcpd *)
val maybe_add_lease: __context:Context.t -> API.ref_VIF -> unit

(* [get_ip __context vif]: if [vif] is on the Host internal management
   network then return the assigned IP *)
val get_ip: __context:Context.t -> API.ref_VIF -> (int * int * int * int) option

(* [init ()]: load in the saved leases database from disk, if there is one *)
val init: unit -> unit
