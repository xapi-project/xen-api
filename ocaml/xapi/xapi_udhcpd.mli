
(* [maybe_add_lease __context vif]: if [vif] is on the Host internal
   management network then configure a DHCP lease via udhcpd *)
val maybe_add_lease: __context:Context.t -> API.ref_VIF -> unit

(* [maybe_remove_lease __context vm]: if [vm] has a VIF on the Host
   internal management network then remove the assigned DHCP lease *)
val maybe_remove_lease: __context:Context.t -> API.ref_VM -> unit

(* [handler req s]: proxies data from [s] through to the VM identified
   in [req] *)
val handler: Http.request -> Unix.file_descr -> unit
