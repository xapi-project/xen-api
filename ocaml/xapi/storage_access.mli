module SR :
sig
  val attach : __context:Context.t -> self:API.ref_SR -> unit
  val detach : __context:Context.t -> self:API.ref_SR -> unit
end

module VDI :
sig
  val initialise_refcounts_from_db : unit -> unit

  val attach     : __context:Context.t -> self:API.ref_VDI -> mode:[`RO|`RW] -> unit
  val detach     : __context:Context.t -> self:API.ref_VDI -> unit
  val activate   : __context:Context.t -> self:API.ref_VDI -> unit
  val deactivate : __context:Context.t -> self:API.ref_VDI -> unit

  val check_enclosing_sr_for_capability : Context.t -> Smint.capability -> [ `VDI ] Ref.t -> bool
  val get_physical_path:[`VDI] Uuid.t -> string
end

val use_vdi                          : __context:Context.t -> vdi:API.ref_VDI -> mode:[`RO|`RW] -> unit
val deactivate_and_detach            : __context:Context.t -> vdi:API.ref_VDI -> unit
val with_careful_attach_and_activate : __context:Context.t -> vdis:(API.ref_VDI * [`RO|`RW]) list -> leave_activated:bool -> (unit -> 'a ) -> 'a
val use_vdi_from_vbd                 : __context:Context.t -> [`VBD] Ref.t -> unit

val set_dirty                        : __context:Context.t -> self:[`SR] Ref.t -> unit
