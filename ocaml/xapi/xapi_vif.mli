(** Module that defines API functions for VIF objects *)

(** {2 API functions} *)

(** Hotplug the VIF, dynamically attaching it to the running VM *)
val plug : __context:Context.t -> self:API.ref_VIF -> unit

(** Hot-unplug the VIF, dynamically unattaching it to the running VM *)
val unplug : __context:Context.t -> self:API.ref_VIF -> unit

(** Create a new VIF instance *)
val create :
  __context:Context.t ->
  device:string ->
  network:[ `network ] Ref.t ->
  vM:[ `VM ] Ref.t ->
  mAC:string ->
  mTU:int64 ->
  other_config:(string * string) list ->
  qos_algorithm_type:string ->
  qos_algorithm_params:(string * string) list -> API.ref_VIF

(** Destroy the specified VIF instance *)
val destroy : __context:Context.t -> self:[ `VIF ] Ref.t -> unit

(** {2 Helper Functions} *)

val assert_operation_valid :
  __context:Context.t -> self:[ `VIF ] Ref.t -> op:API.vif_operations -> unit
val update_allowed_operations :
  __context:Context.t -> self:[ `VIF ] Ref.t -> unit
val dynamic_create :
  __context:Context.t -> vif:API.ref_VIF -> Locking_helpers.token -> unit
val destroy_vif :
  __context:Context.t -> xs:Xs.xsh -> 'a -> [ `VIF ] Ref.t -> 'b -> unit
val dynamic_destroy :
  __context:Context.t -> vif:[ `VIF ] Ref.t -> Locking_helpers.token -> unit
