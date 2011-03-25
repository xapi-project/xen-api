val create :
	__context:Context.t -> name_label:string -> name_description:string -> [ `VM_appliance ] Ref.t
val destroy :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> unit

val assert_operation_valid :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> op:[`start | `clean_shutdown | `hard_shutdown | `shutdown ] -> unit
val update_allowed_operations :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> unit

val start :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> paused:bool -> unit
val clean_shutdown :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> unit
val hard_shutdown :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> unit
val shutdown :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> unit

val assert_can_be_recovered :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> session_to:[ `session ] Ref.t -> unit
val recover :
	__context:Context.t -> self:[ `VM_appliance ] Ref.t -> session_to:[ `session ] Ref.t -> force:bool -> unit
