val with_block_attached_devices :    Context.t -> (XMLRPC.xmlrpc -> XMLRPC.xmlrpc) -> API.ref_session -> API.ref_VDI list -> API.vbd_mode -> (string list -> 'a) -> 'a
val with_block_attached_device  :    Context.t -> (XMLRPC.xmlrpc -> XMLRPC.xmlrpc) -> API.ref_session -> API.ref_VDI -> API.vbd_mode -> (string -> 'a) -> 'a
val with_new_fs_vdi : Context.t -> name_label:string -> name_description:string -> sR:API.ref_SR -> required_space:int64 -> _type:API.vdi_type ->
  sm_config:API.string_to_string_map -> (API.ref_VDI -> string -> 'a) -> 'a
val with_fs_vdi :   Context.t -> API.ref_VDI -> (string -> 'a) -> 'a
val copy_vdi : __context:Context.t -> API.ref_VDI -> API.ref_VDI -> unit


val must_write_zeroes_into_new_vdi : __context:Context.t -> API.ref_VDI -> bool
