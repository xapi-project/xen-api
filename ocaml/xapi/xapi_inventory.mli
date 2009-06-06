exception Missing_inventory_key of string

val read_inventory: unit -> unit
val reread_inventory: unit -> unit

val lookup: string -> string
val remove: string -> unit
val update: string -> string -> unit

val parse_inventory_entry: string -> (string * string) option

(* Keys defined in Geneva *)
val _product_brand : string
val _product_name : string
val _product_version : string
val _build_number : string
val _kernel_version : string
val _xen_version : string
val _installation_date : string
val _default_sr : string
val _primary_disk : string
val _backup_partition : string
val _installation_uuid : string
val _default_sr_physdevs : string
val _dom0_mem : string


(* Keys defined in Rio *)
val _control_domain_uuid : string
val _management_interface : string

(* Keys defined in Miami *)
val _oem_manufacturer : string
val _oem_model : string
val _oem_build_number : string
val _machine_serial_number : string
val _machine_serial_name : string

(* Keys defined in Orlando *)
val _ha_interfaces : string
