let xenstored_major = 1
let xenstored_minor = 0

let xenstored_proc_kva = "/proc/xen/xsd_kva"
let xenstored_proc_port = "/proc/xen/xsd_port"

let xs_daemon_socket = "/var/run/xenstored/socket"
let xs_daemon_socket_ro = "/var/run/xenstored/socket_ro"

let default_config_dir = "/etc/xensource"

let maxwatch = ref (-1)
let maxtransaction = ref (0)

let domid_self = 0x7FF0

exception Not_a_directory of string
exception Not_a_value of string
exception Already_exist
exception Doesnt_exist
exception Lookup_Doesnt_exist of string
exception Invalid_path
exception Permission_denied
exception Unknown_operation
