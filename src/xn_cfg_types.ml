let ( |> ) a b = b a

type value =
	| String of string
	| Int of int
	| List of value list
[@@deriving rpc]

exception Type_error of string * string

let string = function
	| String x -> x
	| x -> raise (Type_error("string", x |> rpc_of_value |> Jsonrpc.to_string))

let int = function
	| Int x -> x
	| x -> raise (Type_error("int", x |> rpc_of_value |> Jsonrpc.to_string))

let bool = function
	| Int 1 -> true
	| Int 0 -> false
	| x -> raise (Type_error("bool", x |> rpc_of_value |> Jsonrpc.to_string))

let list f = function
	| List vs -> List.map f vs
	| x -> raise (Type_error("int", x |> rpc_of_value |> Jsonrpc.to_string))

type config = (string * value) list [@@deriving rpc]

(* Well-known constants *)
let _kernel = "kernel"
let _ramdisk = "ramdisk"
let _root = "root"
let _builder = "builder"
let _bootloader = "bootloader"
let _boot = "boot"
let _name = "name"
let _uuid = "uuid"
let _memory = "memory" (* MiB *)
let _cpus = "cpus" (* pCPUs *)
let _vcpus = "vcpus" (* number of vCPUs *)

let _vif = "vif"
let _backend = "backend"
let _bridge = "bridge"
let _ip = "ip"
let _mac = "mac"
let _script = "script"
let _type = "type"
let _vifname = "vifname"

let _disk = "disk"
let _pci = "pci"
let _msitranslate = "msitranslate"
let _power_mgmt = "power_mgmt"
let _vm_pci_msitranslate = "pci_msitranslate"
let _vm_pci_power_mgmt = "pci_power_mgmt"
let _vm_has_vendor_device = "has_vendor_device"
