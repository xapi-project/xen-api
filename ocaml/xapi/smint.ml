(* *)
open Pervasiveext

module D=Debug.Debugger(struct let name="smint" end)
open D

type vdi_info = {
	vdi_info_uuid: string option;
	vdi_info_location: string;
}

let make_vdi_info ~location ?uuid () = 
  { vdi_info_uuid = uuid;
    vdi_info_location = location;
  }

(** Very primitive first attempt at a set of backend capabilities *)
type capability =
    | Sr_create | Sr_delete | Sr_attach | Sr_detach | Sr_scan | Sr_probe | Sr_update
    | Vdi_create | Vdi_delete | Vdi_attach | Vdi_detach
    | Vdi_clone | Vdi_snapshot | Vdi_resize | Vdi_activate | Vdi_deactivate
    | Vdi_update | Vdi_introduce 
    | Vdi_resize_online
    | Vdi_generate_config

let all_capabilities =
  [ Sr_create; Sr_delete; Sr_attach; Sr_detach; Sr_scan; Sr_probe; Sr_update;
    Vdi_create; Vdi_delete; Vdi_attach; Vdi_detach;
    Vdi_clone; Vdi_resize; Vdi_activate; Vdi_deactivate;
    Vdi_update; Vdi_introduce;
    Vdi_resize_online
  ]

type sr_driver_info = {
        sr_driver_filename: string;
	sr_driver_name: string;
	sr_driver_description: string;
	sr_driver_vendor: string;
	sr_driver_copyright: string;
	sr_driver_version: string;
	sr_driver_required_api_version: string;
	sr_driver_capabilities: capability list;
	sr_driver_text_capabilities: string list;
	sr_driver_configuration: (string * string) list;
}

exception Backend_missing_field of string
exception Backend_report_error of (int * string)
exception Command_failed of (int * string * string (*stdout*) * string (*stderr*))
exception Command_killed of (int * string * string (*stdout*) * string (*stderr*))
exception Missing_field of string

exception Not_implemented_in_backend (* Raised by clone at least *)

exception Sr_not_empty
exception Vdi_in_use
exception Device_in_use


