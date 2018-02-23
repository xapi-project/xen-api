module StringDiff =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module VdiNbdServerInfoDiff = struct
  type t = API.vdi_nbd_server_info_t
  let compare a b =
    let (>||=) a b = if a = 0 then b else a in
    let open API in
    (compare a.vdi_nbd_server_info_exportname b.vdi_nbd_server_info_exportname) >||=
    (compare a.vdi_nbd_server_info_address b.vdi_nbd_server_info_address) >||=
    (compare a.vdi_nbd_server_info_port b.vdi_nbd_server_info_port) >||=
    (compare a.vdi_nbd_server_info_cert b.vdi_nbd_server_info_cert) >||=
    (compare a.vdi_nbd_server_info_subject b.vdi_nbd_server_info_subject) >||=
    0
  let pp_printer formatter info =
    let open API in
    Format.pp_open_box formatter 0;
    Format.pp_print_string formatter info.vdi_nbd_server_info_exportname; Format.pp_print_space formatter ();
    Format.pp_print_string formatter info.vdi_nbd_server_info_address; Format.pp_print_space formatter ();
    Format.pp_print_int formatter (Int64.to_int info.vdi_nbd_server_info_port); Format.pp_print_space formatter ();
    Format.pp_print_string formatter info.vdi_nbd_server_info_cert; Format.pp_print_space formatter ();
    Format.pp_print_string formatter info.vdi_nbd_server_info_subject; Format.pp_print_space formatter ();
    Format.pp_close_box formatter ()
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module NetworkPurposeDiff = struct
  type t = API.network_purpose
  let compare = compare
  let pp_printer formatter purpose = Format.pp_print_string formatter (Record_util.network_purpose_to_string purpose)
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module StringSet = OUnitDiff.SetMake(StringDiff)
module NetworkPurposeSet = OUnitDiff.SetMake(NetworkPurposeDiff)
module StringList = OUnitDiff.ListSimpleMake(StringDiff)
module VdiNbdServerInfoSet = OUnitDiff.SetMake(VdiNbdServerInfoDiff)
