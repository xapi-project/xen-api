open Ctypes

module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

module U8 = Unsigned.UInt8
module U16 = Unsigned.UInt16

module Pci_dev = struct
  type t = {
    domain : int;
    bus : int;
    dev : int;
    func : int;
    vendor_id : int;
    device_id : int;
    device_class : int;
    irq : int;
    base_addr : nativeint list;
    size : nativeint list;
    rom_base_addr : nativeint;
    rom_size : nativeint;
    phy_slot : string option;
    subsystem_id : (int * int) option;
  }
  let make (_t: B.Pci_dev.t) =
    {
      domain = getf !@_t B.Pci_dev.domain |> U16.to_int;
      bus = getf !@_t B.Pci_dev.bus |> U8.to_int;
      dev = getf !@_t B.Pci_dev.dev |> U8.to_int;
      func = getf !@_t B.Pci_dev.func |> U8.to_int;
      vendor_id = getf !@_t B.Pci_dev.vendor_id |> U16.to_int;
      device_id = getf !@_t B.Pci_dev.device_id |> U16.to_int;
      device_class = getf !@_t B.Pci_dev.device_class |> U16.to_int;
      irq = getf !@_t B.Pci_dev.irq;
      base_addr = getf !@_t B.Pci_dev.base_addr |> CArray.to_list;
      size = getf !@_t B.Pci_dev.size |> CArray.to_list;
      rom_base_addr = getf !@_t B.Pci_dev.rom_base_addr;
      rom_size = getf !@_t B.Pci_dev.rom_size;
      phy_slot = getf !@_t B.Pci_dev.phy_slot;
      subsystem_id =
        match (B.pci_read_byte _t T.Header.header_type |> U8.to_int) land 0x7f with
        | x when x = T.Header.header_type_normal ->
          Some (
            B.pci_read_word _t T.Header.subsystem_vendor_id |> U16.to_int,
            B.pci_read_word _t T.Header.subsystem_id |> U16.to_int)
        | x when x = T.Header.header_type_cardbus ->
          Some (
            B.pci_read_word _t T.Header.cb_subsystem_vendor_id |> U16.to_int,
            B.pci_read_word _t T.Header.cb_subsystem_id |> U16.to_int)
        | _ -> None
    }
end

module Pci_access = struct
  type t = B.Pci_access.t

  let devices t =
    let rec list_of_linked_list acc = function
    | None -> acc
    | Some d -> list_of_linked_list (d::acc) (getf !@d B.Pci_dev.next) in
    list_of_linked_list [] (getf !@t B.Pci_access.devices)
end

let crush_flags f =
  List.fold_left (fun i o -> i lor (f o)) 0
let id x = x

let maybe f = function
  | Some x -> f x
  | None -> ()

let scan_bus = B.pci_scan_bus

let with_string ?(size=1024) f =
  (* Using an ocaml string violates this rule from the ctypes FAQ:
   * string is unsuitable for binding to C functions that write
   * into the string.
   * The caveats from ocaml_string do not apply (we do not release
   * the runtime lock (the default) or call back into OCaml from
   * these functions), because string actually does a copy,
   * however the lifetime of the value returned by libpci is bound
   * to the lifetime of the input parameter, which can be moved
   * by the GC and cause problems. *)
  let buf = CArray.make char ~initial:'\x00' size in
  let s = CArray.start buf in
  let r = f s size in
  (* Keep `s` alive through the C binding invocation in `f` *)
  ignore (Sys.opaque_identity (List.hd [s]));
  r

let lookup_class_name pci_access class_id =
  with_string (fun buf size ->
    B.pci_lookup_name_1_ary pci_access buf size T.Lookup_mode.lookup_class
      class_id)

let lookup_progif_name pci_access class_id progif_id =
  with_string (fun buf size ->
    B.pci_lookup_name_2_ary pci_access buf size T.Lookup_mode.lookup_progif
      class_id progif_id)

let lookup_vendor_name pci_access vendor_id =
  with_string (fun buf size ->
    B.pci_lookup_name_1_ary pci_access buf size T.Lookup_mode.lookup_vendor
      vendor_id)

let lookup_device_name pci_access vendor_id device_id =
  with_string (fun buf size ->
    B.pci_lookup_name_2_ary pci_access buf size T.Lookup_mode.lookup_device
      vendor_id device_id)

let lookup_subsystem_vendor_name pci_access subv_id =
  with_string (fun buf size ->
    let lookup_flags = T.Lookup_mode.([ lookup_subsystem; lookup_vendor ]) in
    B.pci_lookup_name_1_ary pci_access buf size (crush_flags id lookup_flags)
      subv_id)

let lookup_subsystem_device_name pci_access vendor_id device_id subv_id subd_id =
  with_string (fun buf size ->
    let lookup_flags = T.Lookup_mode.([ lookup_subsystem; lookup_device ]) in
    B.pci_lookup_name_4_ary pci_access buf size (crush_flags id lookup_flags)
      vendor_id device_id subv_id subd_id)

let with_access ?(cleanup=true) ?from_dump f =
  let pci_access = B.pci_alloc () in
  maybe (fun path ->
    setf !@pci_access B.Pci_access.method_ T.Access_type.dump;
    ignore @@ B.pci_set_param pci_access "dump.name" path;
  ) from_dump;
  B.pci_init pci_access;
  if not cleanup then f pci_access
  else
    let result =
      try f pci_access
      with exn ->
        B.pci_cleanup pci_access;
        raise exn
    in
    B.pci_cleanup pci_access;
    result

let get_devices pci_access =
  B.pci_scan_bus pci_access;
  let devs = Pci_access.devices pci_access in
  (* Be sure to fill all the fields that can be accessed from a Pci_dev.t *)
  let fill_flags = T.Fill_flag.([
    fill_ident; fill_irq; fill_bases; fill_rom_base; fill_sizes; fill_class;
    fill_caps; fill_ext_caps; fill_phys_slot; fill_module_alias; ]) in
  let flags = crush_flags id fill_flags in
  List.map (fun d -> let (_: int) = B.pci_fill_info d flags in Pci_dev.make d) devs
