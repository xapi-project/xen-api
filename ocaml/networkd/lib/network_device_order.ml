(** Generete an order for host network devices and keep the order as stable as possible.
  *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let last_file_path = "/var/lib/xcp/last_ordered_network_devices.json"

let old_file_path = "/var/lib/xcp/old_ordered_network_devices.json"

let initial_rules_file_path =
  "/var/lib/xcp/initial_ordered_network_devices.conf"

let cmd_biosdevname = "/usr/sbin/biosdevname"

(** To hold the generated order in memory. It is written once, and since that read-only. *)
let order : (int * string option) list ref = ref []

(** PCI address in format SBDF: segment:bus:device:function *)
module PciAddr = struct
  type t = {segment: int; bus: int; dev: int; func: int} [@@deriving yojson]

  let default = {segment= 0; bus= 0; dev= 0; func= 0}

  let pattern =
    Re.Posix.compile_pat
      "^([0-9a-fA-F]{4}):([0-9a-fA-F]{2}):([0-9a-fA-F]{2})\\.([0-9a-fA-F])$"

  let to_string t =
    Printf.sprintf "%04x:%02x:%02x.%d" t.segment t.bus t.dev t.func

  let int_of_hex_str s = int_of_string (Printf.sprintf "0x%s" s)

  exception Parse_error of string

  let of_string_exn s =
    match Re.exec_opt pattern s with
    | Some g -> (
      try
        let segment = int_of_hex_str (Re.Group.get g 1) in
        let bus = int_of_hex_str (Re.Group.get g 2) in
        let dev = int_of_hex_str (Re.Group.get g 3) in
        let func = int_of_hex_str (Re.Group.get g 4) in
        {segment; bus; dev; func}
      with _ -> raise (Parse_error s)
    )
    | None ->
        raise (Parse_error s)

  let compare t1 t2 =
    match
      ( compare t1.segment t2.segment
      , compare t1.bus t2.bus
      , compare t1.dev t2.dev
      , compare t1.func t2.func
      )
    with
    | x, _, _, _ when x <> 0 ->
        x
    | _, x, _, _ when x <> 0 ->
        x
    | _, _, x, _ when x <> 0 ->
        x
    | _, _, _, x ->
        x
end

module PciAddrMap = Map.Make (PciAddr)
module MacaddrSet = Set.Make (Macaddr)
module MacaddrMap = Map.Make (Macaddr)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module ToMap = struct
  module type MapOut = sig
    type !+'a t

    type key

    val empty : 'a t

    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  end

  module type MapIn = sig
    type !+'a t

    type key

    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  end

  module MakeForList (M : MapOut) = struct
    exception Duplicate_key

    let fail _ = raise Duplicate_key

    (** [to_11_map ~by l] is a map. For each binding in the map, the key is [by v] and the value is [v] in the list. *)
    let to_11_map ~by l =
      List.fold_left
        (fun acc v ->
          let f x = Some (Option.fold ~none:v ~some:fail x) in
          M.update (by v) f acc
        )
        M.empty l
  end

  module MakeForMap (MI : MapIn) (MO : MapOut) = struct
    exception Duplicate_key

    let fail _ = raise Duplicate_key

    (** [to_11_map ~by m] is a map. For each binding in the map, the key is [by v] and the value is [v] which is a value in the [m]. *)
    let to_11_map ~by m =
      MI.fold
        (fun k v acc ->
          let f x = Some (Option.fold ~none:v ~some:fail x) in
          MO.update (by k v) f acc
        )
        m MO.empty

    (** [to_1n_map ~by m] is a map. For each binding in the map, the key is [by v] and the value is a list of [v] which are values in the [m]. *)
    let to_1n_map ~by m =
      MI.fold
        (fun k v acc ->
          let f x = Some (Option.fold ~none:[v] ~some:(List.cons v) x) in
          MO.update (by k v) f acc
        )
        m MO.empty
  end
end

module ListToIntMap = ToMap.MakeForList (IntMap)
module ListToMacaddrMap = ToMap.MakeForList (MacaddrMap)
module MacaddrMapToPciAddrMap = ToMap.MakeForMap (MacaddrMap) (PciAddrMap)
module MacaddrMapToIntMap = ToMap.MakeForMap (MacaddrMap) (IntMap)

(** Network device represented by name, PCI address and MAC address. *)
module NetDev = struct
  let pci_addr_to_yojson pci_addr =
    PciAddr.to_string pci_addr |> [%to_yojson: string]

  let yojson_to_pci_addr = function
    | `String x -> (
      try Ok (PciAddr.of_string_exn x)
      with _ -> Error "Invalid string for PCI address"
    )
    | _ ->
        Error "Invalid yojson for PCI address"

  let mac_addr_to_yojson mac_addr =
    Macaddr.to_string mac_addr |> [%to_yojson: string]

  let yojson_to_mac_addr = function
    | `String x -> (
      try Ok (Macaddr.of_string_exn x)
      with _ -> Error "Invalid string for MAC address"
    )
    | _ ->
        Error "Invalid yojson for MAC address"

  type t = {
      name: string
    ; pci_addr: PciAddr.t
          [@to_yojson pci_addr_to_yojson] [@of_yojson yojson_to_pci_addr]
    ; mac_addr: Macaddr.t
          [@to_yojson mac_addr_to_yojson] [@of_yojson yojson_to_mac_addr]
  }
  [@@deriving yojson]

  let default =
    {
      name= ""
    ; pci_addr= PciAddr.default
    ; mac_addr= Macaddr.of_string_exn "00:00:00:00:00:00"
    }

  let to_string t =
    Printf.sprintf "network device (name=%s) (MAC address=%s) (PCI address=%s)"
      t.name
      (Macaddr.to_string t.mac_addr)
      (PciAddr.to_string t.pci_addr)
end

(** Configuration of the position for a network device which can be identified by the [index]. *)
module NetDevMapping = struct
  type index = Mac_addr of Macaddr.t | Pci_addr of PciAddr.t | Label of string

  let string_of_index = function
    | Mac_addr mac_addr ->
        Printf.sprintf "mac:\"%s\"" (Macaddr.to_string mac_addr)
    | Pci_addr pci_addr ->
        Printf.sprintf "pci:\"%s\"" (PciAddr.to_string pci_addr)
    | Label name ->
        Printf.sprintf "label:\"%s\"" name

  exception Unsupported_type of string

  let index_of_string ty value =
    match ty with
    | ty when ty = "pci" ->
        Pci_addr (PciAddr.of_string_exn value)
    | ty when ty = "mac" ->
        Mac_addr (Macaddr.of_string_exn value)
    | ty when ty = "label" ->
        Label value
    | _ ->
        raise (Unsupported_type ty)

  type t = {position: int; index: index}

  let matched (net_dev : NetDev.t) t : bool =
    match t.index with
    | Mac_addr mac_addr ->
        let to_str = Macaddr.to_string in
        to_str net_dev.mac_addr = to_str mac_addr
    | Pci_addr pci_addr ->
        net_dev.pci_addr = pci_addr
    | Label label ->
        net_dev.name = label

  let to_string t = Printf.sprintf "%d:%s" t.position (string_of_index t.index)

  let parse_mapping_conf =
    let comment_pattern = Re.Posix.compile_pat "^[ ]*(#.*)?$" in
    let mapping_pattern =
      Re.Posix.compile_pat "^[ ]*([0-9]+):(mac|pci|label):\"([0-9a-z:]+)\"[ ]*$"
    in
    fun line ->
      debug "%s: line: %s" __FUNCTION__ line ;
      match Re.execp comment_pattern line with
      | true ->
          None
      | false ->
          Re.exec_opt mapping_pattern line
          |> Option.map (fun g ->
                 let position = int_of_string (Re.Group.get g 1) in
                 let value = Re.Group.get g 3 in
                 let index = index_of_string (Re.Group.get g 2) value in
                 let m = {position; index} in
                 debug "%s: mapping: %s" __FUNCTION__ (to_string m) ;
                 m
             )

  exception Duplicate_position

  let assert_no_duplicate_position (l : t list) =
    try
      let _ : t IntMap.t =
        ListToIntMap.to_11_map ~by:(fun dev -> dev.position) l
      in
      ()
    with ListToIntMap.Duplicate_key -> raise Duplicate_position

  let mappings_of_file ~(path : string) : t list =
    if not (Sys.file_exists path) then
      []
    else
      let read_lines = Xapi_stdext_unix.Unixext.read_lines in
      let l = read_lines ~path |> List.filter_map parse_mapping_conf in
      (* Ensure no position conflicts. *)
      assert_no_duplicate_position l ;
      l
end

let ethn_pattern = Re.Posix.compile_pat "^eth([0-9]+)$"

exception Not_ethN of string

let n_of_ethn ethn =
  match Re.exec_opt ethn_pattern ethn with
  | Some g ->
      int_of_string (Re.Group.get g 1)
  | None ->
      raise (Not_ethN ethn)

(** A network device which has not been assigned a position in the order. *)
module UnOrderedNetDev = struct
  type t = {net_dev: NetDev.t; bios_eth_order: int}

  let default = {net_dev= NetDev.default; bios_eth_order= -1}

  let to_string t =
    Printf.sprintf "UnOrderedNetDev: %s, bios_eth_order=%d"
      (NetDev.to_string t.net_dev)
      t.bios_eth_order

  exception Missing_key of string

  let parse output_of_one_dev =
    debug "%s: line: %s" __FUNCTION__ output_of_one_dev ;
    let kvs =
      let open Astring.String in
      cuts ~sep:"\n" output_of_one_dev
      |> List.filter_map (fun line ->
             cut ~sep:":" line |> Option.map (fun (k, v) -> (trim k, trim v))
         )
    in
    List.iter (fun (k, v) -> debug "%s: [%s]=[%s]" __FUNCTION__ k v) kvs ;
    [
      ( "BIOS device"
      , fun acc value -> {acc with bios_eth_order= n_of_ethn value}
      )
    ; ( "Kernel name"
      , fun acc value -> {acc with net_dev= {acc.net_dev with name= value}}
      )
    ; ( "Assigned MAC"
      , fun acc value ->
          {
            acc with
            net_dev= {acc.net_dev with mac_addr= Macaddr.of_string_exn value}
          }
      )
    ; ( "Bus Info"
      , fun acc value ->
          {
            acc with
            net_dev= {acc.net_dev with pci_addr= PciAddr.of_string_exn value}
          }
      )
    ]
    |> List.fold_left
         (fun (missing_keys, acc) (key, f) ->
           match List.assoc_opt key kvs with
           | Some v ->
               (missing_keys, f acc v)
           | None ->
               (key :: missing_keys, acc)
         )
         ([], default)
    |> fun (missing_keys, r) ->
    match missing_keys with
    | [] ->
        debug "%s: %s" __FUNCTION__ (to_string r) ;
        r
    | key :: _ ->
        raise (Missing_key key)

  exception Duplicate_mac_address

  let get_all () : t MacaddrMap.t =
    try
      Network_utils.call_script cmd_biosdevname
        ["--policy"; "all_ethN"; "-d"; "-x"]
      |> Astring.String.cuts ~sep:"\n\n"
      |> List.filter (fun line -> line <> "")
      |> List.map parse
      |> ListToMacaddrMap.to_11_map ~by:(fun v -> v.net_dev.mac_addr)
    with
    | ListToMacaddrMap.Duplicate_key ->
        raise Duplicate_mac_address
    | e ->
        error "%s" "Can't parse the output of the biosdevname!" ;
        raise e
end

(** A network device which is being ordered. *)
module OrderingNetDev = struct
  type t = {
      net_dev: NetDev.t
    ; position: int option
    ; bios_eth_order: int
    ; multinic: bool
  }
  [@@deriving yojson]

  let to_string t = Printf.sprintf "%s" (Yojson.Safe.to_string (to_yojson t))

  let of_unordered_map (currents : UnOrderedNetDev.t MacaddrMap.t) :
      t MacaddrMap.t =
    let pci_cnt =
      let f o = Some (Option.fold ~none:1 ~some:(fun c -> c + 1) o) in
      MacaddrMap.fold
        (fun _ dev acc ->
          PciAddrMap.update dev.UnOrderedNetDev.net_dev.pci_addr f acc
        )
        currents PciAddrMap.empty
    in
    MacaddrMap.map
      (fun dev ->
        let multinic =
          (* Will never raise exception or be < 1 *)
          let c =
            PciAddrMap.find dev.UnOrderedNetDev.net_dev.pci_addr pci_cnt
          in
          if c > 1 then true else false
        in
        {
          net_dev= dev.net_dev
        ; position= None
        ; bios_eth_order= dev.bios_eth_order
        ; multinic
        }
      )
      currents

  let compare_on_mac t1 t2 =
    Macaddr.compare t1.net_dev.mac_addr t2.net_dev.mac_addr

  let compare_on_bios_eth_order t1 t2 =
    compare t1.bios_eth_order t2.bios_eth_order
end

(** A network device which has been in an order. *)
module OrderedNetDev = struct
  type t = {net_dev: NetDev.t; position: int} [@@deriving yojson]

  let compare_on_mac t1 t2 =
    Macaddr.compare t1.net_dev.mac_addr t2.net_dev.mac_addr

  let of_ordering (ordering : OrderingNetDev.t) =
    {net_dev= ordering.net_dev; position= Option.get ordering.position}

  let to_string t =
    Printf.sprintf "%d -> %s" t.position (NetDev.to_string t.net_dev)

  let write_to_file ~path l =
    List.map to_yojson l |> (fun l -> `List l) |> Yojson.Safe.to_file path

  exception Parse_error of string

  let order_of_file ~(path : string) : t MacaddrMap.t =
    if not (Sys.file_exists path) then
      MacaddrMap.empty
    else
      let read_file = Xapi_stdext_unix.Unixext.string_of_file in
      let yojson_str =
        match read_file path with s when s = "" -> "[]" | s -> s
      in
      debug "%s: yojson string: %s" __FUNCTION__ yojson_str ;
      yojson_str |> Yojson.Safe.from_string |> function
      | `List l ->
          let devs =
            List.map
              (fun x ->
                match of_yojson x with
                | Result.Ok dev ->
                    debug "%s: %s -> %s" __FUNCTION__
                      (Macaddr.to_string dev.net_dev.mac_addr)
                      (to_string dev) ;
                    dev
                | Result.Error err ->
                    raise (Parse_error err)
              )
              l
          in
          (* Ensure no position conflicts. *)
          let _ : t IntMap.t =
            ListToIntMap.to_11_map ~by:(fun dev -> dev.position) devs
          in
          ListToMacaddrMap.to_11_map ~by:(fun dev -> dev.net_dev.mac_addr) devs
      | _ ->
          raise (Parse_error "Not a list")

  let map_by_pci (m : t MacaddrMap.t) : t list PciAddrMap.t =
    MacaddrMapToPciAddrMap.to_1n_map ~by:(fun _k v -> v.net_dev.pci_addr) m

  let map_by_position (m : t MacaddrMap.t) : t IntMap.t =
    MacaddrMapToIntMap.to_11_map ~by:(fun _k v -> v.position) m

  exception Duplicate_position

  let assert_no_duplicate_position (l : t list) =
    try
      let _ : t IntMap.t =
        ListToIntMap.to_11_map ~by:(fun dev -> dev.position) l
      in
      ()
    with _ -> raise Duplicate_position
end

let assign_position_by_mapping ~(mappings : NetDevMapping.t list)
    (dev : OrderingNetDev.t) : OrderingNetDev.t =
  let open NetDevMapping in
  match List.filter (matched dev.net_dev) mappings with
  | {position; _} :: _ ->
      debug "%s: assign position: %d <- %s" __FUNCTION__ position
        (OrderingNetDev.to_string dev) ;
      {dev with position= Some position}
  | [] ->
      dev

let assign_position_by_mac ~dbg ~(lasts : OrderedNetDev.t MacaddrMap.t)
    (dev : OrderingNetDev.t) : OrderingNetDev.t =
  let mac_addr = dev.net_dev.mac_addr in
  match (dev, MacaddrMap.find_opt mac_addr lasts) with
  | ({position= Some _; _} as dev), _ ->
      (* The position has been assgined. *)
      dev
  | {position= None; _}, Some OrderedNetDev.{position; _} ->
      (* Found a MAC matched network device in lasts: assign the position as last. *)
      debug "%s %s: assign position: %d <- %s" __FUNCTION__ dbg position
        (OrderingNetDev.to_string dev) ;
      {dev with position= Some position}
  | ({position= None; _} as dev), None ->
      debug "%s %s: skip %s" __FUNCTION__ dbg (OrderingNetDev.to_string dev) ;
      (* a new network device: leave it unassigned at the moment *)
      dev

let assign_position_by_pci ~dbg ~(seen_pcis : OrderedNetDev.t list PciAddrMap.t)
    ~(curr_macs : MacaddrSet.t) (dev : OrderingNetDev.t) : OrderingNetDev.t =
  match (dev, PciAddrMap.find_opt dev.net_dev.pci_addr seen_pcis) with
  | ({position= Some _; _} as dev), _ ->
      dev (* The device's position has been assgined. *)
  | ( {position= None; multinic= false; _}
    , Some [OrderedNetDev.{position; net_dev= {mac_addr; _}}] ) -> (
    (* Not a multinic funciton.
     * And found a ever-seen device which had located at the same PCI address.
     *)
    match MacaddrSet.find_opt mac_addr curr_macs with
    | None ->
        (* The ever-seen device has been removed - not in current MAC addresses.
         * This is a replacement: assign the position as before.
         *)
        debug "%s %s: assign position: %d <- %s" __FUNCTION__ dbg position
          (OrderingNetDev.to_string dev) ;
        {dev with position= Some position}
    | Some _ ->
        (* The ever-seen device is still in current list.
         * It must have been positioned via the MAC address. But its PCI address changes.
         *)
        debug "%s %s: skip (seen) %s" __FUNCTION__ dbg
          (OrderingNetDev.to_string dev) ;
        dev (* Leave the current one unassigned at the moment. *)
  )
  | _ ->
      debug "%s %s: skip %s" __FUNCTION__ dbg (OrderingNetDev.to_string dev) ;
      dev (* leave the current one unassigned at the moment. *)

let assign_position_for_multinic_funcs
    ~(last_pcis : OrderedNetDev.t list PciAddrMap.t)
    ~(assigned_positions : IntSet.t) (multinics : OrderingNetDev.t MacaddrMap.t)
    : OrderingNetDev.t MacaddrMap.t =
  let open OrderingNetDev in
  MacaddrMapToPciAddrMap.to_1n_map
    ~by:(fun _k v -> v.net_dev.pci_addr)
    multinics
  |> PciAddrMap.mapi
       (fun (pci_addr : PciAddr.t) (devs : OrderingNetDev.t list) ->
         (* The devices were previously occupying the PCI address.
          * The positions of these devices are called the "last positions".
          *)
         let last_devs : OrderedNetDev.t list =
           PciAddrMap.find_opt pci_addr last_pcis
           |> Option.fold ~none:[] ~some:Fun.id
         in
         match
           ( List.exists
               (fun OrderedNetDev.{position; _} ->
                 IntSet.mem position assigned_positions
               )
               last_devs
           , List.length devs = List.length last_devs
           , List.exists (fun dev -> Option.is_some dev.position) devs
           )
         with
         | false, true, false ->
             (* All the "last positions" have not been assigned yet.
              * And no change on the number of devices sharing the PCI address.
              * And none of them are assigned positions so far. The MAC addresses of all have changed.
              * Re-assign the "last positions" by sorting with MAC addresses.
              *)
             let devs' = List.sort compare_on_mac devs in
             let lasts' = List.sort OrderedNetDev.compare_on_mac last_devs in
             List.rev_map2
               (fun dev last ->
                 let position = last.OrderedNetDev.position in
                 debug "%s: assign position: %d <- %s" __FUNCTION__ position
                   (OrderingNetDev.to_string dev) ;
                 {dev with position= Some position}
               )
               devs' lasts'
         | _ ->
             (* Some of the "last positions" have been assigned:
                  assigned by MAC - assigned to the same devices but more new ones reported, or
                  assigned by PCI - the assigned device must not be multinic and the multinic function changes to another PCI address.
                  
              * Or the number of devices sharing the PCI address changes:
                  the multinic function changes to another PCI address, or
                  new devices are reported.

              * Or some of the devices have already been assigned with positions,
              *   that more new devices are reported.
              *
              * In the case that the multinic function changes to another PCI address,
              * assigning by MAC will handle the devices with the same MAC address.
              * Otherwise, both MAC and PCI change, no way to keep them stable with the lasts and olds.
              *)
             let positioned, not_positioned =
               List.partition (fun dev -> Option.is_some dev.position) devs
             in
             (* Update the BIOS eth order for ordering later. *)
             let not_positioned' =
               (* Collect all BIOS eth order numbers *)
               let bios_eth_orders =
                 not_positioned
                 |> List.map (fun dev -> dev.bios_eth_order)
                 |> List.sort compare
               in
               (* Re-assgin the BIOS eth order by zipping the BIOS eth order and MAC order. *)
               not_positioned
               |> List.stable_sort compare_on_mac
               |> List.rev_map2
                    (fun bios_eth_order dev -> {dev with bios_eth_order})
                    bios_eth_orders
             in
             List.rev_append positioned not_positioned'
     )
  |> Fun.flip (PciAddrMap.fold (fun _ l acc -> List.rev_append acc l)) []
  |> ListToMacaddrMap.to_11_map ~by:(fun dev -> dev.net_dev.mac_addr)

let assign_position_for_remaining ~(max_position : int)
    (devs : OrderingNetDev.t list) : int * OrderingNetDev.t list =
  List.fold_left
    (fun (acc_pos, acc) dev ->
      debug "%s: assign position: %d <- %s" __FUNCTION__ acc_pos
        (OrderingNetDev.to_string dev) ;
      let pos = acc_pos + 1 in
      (pos, {dev with position= Some pos} :: acc)
    )
    (max_position, []) devs

let changes_on_olds ~(currents : UnOrderedNetDev.t MacaddrMap.t)
    ~(lasts : OrderedNetDev.t MacaddrMap.t)
    ~(olds : OrderedNetDev.t MacaddrMap.t) : int * OrderedNetDev.t list =
  let new_olds =
    olds
    |> MacaddrMap.filter (fun mac_addr _ ->
           (* Not being added back *)
           not (MacaddrMap.mem mac_addr currents)
       )
    |> OrderedNetDev.map_by_position
    |> MacaddrMap.fold
         (fun last_mac_addr dev acc_olds ->
           match MacaddrMap.find_opt last_mac_addr currents with
           | Some _ ->
               (* The last is in currents. *)
               acc_olds
           | None ->
               (* The last is not in currents. It has been removed.
                * Add the last into olds, or replace the old with the same position.
                *)
               IntMap.update dev.OrderedNetDev.position (Fun.const (Some dev))
                 acc_olds
         )
         lasts
  in
  let max =
    IntMap.max_binding_opt new_olds |> Option.fold ~none:(-1) ~some:fst
  in
  (max, new_olds |> IntMap.bindings |> List.map snd)

let assigned_positions (orderings : OrderingNetDev.t MacaddrMap.t) : IntSet.t =
  MacaddrMap.fold
    (fun _ dev acc ->
      match dev with
      | OrderingNetDev.{position= Some position; _} ->
          IntSet.add position acc
      | _ ->
          acc
    )
    orderings IntSet.empty

let generate_order ~(currents : UnOrderedNetDev.t MacaddrMap.t)
    ~(mappings : NetDevMapping.t list) ~(lasts : OrderedNetDev.t MacaddrMap.t)
    ~(olds : OrderedNetDev.t MacaddrMap.t) :
    int * OrderedNetDev.t list * OrderedNetDev.t list =
  let open OrderingNetDev in
  let curr_macs : MacaddrSet.t =
    MacaddrMap.bindings currents |> List.map fst |> MacaddrSet.of_list
  in
  let last_pcis = OrderedNetDev.map_by_pci lasts in
  let old_pcis = OrderedNetDev.map_by_pci olds in
  let orderings =
    let module M = MacaddrMap in
    currents
    |> of_unordered_map
    |> M.map (assign_position_by_mapping ~mappings)
    |> M.map (assign_position_by_mac ~dbg:"lasts" ~lasts)
    |> M.map
         (assign_position_by_pci ~dbg:"lasts" ~seen_pcis:last_pcis ~curr_macs)
    |> M.map (assign_position_by_pci ~dbg:"olds" ~seen_pcis:old_pcis ~curr_macs)
    |> M.map (assign_position_by_mac ~dbg:"olds" ~lasts:olds)
  in
  let orderings =
    (* Partitioning the ordering list into two:
     *   multinics - the devices each share a PCI BUS ID with others (multinic function).
     *   others - the deivces each occupy a PCI BUS ID exclusively.
     *)
    let assigned_positions = assigned_positions orderings in
    let ( (multinics : OrderingNetDev.t MacaddrMap.t)
        , (others : OrderingNetDev.t MacaddrMap.t) ) =
      orderings |> MacaddrMap.partition (fun _mac dev -> dev.multinic)
    in
    let f _ _ _ = failwith "Impossible duplication on MAC address." in
    assign_position_for_multinic_funcs ~last_pcis ~assigned_positions multinics
    |> MacaddrMap.union f others
  in
  let positioned, not_positioned =
    orderings
    |> MacaddrMap.partition (fun _mac dev -> Option.is_some dev.position)
  in
  let max_pos_in_olds, new_olds = changes_on_olds ~currents ~lasts ~olds in
  let max_position =
    MacaddrMap.fold
      (fun _key dev max ->
        match dev.position with Some pos when pos > max -> pos | _ -> max
      )
      positioned max_pos_in_olds
  in
  let new_max_posistion, ordered_remaining =
    MacaddrMap.bindings not_positioned
    |> List.map snd
    |> List.stable_sort compare_on_bios_eth_order
    |> assign_position_for_remaining ~max_position
  in
  let new_ordered : OrderedNetDev.t list =
    ordered_remaining
    |> List.rev_append (MacaddrMap.bindings positioned |> List.map snd)
    |> List.map OrderedNetDev.of_ordering
  in
  OrderedNetDev.assert_no_duplicate_position new_ordered ;
  OrderedNetDev.assert_no_duplicate_position new_olds ;
  (new_max_posistion, new_ordered, new_olds)

let generate ?(force = false) () =
  let open OrderedNetDev in
  let currents = UnOrderedNetDev.get_all () in
  let lasts : t MacaddrMap.t =
    if force then MacaddrMap.empty else order_of_file ~path:last_file_path
  in
  let mappings : NetDevMapping.t list =
    if MacaddrMap.is_empty lasts then
      NetDevMapping.mappings_of_file ~path:initial_rules_file_path
    else
      []
  in
  let olds : t MacaddrMap.t = order_of_file ~path:old_file_path in
  let (max_position : int), (new_ordered : t list), (new_olds : t list) =
    generate_order ~currents ~mappings ~lasts ~olds
  in
  debug "%s" "New order:" ;
  new_ordered |> List.iter (fun x -> debug "%s" (to_string x)) ;
  write_to_file ~path:last_file_path new_ordered ;
  debug "%s" "New olds:" ;
  new_olds |> List.iter (fun x -> debug "%s" (to_string x)) ;
  write_to_file ~path:old_file_path new_olds ;

  let order' =
    let m = ListToIntMap.to_11_map ~by:(fun d -> d.position) new_ordered in
    List.init max_position (fun position ->
        let name =
          IntMap.find_opt position m |> Option.map (fun d -> d.net_dev.name)
        in
        (position, name)
    )
  in
  (* Record the order in memory *)
  order := order' ;
  (* Find the NICs whose name change *)
  let m = map_by_position lasts in
  List.fold_left
    (fun acc {position; net_dev= {name= curr_name; _}} ->
      match IntMap.find_opt position m with
      | Some {net_dev= {name= last_name; _}; _} when last_name <> curr_name ->
          (last_name, curr_name) :: acc
      | _ ->
          acc
    )
    [] new_ordered

let order () : (int * string option) list = !order

let order_of_eths () : (int * string option) list =
  Network_utils.Sysfs.list ()
  |> List.map (fun eth_name -> (n_of_ethn eth_name, Some eth_name))
