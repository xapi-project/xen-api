(** Generete an order for host network devices and keep the order as stable as possible.
  *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D
open Network_interface

let initial_rules_file_path = "/var/lib/xcp/initial_network_device_rules.conf"

let cmd_biosdevname = "/usr/sbin/biosdevname"

(** PCI address in format SBDF: domain:bus:device:function *)
module Pciaddr = struct
  type t = Xcp_pci.address

  let default = Xcp_pci.{domain= 0; bus= 0; dev= 0; fn= 0}

  let to_string = Xcp_pci.string_of_address

  exception Parse_error of string

  let of_string_exn s =
    try Xcp_pci.address_of_string s with _ -> raise (Parse_error s)

  let compare t1 t2 =
    let open Xcp_pci in
    match
      ( compare t1.domain t2.domain
      , compare t1.bus t2.bus
      , compare t1.dev t2.dev
      , compare t1.fn t2.fn
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

module PciaddrMap = Map.Make (Pciaddr)
module MacaddrSet = Set.Make (Macaddr)
module MacaddrMap = Map.Make (Macaddr)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module ToMap = struct
  module type Map = sig
    type !+'a t

    type key

    val empty : 'a t

    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  end

  module Make (M : Map) = struct
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

    (** [to_1n_map ~by m] is a map. For each binding in the map, the key is [by v] and the value is a list of [v] which are values in the [m]. *)
    let to_1n_map ~by l =
      List.fold_left
        (fun acc v ->
          let f x = Some (Option.fold ~none:[v] ~some:(List.cons v) x) in
          M.update (by v) f acc
        )
        M.empty l
  end
end

module ListToIntMap = ToMap.Make (IntMap)
module ListToMacaddrMap = ToMap.Make (MacaddrMap)
module ListToPciaddrMap = ToMap.Make (PciaddrMap)

(** A rule is to specify a position for a network device which can be
    identified by MAC address, PCI address, or name label. *)
module Rule = struct
  type index = Mac_addr of Macaddr.t | Pci_addr of Pciaddr.t | Label of string

  exception Parse_error of string

  type t = {position: int; index: index}

  let matches ~(mac : Macaddr.t) ~(pci : Pciaddr.t) ~(label : string) t : bool =
    match t.index with
    | Mac_addr mac' ->
        mac' = mac
    | Pci_addr pci' ->
        pci' = pci
    | Label label' ->
        label' = label

  let parse line =
    debug "%s: line: %s" __FUNCTION__ line ;
    try
      Scanf.sscanf line {|%d:%s@="%s@"|} (fun position ty value ->
          let index =
            match ty with
            | "pci" ->
                Pci_addr (Pciaddr.of_string_exn value)
            | "mac" ->
                Mac_addr (Macaddr.of_string_exn value)
            | "label" ->
                Label value
            | _ ->
                raise (Parse_error line)
          in
          {position; index}
      )
    with
    | Parse_error _ as e ->
        raise e
    | _ ->
        raise (Parse_error line)

  exception Duplicate_position

  let assert_no_duplicate_position (l : t list) =
    try ListToIntMap.to_11_map ~by:(fun dev -> dev.position) l |> ignore
    with ListToIntMap.Duplicate_key -> raise Duplicate_position

  let read ~(path : string) : t list =
    if not (Sys.file_exists path) then
      []
    else
      let read_lines = Xapi_stdext_unix.Unixext.read_lines in
      let l = read_lines ~path |> List.map parse in
      assert_no_duplicate_position l ;
      l
end

exception Not_ethN of string

let n_of_ethn ethn =
  try Scanf.sscanf ethn "eth%d" (fun n -> n) with _ -> raise (Not_ethN ethn)

(** A network device which has not been assigned a position in the order. *)
module Dev = struct
  type t = {
      name: Network_interface.iface
    ; mac: Network_interface.mac_address
    ; pci: Xcp_pci.address
    ; bios_eth_order: int
    ; multi_nic: bool
  }

  let default =
    {
      name= ""
    ; mac= Macaddr.of_string_exn "00:00:00:00:00:00"
    ; pci= Pciaddr.default
    ; bios_eth_order= -1
    ; multi_nic= false
    }

  let compare_on_mac t1 t2 = Macaddr.compare t1.mac t2.mac

  let compare_on_bios_eth_order t1 t2 =
    compare t1.bios_eth_order t2.bios_eth_order

  let to_string t =
    Printf.sprintf "Name=%s; MAC=%s; PCI=%s; bios_eth_order=%d; multi_nic=%s"
      t.name (Macaddr.to_string t.mac) (Pciaddr.to_string t.pci)
      t.bios_eth_order
      (string_of_bool t.multi_nic)

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
    ; ("Kernel name", fun acc value -> {acc with name= value})
    ; ( "Assigned MAC"
      , fun acc value -> {acc with mac= Macaddr.of_string_exn value}
      )
    ; ("Bus Info", fun acc value -> {acc with pci= Pciaddr.of_string_exn value})
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

  let update_multi_nic currents =
    let pci_cnt =
      let f o = Some (Option.fold ~none:1 ~some:(fun c -> c + 1) o) in
      List.fold_left
        (fun acc dev -> PciaddrMap.update dev.pci f acc)
        PciaddrMap.empty currents
    in
    List.map
      (fun dev : t ->
        let multi_nic =
          (* Will never raise exception or be < 1 *)
          let c = PciaddrMap.find dev.pci pci_cnt in
          if c > 1 then true else false
        in
        {dev with multi_nic}
      )
      currents

  let get_all () : t list =
    try
      let all =
        Network_utils.call_script cmd_biosdevname
          ["--policy"; "all_ethN"; "-d"; "-x"]
        |> Astring.String.cuts ~sep:"\n\n"
        |> List.filter (fun line -> line <> "")
        |> List.map parse
        |> update_multi_nic
      in
      ListToMacaddrMap.to_11_map ~by:(fun v -> v.mac) all |> ignore ;
      all
    with
    | ListToMacaddrMap.Duplicate_key ->
        raise Duplicate_mac_address
    | e ->
        error "%s" "Can't parse the output of the biosdevname!" ;
        raise e
end

(** A network device which has been in an order. *)
module OrderedDev = struct
  type t = Network_interface.ordered_iface

  let compare_on_mac t1 t2 = Macaddr.compare t1.mac t2.mac

  let to_string t =
    Printf.sprintf "position=%d; name=%s; MAC=%s; PCI=%s; present=%s" t.position
      t.name (Macaddr.to_string t.mac) (Pciaddr.to_string t.pci)
      (string_of_bool t.present)

  let map_by_pci (l : t list) : t list PciaddrMap.t =
    ListToPciaddrMap.to_1n_map ~by:(fun v -> v.pci) l

  let map_by_position (l : t list) : t IntMap.t =
    ListToIntMap.to_11_map ~by:(fun v -> v.position) l

  exception Duplicate_position

  let assert_no_duplicate_position (l : t list) =
    try ListToIntMap.to_11_map ~by:(fun dev -> dev.position) l |> ignore
    with _ -> raise Duplicate_position

  exception Duplicate_mac_address

  let assert_no_duplicate_mac (l : t list) =
    try ListToMacaddrMap.to_11_map ~by:(fun dev -> dev.mac) l |> ignore
    with _ -> raise Duplicate_mac_address

  let assign_position (dev : Dev.t) position =
    Network_interface.
      {name= dev.name; mac= dev.mac; pci= dev.pci; position; present= true}
end

type ordering = OrderedDev.t list * Dev.t list

let assign_position_by_rules ~(rules : Rule.t list)
    ((ordered, unordered) : ordering) : ordering =
  List.fold_left
    (fun (acc_ordered, acc_unordered) (dev : Dev.t) ->
      match
        List.filter
          (Rule.matches ~mac:dev.mac ~pci:dev.pci ~label:dev.name)
          rules
      with
      | {position; _} :: _ ->
          debug "%s: assign position: %d <- %s" __FUNCTION__ position
            (Dev.to_string dev) ;
          let dev' = OrderedDev.assign_position dev position in
          (dev' :: acc_ordered, acc_unordered)
      | [] ->
          (acc_ordered, dev :: acc_unordered)
    )
    (ordered, []) unordered

let assign_position_by_mac ~(last_order : OrderedDev.t list)
    ((ordered, unordered) : ordering) : ordering =
  List.fold_left
    (fun (acc_ordered, acc_unordered) (dev : Dev.t) ->
      match List.find_opt (fun dev' -> dev.mac = dev'.mac) last_order with
      | Some {position; _} ->
          (* Found a MAC matched network device in [last_order]: assign the position as last. *)
          debug "%s: assign position: %d <- %s" __FUNCTION__ position
            (Dev.to_string dev) ;
          let dev' = OrderedDev.assign_position dev position in
          (dev' :: acc_ordered, acc_unordered)
      | None ->
          debug "%s: skip %s" __FUNCTION__ (Dev.to_string dev) ;
          (* a new network device: leave it unassigned at the moment *)
          (acc_ordered, dev :: acc_unordered)
    )
    (ordered, []) unordered

let assign_position_by_pci ~(last_pcis : OrderedDev.t list PciaddrMap.t)
    ~(curr_macs : MacaddrSet.t) ((ordered, unordered) : ordering) : ordering =
  List.fold_left
    (fun (acc_ordered, acc_unordered) (dev : Dev.t) ->
      match (dev, PciaddrMap.find_opt dev.pci last_pcis) with
      | Dev.{multi_nic= false; _}, Some [{position; mac; _}] -> (
        (* Not a multi-nic funciton.
           And found a ever-seen device which had located at the same PCI address. *)
        match MacaddrSet.find_opt mac curr_macs with
        | None ->
            (* The ever-seen device has been removed - not in current MAC addresses.
               This is a replacement: assign the position as before. *)
            debug "%s: assign position: %d <- %s" __FUNCTION__ position
              (Dev.to_string dev) ;
            let dev' = OrderedDev.assign_position dev position in
            (dev' :: acc_ordered, acc_unordered)
        | Some _ ->
            (* The ever-seen device is still presenting this time.
               It must have been positioned via the MAC address already. But its PCI address changes. *)
            debug "%s: skip (seen) %s" __FUNCTION__ (Dev.to_string dev) ;
            (acc_ordered, dev :: acc_unordered)
      )
      | _ ->
          debug "%s: skip %s" __FUNCTION__ (Dev.to_string dev) ;
          (acc_ordered, dev :: acc_unordered)
    )
    (ordered, []) unordered

let assign_position_for_multinic ~(last_pcis : OrderedDev.t list PciaddrMap.t)
    ~(assigned_positions : IntSet.t) (multinics : Dev.t list) : ordering =
  PciaddrMap.fold
    (fun pci devs (acc_ordered, acc_unordered) ->
      (* The [last_devs] are the devices which were previously occupying the PCI address.
         The positions of these devices are called the "last positions". *)
      let last_devs =
        PciaddrMap.find_opt pci last_pcis |> Option.fold ~none:[] ~some:Fun.id
      in
      match
        ( List.exists
            (fun {position; _} -> IntSet.mem position assigned_positions)
            last_devs
        , List.length devs = List.length last_devs
        )
      with
      | false, true ->
          (* All the "last positions" have not been assigned yet.
             And no change on the number of devices sharing the PCI address.
             Re-assign the "last positions" by sorting with MAC addresses. *)
          let devs' = List.sort Dev.compare_on_mac devs in
          let lasts' = List.sort OrderedDev.compare_on_mac last_devs in
          let ordered_devs =
            List.rev_map2
              (fun dev last ->
                let position = last.position in
                debug "%s: assign position: %d <- %s" __FUNCTION__ position
                  (Dev.to_string dev) ;
                OrderedDev.assign_position dev position
              )
              devs' lasts'
          in
          (List.rev_append ordered_devs acc_ordered, acc_unordered)
      | true, _
      (* Some of the "last positions" have been assigned by MAC address.
         But there are some new ones reported this time. *)
      | false, false ->
          (* This means at this PCI address, the devices have completely
             different MAC addresses and the number of devices changes as well.
             Consider them being new devices. *)

          (* Collect all BIOS eth order numbers *)
          let bios_eth_orders =
            devs
            |> List.map (fun dev -> dev.Dev.bios_eth_order)
            |> List.sort compare
          in
          (* Re-assgin the BIOS eth order by zipping the BIOS eth order and MAC order. *)
          let unordered_devs =
            devs
            |> List.stable_sort Dev.compare_on_mac
            |> List.rev_map2
                 (fun bios_eth_order dev -> Dev.{dev with bios_eth_order})
                 bios_eth_orders
          in
          (acc_ordered, List.rev_append unordered_devs acc_unordered)
    )
    (ListToPciaddrMap.to_1n_map ~by:(fun v -> v.Dev.pci) multinics)
    ([], [])

let assign_position_for_remaining ~(max_position : int) (devs : Dev.t list) :
    OrderedDev.t list =
  List.fold_left
    (fun (acc_pos, acc) (dev : Dev.t) ->
      let pos = acc_pos + 1 in
      debug "%s: assign position: %d <- %s" __FUNCTION__ pos (Dev.to_string dev) ;
      let dev' = OrderedDev.assign_position dev pos in
      (pos, dev' :: acc)
    )
    (max_position, []) devs
  |> snd

let sort' ~(currents : Dev.t list) ~(rules : Rule.t list)
    ~(last_order : OrderedDev.t list) : OrderedDev.t list =
  let open Dev in
  let curr_macs =
    currents |> List.map (fun dev -> dev.mac) |> MacaddrSet.of_list
  in
  let last_pcis = OrderedDev.map_by_pci last_order in
  let ordered, unordered =
    ([], currents)
    |> assign_position_by_rules ~rules
    |> assign_position_by_mac ~last_order
    |> assign_position_by_pci ~last_pcis ~curr_macs
  in
  let ordered, remaining =
    (* Split the unordered list into two:
         multinics - the devices each share a PCI BUS ID with others (multinic function).
         remaining - the deivces each occupy a PCI BUS ID exclusively. *)
    let multinics, remaining =
      unordered |> List.partition (fun dev -> dev.multi_nic)
    in
    let assigned_positions =
      ordered |> List.map (fun dev -> dev.position) |> IntSet.of_list
    in
    let ordered', unordered' =
      assign_position_for_multinic ~last_pcis ~assigned_positions multinics
    in
    (List.rev_append ordered ordered', List.rev_append remaining unordered')
  in
  let removed =
    let m = OrderedDev.map_by_position ordered in
    last_order
    |> List.filter_map (fun (dev : OrderedDev.t) ->
           if MacaddrSet.mem dev.mac curr_macs then
             None
           else
             Some {dev with present= false}
       )
    |> List.filter (fun dev -> not (IntMap.mem dev.position m))
  in
  let ordered = List.rev_append ordered removed in
  let max_position =
    List.fold_left
      (fun max dev -> if max < dev.position then dev.position else max)
      (-1) ordered
  in
  let new_order =
    remaining
    |> List.stable_sort compare_on_bios_eth_order
    |> assign_position_for_remaining ~max_position
    |> List.rev_append ordered
  in
  OrderedDev.assert_no_duplicate_position new_order ;
  OrderedDev.assert_no_duplicate_mac new_order ;
  new_order

let sort last_order =
  let rules, last_order =
    if last_order = [] then
      (Rule.read ~path:initial_rules_file_path, [])
    else
      ([], last_order)
  in
  let currents = Dev.get_all () in
  currents
  |> List.iter (fun x -> debug "%s current: %s" __FUNCTION__ (Dev.to_string x)) ;
  let new_order = sort' ~currents ~rules ~last_order in
  new_order
  |> List.iter (fun x ->
         debug "%s new order: %s" __FUNCTION__ (OrderedDev.to_string x)
     ) ;

  (* Find the NICs whose name changes *)
  let changes =
    let m = OrderedDev.map_by_position last_order in
    List.fold_left
      (fun acc {position; name= curr; _} ->
        match IntMap.find_opt position m with
        | Some {name= last; _} when last <> curr ->
            (last, curr) :: acc
        | _ ->
            acc
      )
      [] new_order
  in
  (new_order, changes)
