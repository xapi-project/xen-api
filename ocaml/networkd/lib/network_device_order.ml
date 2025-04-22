(*
 * Copyright (c) Cloud Software Group, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Generate an order for host network devices and keep the order as stable as possible.
  *)

module D = Debug.Make (struct let name = __MODULE__ end)

open D
open Network_interface

let initial_rules_file_path =
  "/etc/firstboot.d/data/initial_network_device_rules.conf"

let ( let* ) = Result.bind

let cmd_biosdevname = "/usr/sbin/biosdevname"

type error =
  | Pci_addr_parse_error of string
  | Mac_addr_parse_error of string
  | Rule_parse_error of string
  | Missing_biosdevname_key of string
  | Duplicate_mac_address
  | Duplicate_position
  | Invalid_biosdevname_key_value of (string * string)

let string_of_error = function
  | Pci_addr_parse_error s ->
      Printf.sprintf "Invalid PCI address: %s" s
  | Mac_addr_parse_error s ->
      Printf.sprintf "Invalid MAC address: %s" s
  | Rule_parse_error s ->
      Printf.sprintf "Invalid rule: %s" s
  | Missing_biosdevname_key k ->
      Printf.sprintf "Missing key in biosdevname output: %s" k
  | Duplicate_mac_address ->
      "Duplicate MAC address"
  | Duplicate_position ->
      "Duplicate position"
  | Invalid_biosdevname_key_value (k, v) ->
      Printf.sprintf "Invalid key-value pair in biosdevname output: %s=%s" k v

module Pciaddr = struct
  type t = Xcp_pci.address

  let default = Xcp_pci.{domain= 0; bus= 0; dev= 0; fn= 0}

  let to_string = Xcp_pci.string_of_address

  let of_string s =
    try Ok (Xcp_pci.address_of_string s)
    with _ -> Error (Pci_addr_parse_error s)

  let compare t1 t2 =
    let open Xcp_pci in
    let ( <?> ) a b = if a = 0 then b else a in
    compare t1.domain t2.domain
    <?> compare t1.bus t2.bus
    <?> compare t1.dev t2.dev
    <?> compare t1.fn t2.fn
end

module Macaddr = struct
  include Macaddr

  let of_string s =
    of_string s |> Result.map_error (fun _ -> Mac_addr_parse_error s)
end

module PciaddrMap = Map.Make (Pciaddr)
module MacaddrSet = Set.Make (Macaddr)
module MacaddrMap = Map.Make (Macaddr)
module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)

module UniqueMap (M : Map.S) : sig
  exception Duplicate_key

  val of_unique_list : ('a -> M.key) -> 'a list -> 'a M.t
  (** [of_unique_list map lst] creates a map with the values in [lst]. Their
      keys are created by calling [map value]. Raises [Duplicate_key] whenever
      more than one value in [lst] produces the same key when calling
      [map value]. *)
end = struct
  exception Duplicate_key

  let fail _ = raise Duplicate_key

  let of_unique_list map l =
    List.fold_left
      (fun acc v ->
        let f x = Some (Option.fold ~none:v ~some:fail x) in
        M.update (map v) f acc
      )
      M.empty l
end

module MultiMap (M : Map.S) : sig
  val of_list : ('a -> M.key) -> 'a list -> 'a list M.t
  (** [of_list map lst] creates a map with the values in [lst]. Their keys are
      created by calling [map value]. Whenever more than a value generates the
      key when calling [map value], the values are concatenated as a list. *)
end = struct
  let of_list map l =
    List.fold_left
      (fun acc v ->
        let f x = Some (Option.fold ~none:[v] ~some:(List.cons v) x) in
        M.update (map v) f acc
      )
      M.empty l
end

module IntUniqueMap = UniqueMap (IntMap)
module MacaddrUniqueMap = UniqueMap (MacaddrMap)
module PciaddrMultiMap = MultiMap (PciaddrMap)

let fold_results (l : ('a, 'e) result list) : ('a list, 'e) result =
  List.fold_left
    (fun acc r ->
      match (acc, r) with
      | Ok acc, Ok r ->
          Ok (r :: acc)
      | Error error, _ ->
          Error error
      | Ok _, Error error ->
          Error error
    )
    (Ok []) l

module Rule = struct
  type index = Mac_addr of Macaddr.t | Pci_addr of Pciaddr.t | Label of string

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
          let to_rule index = Ok {position; index} in
          match ty with
          | "pci" ->
              let* pci = Pciaddr.of_string value in
              to_rule (Pci_addr pci)
          | "mac" ->
              let* mac = Macaddr.of_string value in
              to_rule (Mac_addr mac)
          | "label" ->
              to_rule (Label value)
          | _ ->
              Error (Rule_parse_error line)
      )
    with _ -> Error (Rule_parse_error line)

  let validate (l : (t, error) result list) =
    let* rules = fold_results l in
    try
      IntUniqueMap.of_unique_list (fun dev -> dev.position) rules |> ignore ;
      Ok rules
    with IntUniqueMap.Duplicate_key -> Error Duplicate_position

  let read ~(path : string) : (t list, error) result =
    if not (Sys.file_exists path) then
      Ok []
    else
      Xapi_stdext_unix.Unixext.read_lines ~path |> List.map parse |> validate
end

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

  let n_of_ethn ethn =
    try Ok (Scanf.sscanf ethn "eth%d" (fun n -> n))
    with _ -> Error (Invalid_biosdevname_key_value ("BIOS device", ethn))

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
      , fun r v ->
          let* bios_eth_order = n_of_ethn v in
          Ok {r with bios_eth_order}
      )
    ; ("Kernel name", fun r v -> Ok {r with name= v})
    ; ( "Assigned MAC"
      , fun r v ->
          let* mac = Macaddr.of_string v in
          Ok {r with mac}
      )
    ; ( "Bus Info"
      , fun r v ->
          let* pci = Pciaddr.of_string v in
          Ok {r with pci}
      )
    ]
    |> List.fold_left
         (fun acc (k, f) ->
           let* r = acc in
           match List.assoc_opt k kvs with
           | Some v ->
               Result.map_error
                 (fun _ -> Invalid_biosdevname_key_value (k, v))
                 (f r v)
           | None ->
               Error (Missing_biosdevname_key k)
         )
         (Ok default)

  let update_multi_nic devs =
    let pci_cnt =
      let f o = Some (Option.fold ~none:1 ~some:(fun c -> c + 1) o) in
      List.fold_left
        (fun acc dev -> PciaddrMap.update dev.pci f acc)
        PciaddrMap.empty devs
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
      devs

  let get_all () : (t list, error) result =
    let* devs =
      Network_utils.call_script cmd_biosdevname
        ["--policy"; "all_ethN"; "-d"; "-x"]
      |> Astring.String.cuts ~sep:"\n\n"
      |> List.filter (fun line -> line <> "")
      |> List.map parse
      |> fold_results
    in
    try
      MacaddrUniqueMap.of_unique_list (fun v -> v.mac) devs |> ignore ;
      Ok (update_multi_nic devs)
    with MacaddrUniqueMap.Duplicate_key -> Error Duplicate_mac_address
end

module OrderedDev = struct
  type t = Network_interface.ordered_iface

  let compare_on_mac t1 t2 = Macaddr.compare t1.mac t2.mac

  let to_string t =
    Printf.sprintf "position=%d; name=%s; MAC=%s; PCI=%s; present=%s" t.position
      t.name (Macaddr.to_string t.mac) (Pciaddr.to_string t.pci)
      (string_of_bool t.present)

  let map_by_pci (l : t list) : t list PciaddrMap.t =
    PciaddrMultiMap.of_list (fun v -> v.pci) l

  let map_by_position (l : t list) : (t IntMap.t, error) result =
    try Ok (IntUniqueMap.of_unique_list (fun v -> v.position) l)
    with _ -> Error Duplicate_position

  let validate_no_duplicate_position (l : t list) : (t list, error) result =
    try
      IntUniqueMap.of_unique_list (fun dev -> dev.position) l |> ignore ;
      Ok l
    with _ -> Error Duplicate_position

  let validate_no_duplicate_mac (l : t list) : (t list, error) result =
    try
      MacaddrUniqueMap.of_unique_list (fun dev -> dev.mac) l |> ignore ;
      Ok l
    with _ -> Error Duplicate_mac_address

  let validate_order (l : t list) : (t list, error) result =
    let* l = validate_no_duplicate_position l in
    validate_no_duplicate_mac l

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
        List.find_opt
          (Rule.matches ~mac:dev.mac ~pci:dev.pci ~label:dev.name)
          rules
      with
      | Some {position; _} ->
          debug "%s: assign position: %d <- %s" __FUNCTION__ position
            (Dev.to_string dev) ;
          let dev' = OrderedDev.assign_position dev position in
          (dev' :: acc_ordered, acc_unordered)
      | None ->
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
        (* Not a multi-nic function.
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
        PciaddrMap.find_opt pci last_pcis |> Option.value ~default:[]
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
    (PciaddrMultiMap.of_list (fun v -> v.Dev.pci) multinics)
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
    ~(last_order : OrderedDev.t list) : (OrderedDev.t list, error) result =
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
  let* m = OrderedDev.map_by_position ordered in
  let removed =
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
  OrderedDev.validate_order new_order

let sort last_order =
  let* rules = Rule.read ~path:initial_rules_file_path in
  let rules, last_order =
    if last_order = [] then
      (rules, [])
    else
      ([], last_order)
  in
  let* currents = Dev.get_all () in
  currents
  |> List.iter (fun x -> debug "%s current: %s" __FUNCTION__ (Dev.to_string x)) ;
  let* new_order = sort' ~currents ~rules ~last_order in
  new_order
  |> List.iter (fun x ->
         debug "%s new order: %s" __FUNCTION__ (OrderedDev.to_string x)
     ) ;

  (* Find the NICs whose name changes *)
  let* m = OrderedDev.map_by_position last_order in
  let changes =
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
  Ok (new_order, changes)
