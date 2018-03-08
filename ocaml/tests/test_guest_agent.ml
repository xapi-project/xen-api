(*
 * Copyright (C) Citrix Systems Inc.
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

open OUnit
open Test_highlevel

module Networks = Generic.Make (struct
    module Io = struct
      type input_t = string list
      type output_t = (string * string) list

      let string_of_input_t = Test_printers.(list string)
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    type 'a tree = T of 'a * 'a tree list

    let rec add_path_to_tree (T(root, children)) = function
      | [] -> (T(root, children))
      | node :: rest_of_path ->
        try 
          let T(_, children_of_node) = List.find (fun (T(n, _)) -> n = node) children in
          let t = add_path_to_tree (T(node, children_of_node)) rest_of_path in
          T(root, t :: (List.filter (fun (T(n, _)) -> n <> node) children))
        with Not_found ->
          T(root, (add_path_to_tree (T(node, [])) rest_of_path) :: children)

    let construct_tree tree path =
      let open Stdext.Xstringext in
      let nodes = String.split_f (fun s -> s = '/') path in
      add_path_to_tree tree nodes

    let rec list_helper children = function
      | [] -> List.map (fun (T(node, _)) -> node) children
      | node :: rest_of_path ->
        try
          let T(_, children_of_node) = List.find (fun (T(n, _)) -> n = node) children in
          list_helper children_of_node rest_of_path
        with Not_found -> []

    let list (T(root, children)) path =
      let open Stdext.Xstringext in
      let nodes = String.split_f (fun s -> s = '/') path in
      list_helper children nodes


    let transform input = 
      let tree = List.fold_left construct_tree (T("", [])) input in
      List.concat [
          Xapi_guest_agent.networks "attr" "vif" (list tree)
        ; Xapi_guest_agent.networks "xenserver/attr" "net-sriov-vf" (list tree)
      ]

    let tests = [
      (* basic cases *)
      [ "attr/vif/0/ipv6/0";
      ], [ "attr/vif/0/ipv6/0", "0/ipv6/0";
      ];

      [ "attr/vif/0/ipv4/0";
      ], [ "attr/vif/0/ipv4/0", "0/ip";
           "attr/vif/0/ipv4/0", "0/ipv4/0";
      ];

      [ "attr/eth0/ip";
      ], [ "attr/eth0/ip", "0/ip";
           "attr/eth0/ip", "0/ipv4/0";
      ];

      [ "attr/eth0/ipv6/0/addr";
      ], [ "attr/eth0/ip", "0/ip";
           "attr/eth0/ip", "0/ipv4/0";
           "attr/eth0/ipv6/0/addr", "0/ipv6/0";
      ];


      (* index *)
      [ "attr/vif/1/ipv6/2";
      ], [ "attr/vif/1/ipv6/2", "1/ipv6/2";
      ];

      [ "attr/vif/1/ipv4/2";
      ], [ "attr/vif/1/ipv4/2", "1/ip";
           "attr/vif/1/ipv4/2", "1/ipv4/2";
      ];

      [ "attr/eth1/ip";
      ], [ "attr/eth1/ip", "1/ip";
           "attr/eth1/ip", "1/ipv4/0";
      ];

      [ "attr/eth1/ipv6/2/addr";
      ], [ "attr/eth1/ip", "1/ip";
           "attr/eth1/ip", "1/ipv4/0";
           "attr/eth1/ipv6/2/addr", "1/ipv6/2";
      ];

      (* multiple ip addrs *)
      [ "attr/vif/0/ipv6/0";
        "attr/vif/0/ipv6/1";
      ], [ "attr/vif/0/ipv6/1", "0/ipv6/1";
           "attr/vif/0/ipv6/0", "0/ipv6/0";
      ];

      [ "attr/vif/0/ipv4/0";
        "attr/vif/0/ipv4/1";
      ], [ "attr/vif/0/ipv4/1", "0/ipv4/1";
           "attr/vif/0/ipv4/0", "0/ip";
           "attr/vif/0/ipv4/0", "0/ipv4/0";
      ];

      [ "attr/eth0/ip";
        "attr/eth0/ipv6/0/addr";
      ], [ "attr/eth0/ip", "0/ip";
           "attr/eth0/ip", "0/ipv4/0";
           "attr/eth0/ipv6/0/addr", "0/ipv6/0";
      ];

      [ "attr/vif/0/ipv4/0";
        "attr/vif/0/ipv6/0";
      ], [ "attr/vif/0/ipv4/0", "0/ip";
           "attr/vif/0/ipv4/0", "0/ipv4/0";
           "attr/vif/0/ipv6/0", "0/ipv6/0";
      ];

      [ "attr/eth0/ip";
        "attr/vif/0/ipv4/0";
        "attr/eth0/ipv6/0/addr";
        "attr/vif/0/ipv6/0";
      ], [ "attr/vif/0/ipv4/0", "0/ip";
           "attr/vif/0/ipv4/0", "0/ipv4/0";
           "attr/vif/0/ipv6/0", "0/ipv6/0";
      ];

      (* multiple vifs and multiple ip addrs *)
      [ "attr/vif/0/ipv6/0";
        "attr/vif/0/ipv6/1";
        "attr/vif/1/ipv6/0";
        "attr/vif/1/ipv6/1";
      ], [ "attr/vif/0/ipv6/1", "0/ipv6/1";
           "attr/vif/0/ipv6/0", "0/ipv6/0";
           "attr/vif/1/ipv6/1", "1/ipv6/1";
           "attr/vif/1/ipv6/0", "1/ipv6/0";
      ];

      [ "attr/vif/0/ipv4/0";
        "attr/vif/0/ipv4/1";
        "attr/vif/1/ipv4/0";
        "attr/vif/1/ipv4/1";
      ], [ "attr/vif/0/ipv4/1", "0/ipv4/1";
           "attr/vif/0/ipv4/0", "0/ip";
           "attr/vif/0/ipv4/0", "0/ipv4/0";
           "attr/vif/1/ipv4/1", "1/ipv4/1";
           "attr/vif/1/ipv4/0", "1/ip";
           "attr/vif/1/ipv4/0", "1/ipv4/0";
      ];

      (* exceptions *)
      [ "attr/vif/0/ipv4/a";
        "attr/vif/0/ipv4/1";
      ], [];
    ]
  end)

module Initial_guest_metrics = Generic.Make (struct
    module Io = struct
      type input_t = (string * string) list
      type output_t = (string * string) list

      let string_of_input_t = Test_printers.(assoc_list string string)
      let string_of_output_t = Test_printers.(assoc_list string string)
    end

    type 'a mtree = 
      | Lf of 'a * 'a
      | Mt of 'a * 'a mtree list

    let has_name name = function
      | Lf (n, _) -> n = name
      | Mt (n, _) -> n = name

    let get_name = function
      | Lf (n, _) -> n
      | Mt (n, _) -> n

    let rec add_leaf_to_mtree paths leaf_value = function
      | Lf _ -> raise (Failure "Can't add a leaf on a leaf")
      | Mt (root, children) ->
        (match paths with
         | [] ->
           (match children with
            | [] -> Lf(root, leaf_value)
            | _ -> raise (Failure "Can't add a leaf on a tree node"))
         | node :: rest_paths ->
           try 
             let t = List.find (has_name node) children in
             (match t with
              | Lf (_, _) -> raise (Failure "Can't overwrite an existing leaf")
              | Mt (node, children_of_node) ->
                let mt = add_leaf_to_mtree rest_paths leaf_value (Mt(node, children_of_node)) in
                Mt(root, mt :: (List.filter (fun n -> not (has_name node n)) children)))
           with Not_found -> 
             Mt(root, (add_leaf_to_mtree rest_paths leaf_value (Mt(node, []))) :: children))

    let construct_mtree mtree (path, leaf_value) =
      let open Stdext.Xstringext in
      let nodes = String.split_f (fun s -> s = '/') path in
      add_leaf_to_mtree nodes leaf_value mtree

    let rec list_helper children = function
      | [] -> List.map get_name children
      | node :: rest_paths ->
        try
          match List.find (has_name node) children with
          | Lf (_, _) -> []
          | Mt (_, children_of_node) -> list_helper children_of_node rest_paths
        with Not_found -> []

    let list mtree path =
      match mtree with
      | Lf (_, _) -> []
      | Mt (_, children) ->
        let open Stdext.Xstringext in
        let paths = String.split_f (fun s -> s = '/') path in
        list_helper children paths

    let rec lookup_helper mtree = function
      | [] -> 
        (match mtree with
         | Lf (_, v) -> Some v
         | Mt (_, _) -> None)
      | node :: rest_paths ->
        (match mtree with
         | Lf (l, v) -> lookup_helper (Lf(l, v)) rest_paths
         | Mt (_, children) -> 
           try
             lookup_helper (List.find (has_name node) children) rest_paths
           with Not_found -> None)

    let lookup mtree path =
        let open Stdext.Xstringext in
        let paths = String.split_f (fun s -> s = '/') path in
        lookup_helper mtree paths


    let transform input = 
      let tree = List.fold_left construct_mtree (Mt("", [])) input in
      let guest_metrics = Xapi_guest_agent.get_initial_guest_metrics (lookup tree) (list tree) in
      guest_metrics.Xapi_guest_agent.networks


    let tests = [
      (* basic cases *)
      [ "attr/vif/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "xenserver/attr/net-sriov-vf/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "attr/vif/0/ipv4/0", "192.168.0.1";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
      ];

      [ "xenserver/attr/net-sriov-vf/0/ipv4/0", "192.168.0.1";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
      ];

      [ "attr/eth0/ip", "192.168.0.1";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
      ];

      [ "attr/eth0/ipv6/0/addr", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      (* index *)
      [ "attr/vif/1/ipv6/2", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "1/ipv6/2", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "xenserver/attr/net-sriov-vf/1/ipv6/2", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "1/ipv6/2", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "attr/vif/1/ipv4/2", "192.168.0.1";
      ], [ "1/ip", "192.168.0.1";
           "1/ipv4/2", "192.168.0.1";
      ];

      [ "attr/eth1/ip", "192.168.0.1";
      ], [ "1/ip", "192.168.0.1";
           "1/ipv4/0", "192.168.0.1";
      ];

      [ "attr/eth1/ipv6/2/addr", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "1/ipv6/2", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      (* multiple ip addrs *)
      [ "attr/vif/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
        "attr/vif/0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
      ], [ "0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "xenserver/attr/net-sriov-vf/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
        "xenserver/attr/net-sriov-vf/0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
      ], [ "0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "attr/vif/0/ipv4/0", "192.168.0.1";
        "attr/vif/0/ipv4/1", "192.168.1.1";
      ], [ "0/ipv4/1", "192.168.1.1";
           "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
      ];

      [ "attr/eth0/ip", "192.168.0.1";
        "attr/eth0/ipv6/0/addr", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "attr/vif/0/ipv4/0", "192.168.0.1";
        "attr/vif/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      [ "attr/eth0/ip", "192.168.0.1";
        "attr/vif/0/ipv4/0", "192.168.0.1";
        "attr/eth0/ipv6/0/addr", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
        "attr/vif/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ], [ "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
      ];

      (* multiple vifs and multiple ip addrs *)
      [ "attr/vif/0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
        "attr/vif/0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
        "attr/vif/1/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd16";
        "attr/vif/1/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd17";
      ], [ "0/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd07";
           "0/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd06";
           "1/ipv6/1", "fe80:0000:0000:0000:7870:94ff:fe52:dd17";
           "1/ipv6/0", "fe80:0000:0000:0000:7870:94ff:fe52:dd16";
      ];

      [ "attr/vif/0/ipv4/0", "192.168.0.1";
        "attr/vif/0/ipv4/1", "192.168.0.2";
        "attr/vif/1/ipv4/0", "192.168.1.1";
        "attr/vif/1/ipv4/1", "192.168.1.2";
      ], [ "0/ipv4/1", "192.168.0.2";
           "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
           "1/ipv4/1", "192.168.1.2";
           "1/ip", "192.168.1.1";
           "1/ipv4/0", "192.168.1.1";
      ];

      (* combined SRIOV VF and plain VIF *)
      [ "attr/vif/0/ipv4/0", "192.168.0.1";
        "attr/vif/0/ipv4/1", "192.168.0.2";
        "attr/vif/1/ipv4/0", "192.168.1.1";
        "attr/vif/1/ipv4/1", "192.168.1.2";
        "xenserver/attr/net-sriov-vf/2/ipv4/0", "192.168.2.1";
        "xenserver/attr/net-sriov-vf/2/ipv4/1", "192.168.2.2";
        "xenserver/attr/net-sriov-vf/3/ipv4/0", "192.168.3.1";
        "xenserver/attr/net-sriov-vf/3/ipv4/1", "192.168.3.2";
      ], [ "0/ipv4/1", "192.168.0.2";
           "0/ip", "192.168.0.1";
           "0/ipv4/0", "192.168.0.1";
           "1/ipv4/1", "192.168.1.2";
           "1/ip", "192.168.1.1";
           "1/ipv4/0", "192.168.1.1";
           "2/ipv4/1", "192.168.2.2";
           "2/ip", "192.168.2.1";
           "2/ipv4/0", "192.168.2.1";
           "3/ipv4/1", "192.168.3.2";
           "3/ip", "192.168.3.1";
           "3/ipv4/0", "192.168.3.1";
      ];

      (* exceptions *)
      [ "attr/vif/0/ipv4/a", "192.168.0.1";
        "attr/vif/0/ipv4/1", "192.168.0.1";
      ], [];

      [ "xenserver/attr/net-sriov-vf/0/ipv4/a", "192.168.0.1";
        "xenserver/attr/net-sriov-vf/0/ipv4/1", "192.168.0.1";
      ], [];
    ]
  end)

let test =
  "test_guest_agent" >:::
  [
    "test_networks" >::: Networks.tests;
    "test_get_initial_guest_metrics" >::: Initial_guest_metrics.tests;
  ]
