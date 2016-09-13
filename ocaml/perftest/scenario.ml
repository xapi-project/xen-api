(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(* VMs *)
type vm =
  { vbds: int;
    vifs: int;
    tag: string;
    num: int;
    has_affinity: bool;
  }

let default_vm num =
  { vbds = 1;
    vifs = 4;
    tag = "everything";
    num = num;
    has_affinity = true
  }

let string_of_vm (x: vm) =
  let vbds = Printf.sprintf "%s VBDs" (if x.vbds = 0 then "no" else string_of_int x.vbds) in
  let vifs = Printf.sprintf "%s VIFs" (if x.vifs = 0 then "no" else string_of_int x.vifs) in
  Printf.sprintf "%d VMs per host (tag %s) with %s, %s and affinity%s set" x.num x.tag vbds vifs (if x.has_affinity then "" else " not")


(* Pools *)
type pool =
  { id: string;
    sdk: bool;
    hosts: int;
    interfaces_per_host: int;
    vms: vm list;
    bonds: int; (* Needs to be less than or equal to interfaces_per_host / 2 *)
    key: string;
    ipbase: int;
    iscsi_luns: int;
    use_shared_storage: bool;
  }

let default =
  { id="default";
    sdk=true;
    hosts=1;
    interfaces_per_host=6;
    vms =
      [ (default_vm 20);
        { (default_vm 20) with vifs = 0; tag = "novifs" };
        { (default_vm 20) with vbds = 0; tag = "novbds" };
        { (default_vm 20) with vifs = 0; vbds = 0; tag = "novbdsnovifs" }
      ];
    bonds=2;
    key="";
    ipbase=0;
    iscsi_luns=1;
    use_shared_storage=false;
  }

let description_of_pool (x: pool) =
  [ Printf.sprintf "Scenario: %s" x.id;
    Printf.sprintf "Key: %s" x.key;
    Printf.sprintf "%d hosts, each with %d network interfaces, %d of which are paired into %d bonds"
      x.hosts x.interfaces_per_host (x.bonds * 2) x.bonds;
  ] @ (List.map string_of_vm x.vms)

let pools =
  [ { default with id="pool0"; hosts=1 };
    { default with id="pool1"; hosts=4 };
    { default with id="pool2"; hosts=16};
    { default with id="pool3"; hosts=48};
    { default with
      id="real1";
      hosts=1;
      sdk=false;
      bonds=0;
      interfaces_per_host=0;
      vms = [ { (default_vm 50) with tag = "" } ]};
    { default with
      id="xendesktop";
      hosts=8;
      vms = [ { (default_vm 50) with vbds = 0; vifs = 1; tag = "xendesktop"; has_affinity = false } ]};
    { default with
      id="empty";
      hosts=1; (* we won't be starting VMs in the clone test so we don't need any hosts *)
      vms = [ { (default_vm 1) with tag = "winxp-gold"; vifs = 1; vbds = 1 } ]; (* 1 per host *)
      iscsi_luns=6;
      use_shared_storage=true;}
  ]

let get_all () = List.map (fun p -> p.id) pools
let get name = List.find (fun p -> p.id=name) pools

let xml_of_scenario s =
  String.concat "\n"
    (["<scenario>";
      (Printf.sprintf " <id>%s</id>" s.id);
      (Printf.sprintf " <key>%s</key>" s.key);
      (Printf.sprintf " <sdk>%b</sdk>" s.sdk);
      (Printf.sprintf " <hosts>%d</hosts>" s.hosts);
      (Printf.sprintf " <interfaces_per_host>%d</interfaces_per_host>" s.interfaces_per_host);
      (Printf.sprintf " <vms>")]
     @
     (List.map (fun vm -> Printf.sprintf "  <vm vbds=\"%d\" vifs=\"%d\" tag=\"%s\" num=\"%d\" has_affinity=\"%b\" />" vm.vbds vm.vifs vm.tag vm.num vm.has_affinity) s.vms)
     @
     [" </vms>";
      Printf.sprintf " <bonds>%d</bonds>" s.bonds;
      Printf.sprintf " <ipbase>%d</ipbase>" s.ipbase;
      "</scenario>"
     ])


let oc_key = "perftestsetup"
let sr_disk_size = Int64.mul 1048576L 2093049L (* limit of 1 vhd ~2 terabytes (megs, gigs, t.. what?) *)
let sr_disk_device = "xvde"

