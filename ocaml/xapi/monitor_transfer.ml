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
(**
 * @group Performance Monitoring
 *)
 
open Monitor_types

let marshall_vifs l =
  let f x =
    match x with
	(uuid,vif) -> (*(i2,(s,i64_1, i64_2))) ->*)
	  XMLRPC.To.array
	    [
	      XMLRPC.To.string uuid;
	      XMLRPC.To.string (string_of_int vif.vif_n);
	      XMLRPC.To.string vif.vif_name;
	      XMLRPC.To.string (string_of_float vif.vif_tx);
	      XMLRPC.To.string (string_of_float vif.vif_rx)
	    ] in
    XMLRPC.To.array (List.map f l)

let unmarshall_vifs xml =
  let f xml =
    match XMLRPC.From.array (fun x->x) xml with
	[uuid;i2;s;i64_1;i64_2] ->
	  (XMLRPC.From.string uuid,
	   {vif_n=int_of_string (XMLRPC.From.string i2);
	    vif_name=(XMLRPC.From.string s);
	    vif_tx=float_of_string (XMLRPC.From.string i64_1);
	    vif_rx=float_of_string (XMLRPC.From.string i64_2);
	    vif_raw_tx=0L;
	    vif_raw_rx=0L;
	    vif_raw_tx_err=0L;
	    vif_raw_rx_err=0L})
		| _ -> failwith (Printf.sprintf "unmarshall_vifs unexpected XML: %s" (Xml.to_string xml))
 in
    List.map f (XMLRPC.From.array (fun x->x) xml)

let marshall_vbds l =
  let f x =
    match x with
	(uuid,vbd) ->
	  XMLRPC.To.array
	    [
	      XMLRPC.To.string uuid;
	      XMLRPC.To.string (string_of_int vbd.vbd_device_id);
	      XMLRPC.To.string (string_of_float vbd.vbd_io_read);
	      XMLRPC.To.string (string_of_float vbd.vbd_io_write);
	    ] in
    XMLRPC.To.array (List.map f l)

let unmarshall_vbds xml =
  let f xml =
    match XMLRPC.From.array (fun x->x) xml with
	[uuid;i2;i64_1;i64_2] ->
	  (XMLRPC.From.string uuid,
	   {vbd_device_id=int_of_string (XMLRPC.From.string i2);
	    vbd_io_read=float_of_string (XMLRPC.From.string i64_1);
	    vbd_io_write=float_of_string (XMLRPC.From.string i64_2);
	    vbd_raw_io_read=0L;
	    vbd_raw_io_write=0L;})
		| _ -> failwith (Printf.sprintf "unmarshall_vbds unexpected XML: %s" (Xml.to_string xml))
 in
    List.map f (XMLRPC.From.array (fun x->x) xml)

let marshall_float_array (a : float array) =
  let l = Array.to_list a in
    XMLRPC.To.array (List.map (fun x -> XMLRPC.To.string (string_of_float x)) l)

let unmarshall_float_array xml : float array =
  Array.of_list (XMLRPC.From.array (fun x -> float_of_string (XMLRPC.From.string x)) xml)

let marshall_pcpus pcpus =
  XMLRPC.To.array [ marshall_float_array pcpus.pcpus_usage ]

let unmarshall_pcpus xml =
    match XMLRPC.From.array (fun x->x) xml with
	[ia] -> {pcpus_usage=unmarshall_float_array ia}
 		| _ -> failwith (Printf.sprintf "unmarshall_pcpus unexpected XML: %s" (Xml.to_string xml))


let marshall_vcpus l =
  let f x =
    match x with
	(uuid, vcpus) ->
	  XMLRPC.To.array
	    [
	      XMLRPC.To.string uuid;
	      XMLRPC.To.string (string_of_float vcpus.vcpu_sumcpus);
	      marshall_float_array (vcpus.vcpu_vcpus)
	    ] in
    XMLRPC.To.array (List.map f l)

let unmarshall_vcpus xml =
  let f xml =
    match XMLRPC.From.array (fun x->x) xml with
	[uuid;i;ia] ->
	  (XMLRPC.From.string uuid,
	   {vcpu_sumcpus=float_of_string (XMLRPC.From.string i);
	    vcpu_vcpus=unmarshall_float_array ia;
	    vcpu_rawvcpus=[| |];
	    vcpu_cputime=0L;
	   })
		| _ -> failwith (Printf.sprintf "unmarshall_vcpus unexpected XML: %s" (Xml.to_string xml))
 in
    List.map f (XMLRPC.From.array (fun x->x) xml)

let marshall_memory l =
  let f x =
    match x with
	(uuid, mem) ->
	  XMLRPC.To.array
	    [
	      XMLRPC.To.string uuid;
	      XMLRPC.To.string (Int64.to_string mem.memory_mem)
	    ] in
    XMLRPC.To.array (List.map f l)

let unmarshall_memory xml =
  let f xml =
    match XMLRPC.From.array (fun x->x) xml with
	[uuid;i64] ->
	  (XMLRPC.From.string uuid,
	   {memory_mem=Int64.of_string (XMLRPC.From.string i64)})
		| _ -> failwith (Printf.sprintf "unmarshall_memory unexpected XML: %s" (Xml.to_string xml))
 in
    List.map f (XMLRPC.From.array (fun x->x) xml)

let marshall_pifs pifs =
	let f x = match x with
	| pif ->
		XMLRPC.To.array [
			XMLRPC.To.string pif.pif_name;
			XMLRPC.To.string (string_of_float pif.pif_tx);
			XMLRPC.To.string (string_of_float pif.pif_rx);
			XMLRPC.To.string (string_of_bool pif.pif_carrier);
			XMLRPC.To.string (string_of_int pif.pif_speed);
			XMLRPC.To.string (Network_interface.string_of_duplex pif.pif_duplex);
			XMLRPC.To.string pif.pif_pci_bus_path;
			XMLRPC.To.string pif.pif_vendor_id;
			XMLRPC.To.string pif.pif_device_id;
		] in
	XMLRPC.To.array (List.map f pifs)

let unmarshall_pifs xml =
	let f xml = match XMLRPC.From.array (fun x -> x) xml with
	| [ name; i64_1; i64_2; carrier; speed; duplex; pcibuspath; vendor; device ] ->
		{pif_name=XMLRPC.From.string name;
		 pif_tx=float_of_string (XMLRPC.From.string i64_1);
		 pif_rx=float_of_string (XMLRPC.From.string i64_2);
		 pif_raw_tx=0L;
		 pif_raw_rx=0L; (* Ignore these, for RRD only *)
		 pif_carrier=bool_of_string (XMLRPC.From.string carrier);
		 pif_speed=int_of_string (XMLRPC.From.string speed);
		 pif_duplex=Network_interface.duplex_of_string (XMLRPC.From.string duplex);
		 pif_pci_bus_path=XMLRPC.From.string pcibuspath;
		 pif_vendor_id=XMLRPC.From.string vendor;
		 pif_device_id=XMLRPC.From.string device}
		| _ -> failwith (Printf.sprintf "unmarshall_pifs unexpected XML: %s" (Xml.to_string xml))
 in
	List.map f (XMLRPC.From.array (fun x -> x) xml)

let marshall_uuids uuids =
  XMLRPC.To.array (List.map XMLRPC.To.string uuids)

let unmarshall_uuids xml =
  XMLRPC.From.array XMLRPC.From.string xml

let marshall_host_stats hs =
  XMLRPC.To.array
    [
      XMLRPC.To.string (Ref.string_of hs.host_ref);
      XMLRPC.To.string (Int64.to_string hs.total_kib);
      XMLRPC.To.string (Int64.to_string hs.free_kib);
      marshall_vifs hs.vifs;
      marshall_pifs hs.pifs;
      marshall_vbds hs.vbds;
      marshall_pcpus hs.pcpus;
      marshall_vcpus hs.vcpus;
      marshall_memory hs.mem;
      marshall_uuids hs.registered
    ]
    
let unmarshall_host_stats xml =
  match (XMLRPC.From.array (fun x->x) xml) with
      [href; i64_1; i64_2; vifs; pifs; vbds; pcpus; vcpus; mem; uuids] ->
	{timestamp=0.0;
	 host_ref=Ref.of_string (XMLRPC.From.string href);
	 total_kib=Int64.of_string (XMLRPC.From.string i64_1);
	 free_kib=Int64.of_string (XMLRPC.From.string i64_2);
	 vifs=unmarshall_vifs vifs;
	 pifs=unmarshall_pifs pifs;
	 vbds=unmarshall_vbds vbds;
	 pcpus=unmarshall_pcpus pcpus;
	 vcpus=unmarshall_vcpus vcpus;
	 mem=unmarshall_memory mem;
	 registered=unmarshall_uuids uuids}
	| [vifs; pifs; vbds; pcpus; vcpus; mem; hostmetrics; uuids] ->
		(* CA-18377: This case supports unmarshalling of data from a Miami host. *)
		begin
			match (XMLRPC.From.array (fun x->x) hostmetrics) with
				[href; i64_1; i64_2] ->
					{timestamp=0.0;
					 host_ref=Ref.of_string (XMLRPC.From.string href);
					 total_kib=Int64.of_string (XMLRPC.From.string i64_1);
					 free_kib=Int64.of_string (XMLRPC.From.string i64_2);
					 vifs=unmarshall_vifs vifs;
					 pifs=unmarshall_pifs pifs;
					 vbds=unmarshall_vbds vbds;
					 pcpus=unmarshall_pcpus pcpus;
					 vcpus=unmarshall_vcpus vcpus;
					 mem=unmarshall_memory mem;
					 registered=unmarshall_uuids uuids}
				| _ -> failwith (Printf.sprintf "unmarshall_host_stats unexpected XML: %s" (Xml.to_string xml))
		end
		| _ -> failwith (Printf.sprintf "unmarshall_host_stats unexpected XML: %s" (Xml.to_string xml))

let marshall hs = 
  marshall_host_stats hs

let unmarshall xml =
  unmarshall_host_stats xml
