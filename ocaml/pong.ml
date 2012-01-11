(*
 * pong.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of obus, an ocaml implementation of D-Bus.
 *)

(* Very simple service with one object have a ping method *)

open Lwt
open Lwt_io

let ping obj msg =
  lwt () = printlf "received: %s" msg in
  return msg

let interface =
  Ping_pong.Org_foo_bar.make {
    Ping_pong.Org_foo_bar.m_Ping = (fun obj msg -> ping (OBus_object.get obj) msg);
  }

lwt () =
  lwt bus = OBus_bus.session () in

  (* Request a name *)
  lwt _ = OBus_bus.request_name bus "org.plop" in

  (* Create the object *)
  let obj = OBus_object.make ~interfaces:[interface] ["plip"] in
  OBus_object.attach obj ();

  (* Export the object on the connection *)
  OBus_object.export bus obj;

  (* Wait forever *)
  fst (wait ())
