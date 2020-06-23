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

(** important invariants:
  * given a list of hosts containing both the master and
    the members, the master will always be at the head *)
module D = Debug.Make (struct
  let name = "xapi_psr"
end)

type failure =
  | Failed_during_accept_new_pool_secret
  | Failed_during_send_new_pool_secret
  | Failed_during_cleanup

type 'a r = (unit, failure * 'a) result

module type Impl = sig
  type pool_secret

  type pool_secrets = pool_secret * pool_secret (* (old, new) *)

  type host

  val save_checkpoint : string -> unit

  val retrieve_checkpoint : unit -> string option

  val backup : pool_secrets -> unit

  val retrieve : unit -> pool_secrets

  val tell_accept_new_pool_secret : pool_secrets -> host -> unit

  val tell_send_new_pool_secret : pool_secrets -> host -> unit

  val tell_cleanup_old_pool_secret : host -> unit

  val cleanup_master : unit -> unit
end

module Make =
functor
  (Impl : Impl)
  ->
  struct
    let ( >>= ) = Rresult.( >>= )

    type checkpoint =
      | No_checkpoint (* ==> previous rotation was successful*)
      | Accept_new_pool_secret
      | Send_new_pool_secret
      | Cleanup_members
      | Cleanup_master
    [@@deriving rpcty]

    exception Cannot_parse_checkpoint of string

    let string_of_checkpoint x =
      Rpcmarshal.marshal checkpoint.Rpc.Types.ty x |> Jsonrpc.to_string
      |> (* remove leading and trailing '"' *)
      fun s -> String.sub s 1 (String.length s - 2)

    let checkpoint_of_string s =
      let rpc = Rpc.rpc_of_string s in
      match Rpcmarshal.unmarshal checkpoint.Rpc.Types.ty rpc with
      | Ok x ->
          x
      | Error _ ->
          raise (Cannot_parse_checkpoint s)

    let rec iter_break f = function
      | [] ->
          Ok ()
      | x :: xs ->
          f x >>= fun _ -> (iter_break [@tailcall]) f xs

    let rec go pool_secrets master members = function
      | No_checkpoint ->
          (* if we fail to backup the pool secrets, it doesn't really matter,
             you can simply restart the rotation *)
          Impl.backup pool_secrets ;
          (go [@tailcall]) pool_secrets master members Accept_new_pool_secret
      | Accept_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Accept_new_pool_secret) ;
          master :: members
          |> iter_break (fun host ->
                 try
                   Impl.tell_accept_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling host to accept new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_accept_new_pool_secret, host))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Send_new_pool_secret
      | Send_new_pool_secret ->
          Impl.save_checkpoint (string_of_checkpoint Send_new_pool_secret) ;
          master :: members
          |> iter_break (fun host ->
                 try
                   Impl.tell_send_new_pool_secret pool_secrets host ;
                   Ok ()
                 with e ->
                   D.error
                     "failed while telling hosts to send new pool secret. \
                      error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_send_new_pool_secret, host))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Cleanup_members
      | Cleanup_members ->
          Impl.save_checkpoint (string_of_checkpoint Cleanup_members) ;
          members
          |> iter_break (fun member ->
                 try
                   Impl.tell_cleanup_old_pool_secret member ;
                   Ok ()
                 with e ->
                   D.error "failed while telling hosts to cleanup. error= %s"
                     (Printexc.to_string e) ;
                   Error (Failed_during_cleanup, member))
          >>= fun () ->
          (go [@tailcall]) pool_secrets master members Cleanup_master
      | Cleanup_master -> (
          Impl.save_checkpoint (string_of_checkpoint Cleanup_master) ;
          try Impl.cleanup_master () ; Ok ()
          with e ->
            D.error "failed to cleanup the master. error= %s"
              (Printexc.to_string e) ;
            Error (Failed_during_cleanup, master)
        )

    let start pool_secrets ~master ~members =
      match Impl.retrieve_checkpoint () |> Option.map checkpoint_of_string with
      | None | Some No_checkpoint ->
          go pool_secrets master members No_checkpoint
      | Some checkpoint ->
          go (Impl.retrieve ()) master members checkpoint
  end

let start ~__context = failwith "not impl"
