(*
 * Copyright (c) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let whoami () = Printf.sprintf "%s:%d"
    (Filename.basename Sys.argv.(0)) (Unix.getpid ())

open Core.Std
open Async.Std
open Message_switch_core.Protocol
open Cohttp
open Cohttp_async


module M = struct

  let whoami = whoami

  module IO = struct
    include Cohttp_async.Io

    let map f t = Deferred.map ~f t
    let iter f t = Deferred.List.iter t ~f
    let any = Deferred.any
    let is_determined = Deferred.is_determined
  end

  let connect path =
    let maximum_delay = 30. in
    let connect () =
      let s = Socket.create Socket.Type.unix in
      Monitor.try_with ~extract_exn:true (fun () -> Socket.connect s (Socket.Address.Unix.create path)) >>= function
      | Ok x ->
        let fd = Socket.fd s in
        let reader = Reader.create fd in
        let writer = Writer.create fd in
        return (fd, reader, writer)
      | Error e ->
        Socket.shutdown s `Both;
        raise e in
    let rec retry delay =
      Monitor.try_with ~extract_exn:true connect >>= 	function
      | Error (Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ECONNABORTED | Unix.ENOENT), _, _))->
        let delay = min maximum_delay delay in
        Clock.after (Time.Span.of_sec delay) >>= fun () ->
        retry (delay +. delay)
      | Error e -> raise e
      | Ok (_, reader, writer) ->
        return (reader, writer) in
    retry 1.

  let disconnect (_, writer) =
    Writer.close writer

  module Ivar = struct
    include Ivar
  end
  module Mutex = struct
    type t = {
      mutable m: bool;
      c: unit Condition.t;
    }
    let create () =
      let m = false in
      let c = Condition.create () in
      { m; c }
    let with_lock t f =
      let rec wait state =
        if t.m = state
        then return ()
        else
          Condition.wait t.c >>= fun () ->
          wait state in
      wait false >>= fun () ->
      t.m <- true;
      Monitor.protect f
        ~finally:(fun () ->
            t.m <- false;
            Condition.broadcast t.c ();
            return ()
          )
  end
  module Clock = struct
    type timer = {
      cancel: unit Ivar.t;
    }
    let run_after timeout f =
      let timer = { cancel = Ivar.create () } in
      let cancelled = Ivar.read timer.cancel in
      let sleep = Clock.after (Time.Span.of_sec (Float.of_int timeout)) in
      let _ =
        Deferred.any [ cancelled; sleep ] >>= fun () ->
        if Deferred.is_determined cancelled
        then return ()
        else return (f ()) in
      timer

    let cancel t = Ivar.fill t.cancel ()
  end
end

let whoami = M.whoami

module Client = Message_switch_core.Make.Client(M)
module Server = Message_switch_core.Make.Server(M)
