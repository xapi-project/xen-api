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

let whoami () =
  Printf.sprintf "%s:%d" (Filename.basename Sys.argv.(0)) (Unix.getpid ())

open Core
open Async

module M = struct
  let whoami = whoami

  module IO = struct
    include Cohttp_async.Io

    let map f t = Deferred.map ~f t

    let iter f t = Deferred.List.iter t ~f

    let iter_dontwait f t =
      Deferred.don't_wait_for @@ Deferred.List.iter ~how:`Parallel t ~f

    let any = Deferred.any

    let all = Deferred.all

    let is_determined = Deferred.is_determined

    let return_unit = Deferred.unit
  end

  let connect path =
    let maximum_delay = 30. in
    let connect () =
      let s = Socket.create Socket.Type.unix in
      Monitor.try_with ~extract_exn:true (fun () ->
          Socket.connect s (Socket.Address.Unix.create path)
      )
      >>= function
      | Ok _x ->
          let fd = Socket.fd s in
          let reader = Reader.create fd in
          let writer = Writer.create fd in
          return (fd, reader, writer)
      | Error e ->
          Socket.shutdown s `Both ; raise e
    in
    let rec retry delay =
      Monitor.try_with ~extract_exn:true connect >>= function
      | Error
          (Unix.Unix_error
            (Core_unix.(ECONNREFUSED | ECONNABORTED | ENOENT), _, _)
            ) ->
          let delay = Float.min maximum_delay delay in
          Clock.after (Time.Span.of_sec delay) >>= fun () ->
          retry (delay +. delay)
      | Error e ->
          raise e
      | Ok (_, reader, writer) ->
          return (reader, writer)
    in
    retry 0.5

  let disconnect (_, writer) = Writer.close writer

  module Ivar = struct include Ivar end

  module Mutex = struct
    type t = {mutable m: bool; c: unit Condition.t}

    let create () =
      let m = false in
      let c = Condition.create () in
      {m; c}

    let with_lock t f =
      let rec wait () =
        if Bool.(t.m = false) then (
          t.m <- true ;
          return ()
        ) else
          Condition.wait t.c >>= wait
      in
      wait () >>= fun () ->
      Monitor.protect f ~finally:(fun () ->
          t.m <- false ;
          Condition.broadcast t.c () ;
          return ()
      )
  end

  module Condition = struct
    open Async_kernel

    type 'a t = 'a Condition.t

    let create = Condition.create

    let wait = Condition.wait

    let broadcast = Condition.broadcast

    let signal = Condition.signal
  end

  module Clock = struct
    type timer = {cancel: unit Ivar.t}

    let run_after timeout f =
      let timer = {cancel= Ivar.create ()} in
      let cancelled = Ivar.read timer.cancel in
      let sleep = Clock.after (Time.Span.of_sec (Float.of_int timeout)) in
      let _ =
        Deferred.any [cancelled; sleep] >>= fun () ->
        if Deferred.is_determined cancelled then
          return ()
        else
          return (f ())
      in
      timer

    let cancel t = Ivar.fill t.cancel ()
  end
end

module Client = Message_switch_core.Make.Client (M)
module Server = Message_switch_core.Make.Server (M)
module Mtest = Message_switch_core.Mtest.Make (M)
