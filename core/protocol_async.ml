(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms,
with or without modification, are permitted provided
that the following conditions are met:

*   Redistributions of source code must retain the above
    copyright notice, this list of conditions and the
    following disclaimer.
*   Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the
    following disclaimer in the documentation and/or other
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
*)


  let whoami () = Printf.sprintf "%s:%d"
    (Filename.basename Sys.argv.(0)) (Unix.getpid ())

open Core.Std
open Async.Std

open Protocol
open Cohttp
open Cohttp_async


module M = struct

  let whoami = whoami

  module IO = struct
    include Cohttp_async_io
  end

  let connect port =
    let maximum_delay = 30. in
    let connect () =
      Tcp.connect (Tcp.to_host_and_port "127.0.0.1" port) in
    let rec retry delay =
      Monitor.try_with connect >>= function
      | Error (Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ECONNABORTED), _, _))->
        let delay = min maximum_delay delay in
        Clock.after (Time.Span.of_sec delay) >>= fun () ->
        retry (delay +. delay)
      | Error e -> raise e
      | Ok (_, reader, writer) ->
        return (reader, writer) in
    retry 1.

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

module Connection = Protocol.Connection(M.IO)

module Client = Protocol.Client(M)
module Server = Protocol.Server(M.IO)
