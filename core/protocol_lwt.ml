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

open Protocol
open Lwt
open Cohttp
open Cohttp_lwt_unix


module M = struct

  let whoami () = Printf.sprintf "%s:%d"
    (Filename.basename Sys.argv.(0)) (Unix.getpid ())

  module IO = struct
    include Cohttp_lwt_unix_io
  end

	let connect port =
		let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1", port) in
		let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
		let result = ref None in
		lwt () = while_lwt !result = None do
			try_lwt
				lwt () = Lwt_unix.connect fd sockaddr in
				let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.input fd in
				let oc = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.output fd in
				result := Some (ic, oc);
				return ()
			with Unix.Unix_error((Unix.ECONNREFUSED | Unix.ECONNABORTED), _, _) ->
				lwt () = Lwt_unix.sleep 5. in
				return ()
		done in
		match !result with
		| None -> assert false
		| Some x -> return x

  module Ivar = struct
    type 'a t = {
      t: 'a Lwt.t;
      u: 'a Lwt.u;
    }
    let create () =
      let t, u = Lwt.task () in
      { t; u }
    let fill t x = Lwt.wakeup_later t.u x
    let read t = t.t
  end
  module Mutex = struct
    type t = Lwt_mutex.t

    let create = Lwt_mutex.create

    let with_lock = Lwt_mutex.with_lock
  end
  module Clock = struct
    type timer = unit Lwt.t

    let run_after timeout f =
      let t =
        Lwt_unix.sleep (float_of_int timeout) >>= fun () ->
        f ();
        return () in
      t
    let cancel = Lwt.cancel
  end
end

let whoami = M.whoami

module Connection = Protocol.Connection(M.IO)

module Client = Protocol.Client(M)
module Server = Protocol.Server(M.IO)
