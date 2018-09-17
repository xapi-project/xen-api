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

open Lwt


module M = struct

  let whoami () = Printf.sprintf "%s:%d"
      (Filename.basename Sys.argv.(0)) (Unix.getpid ())

  module IO = struct
    include Cohttp_lwt_unix.IO

    let map = Lwt.map
    let iter = Lwt_list.iter_s
    let any = Lwt.choose
    let is_determined t = Lwt.state t <> Lwt.Sleep
  end

  let connect path =
    let sockaddr = Lwt_unix.ADDR_UNIX path in
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    let rec connect' () =
      Lwt.catch (fun () ->
          Lwt_unix.connect fd sockaddr
        ) (function
          | Unix.Unix_error((Unix.ECONNREFUSED | Unix.ECONNABORTED | Unix.ENOENT), _, _) ->
            Lwt_unix.sleep 5. >>= fun () -> connect' ()
          | e ->
            Lwt_unix.close fd >>= fun () -> fail e
        )
    in
    connect' ()
    >>= fun () ->
    let ic = Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~close:(fun () -> return ()) ~mode:Lwt_io.output fd in
    return (ic, oc)

  let disconnect (ic, _oc) =
    Lwt_io.close ic

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


module Client = Message_switch_core.Make.Client(M)
module Server = Message_switch_core.Make.Server(M)
