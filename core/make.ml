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
open Sexplib.Std
open Protocol

module Connection = functor(IO: Cohttp.S.IO) -> struct
  open IO
  module Request = Cohttp.Request.Make(IO)
  module Response = Cohttp.Response.Make(IO)

  let rpc (ic, oc) frame =
    let b, meth, uri = In.to_request frame in
    let body = match b with None -> "" | Some x -> x in
    let headers = In.headers body in
    let req = Cohttp.Request.make ~meth ~headers uri in
    Request.write (fun writer -> match b with
        | Some body ->
          Request.write_body writer body
        | None -> return ()
      ) req oc >>= fun () ->

    Response.read ic >>= function
    | `Ok response ->
      if Cohttp.Response.status response <> `OK then begin
        Printf.fprintf stderr "Server sent: %s\n%!" (Cohttp.Code.string_of_status (Cohttp.Response.status response));
        (* Response.write (fun _ _ -> return ()) response Lwt_io.stderr >>= fun () -> *)
        return (`Error (`Message_switch `Unsuccessful_response))
      end else begin
        let reader = Response.make_body_reader response ic in
        let results = Buffer.create 128 in
        let rec read () =
          Response.read_body_chunk reader >>= function
          | Cohttp.Transfer.Final_chunk x ->
            Buffer.add_string results x;
            return (`Ok (Buffer.contents results))
          | Cohttp.Transfer.Chunk x ->
            Buffer.add_string results x;
            read ()
          | Cohttp.Transfer.Done ->
            return (`Ok (Buffer.contents results)) in
        read ()
      end
    | `Invalid s ->
      Printf.fprintf stderr "Invalid response: '%s'\n%!" s;
      return (`Error (`Message_switch `Failed_to_read_response))
    | `Eof ->
      Printf.fprintf stderr "Empty response\n%!";
      return (`Error (`Message_switch `Failed_to_read_response))
end

module Opt = struct
  let iter f = function
    | None -> ()
    | Some x -> f x
  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

module Client = functor(M: S.BACKEND) -> struct

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
    | `Timeout
    | `Queue_deleted of string
  ]

  type 'a result = ('a, [ `Message_switch of error]) Mresult.result

  let pp_error fmt = function
    | `Message_switch (`Msg x) -> Format.pp_print_string fmt x
    | `Message_switch `Failed_to_read_response ->
      Format.pp_print_string fmt "Failed to read response from the message-switch"
    | `Message_switch `Unsuccessful_response ->
      Format.pp_print_string fmt "Received an unexpected failure from the message-switch"
    | `Message_switch `Timeout ->
      Format.pp_print_string fmt "Timeout"
    | `Message_switch (`Queue_deleted name) ->
      Format.fprintf fmt "The queue %s has been deleted" name

  let error_to_msg = function
    | `Ok x -> `Ok x
    | `Error y ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      pp_error fmt y;
      Format.pp_print_flush fmt ();
      `Error (`Msg (Buffer.contents b))

  module Connection = Connection(M.IO)

  open M.IO

  type 'a io = 'a M.IO.t

  type t = {
    mutable requests_conn: (ic * oc);
    mutable events_conn: (ic * oc);
    requests_m: M.Mutex.t;
    wakener: (message_id, Message.t result M.Ivar.t) Hashtbl.t;
    reply_queue_name: string;
  }

  let ( >>|= ) m f = m >>= function
    | `Ok x -> f x
    | `Error y -> return (`Error y)

  let rec iter_s f = function
    | [] -> return (`Ok ())
    | x :: xs ->
      f x >>|= fun () ->
      iter_s f xs

  let connect ~switch:port () =
    let token = M.whoami () in
    let rec reconnect () =
      M.connect port >>= fun requests_conn ->
      Connection.rpc requests_conn (In.Login token) >>|= fun (_: string) ->
      M.connect port >>= fun events_conn ->
      Connection.rpc events_conn (In.Login token) >>|= fun (_: string) ->
      return (`Ok (requests_conn, events_conn)) in

    reconnect () >>|= fun (requests_conn, events_conn) ->

    let wakener = Hashtbl.create 10 in
    let requests_m = M.Mutex.create () in

    Connection.rpc requests_conn (In.CreateTransient token) >>|= fun reply_queue_name ->

    let t = { requests_conn; events_conn; requests_m; wakener; reply_queue_name } in

    let (_ : unit result M.IO.t) =
      let rec loop from =
        let transfer = {
          In.from = from;
          timeout = timeout;
          queues = [ reply_queue_name ]
        } in
        let frame = In.Transfer transfer in
        Connection.rpc events_conn frame >>= function
        | `Error _ ->
          M.Mutex.with_lock requests_m (fun () ->
            reconnect ()
            >>|= fun (requests_conn, events_conn) ->
            t.requests_conn <- requests_conn;
            t.events_conn <- events_conn;
            return (`Ok ())
          ) >>|= fun () ->
          loop from
        | `Ok raw ->
        let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
        match transfer.Out.messages with
        | [] -> loop from
        | m :: ms ->
          iter_s
            (fun (i, m) ->
               M.Mutex.with_lock requests_m (fun () ->
                   match m.Message.kind with
                   | Message.Response j ->
                     if Hashtbl.mem wakener j then begin
                       let rec loop events_conn =
                         Connection.rpc events_conn (In.Ack i) >>= function
                         | `Ok (_: string) ->
                           M.Ivar.fill (Hashtbl.find wakener j) (`Ok m);
                           return (`Ok ())
                         | `Error _ ->
                           reconnect ()
                           >>|= fun (requests_conn, events_conn) ->
                           t.requests_conn <- requests_conn;
                           t.events_conn <- events_conn;
                           loop events_conn in
                       loop events_conn
                     end else begin
                       Printf.printf "no wakener for id %s, %Ld\n%!" (fst i) (snd i);
                       Hashtbl.iter (fun k v ->
                         Printf.printf "  have wakener id %s, %Ld\n%!" (fst k) (snd k)
                       ) wakener;
                       return (`Ok ())
                     end
                   | Message.Request _ -> return (`Ok ())
                 )
            ) transfer.Out.messages >>|= fun () ->
          loop (Some transfer.Out.next) in
      loop None in
    return (`Ok t)

  let disconnect ~t () =
    M.disconnect t.requests_conn >>= fun () ->
    M.disconnect t.events_conn

  let rpc ~t ~queue ?timeout ~body:x () =
    let ivar = M.Ivar.create () in

    let timer = Opt.map (fun timeout ->
        M.Clock.run_after timeout (fun () -> M.Ivar.fill ivar (`Error (`Message_switch `Timeout)))
      ) timeout in

    let msg = In.Send(queue, {
        Message.payload = x;
        kind = Message.Request t.reply_queue_name
      }) in
    let rec loop () =
      M.Mutex.with_lock t.requests_m
        (fun () ->
           Connection.rpc t.requests_conn (In.CreatePersistent queue)
           >>|= fun (_: string) ->
           Connection.rpc t.requests_conn msg >>= function
           | `Error e -> return (`Error e)
           | `Ok id ->
             begin match message_id_opt_of_rpc (Jsonrpc.of_string id) with
             | None ->
               return (`Error (`Message_switch (`Queue_deleted queue)))
             | Some mid ->
               Hashtbl.add t.wakener mid ivar;
               return (`Ok mid)
             end
        ) >>= function
        | `Ok mid -> return (`Ok mid)
        | `Error (`Message_switch (`Queue_deleted name)) -> return (`Error (`Message_switch (`Queue_deleted name)))
        | `Error _ ->
          (* we expect the event thread to reconnect for us *)
          let ivar' = M.Ivar.create () in
          (* XXX: we don't respect the timeout value here *)
          let (_: M.Clock.timer) = M.Clock.run_after 5 (fun () -> M.Ivar.fill ivar' ()) in
          M.Ivar.read ivar' >>= fun () ->
          loop () in
    loop () >>|= fun mid ->
    M.Ivar.read ivar >>|= fun x ->
    Hashtbl.remove t.wakener mid;
    Opt.iter M.Clock.cancel timer;
    return (`Ok x.Message.payload)

  let list ~t ~prefix ?(filter=`All) () =
    Connection.rpc t.requests_conn (In.List (prefix, filter)) >>|= fun result ->
    return (`Ok (Out.string_list_of_rpc (Jsonrpc.of_string result)))

  let diagnostics ~t () =
    Connection.rpc t.requests_conn In.Diagnostics >>|= fun result ->
    return (`Ok (Diagnostics.t_of_rpc (Jsonrpc.of_string result)))

  let trace ~t ?(from=0L) ?(timeout=0.) () =
    Connection.rpc t.requests_conn (In.Trace(from, timeout)) >>|= fun result ->
    return (`Ok (Out.trace_of_rpc (Jsonrpc.of_string result)))

  let ack ~t ~message:(name, id) () =
    Connection.rpc t.requests_conn (In.Ack (name, id)) >>|= fun _ ->
    return (`Ok ())

  let destroy ~t ~queue:queue_name () =
    Connection.rpc t.requests_conn (In.Destroy queue_name) >>|= fun result ->
    return (`Ok ())

  let shutdown ~t () =
    Connection.rpc t.requests_conn In.Shutdown >>|= fun result ->
    return (`Ok ())
end


module Server = functor(M: S.BACKEND) -> struct

  module Connection = Connection(M.IO)

  open M.IO

  type 'a io = 'a M.IO.t

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
  ]

  type 'a result = ('a, [ `Message_switch of error]) Mresult.result

  let pp_error fmt = function
    | `Message_switch (`Msg x) -> Format.pp_print_string fmt x
    | `Message_switch `Failed_to_read_response ->
      Format.pp_print_string fmt "Failed to read response from the message-switch"
    | `Message_switch `Unsuccessful_response ->
      Format.pp_print_string fmt "Received an unexpected failure from the message-switch"

  let error_to_msg = function
    | `Ok x -> `Ok x
    | `Error y ->
      let b = Buffer.create 16 in
      let fmt = Format.formatter_of_buffer b in
      pp_error fmt y;
      Format.pp_print_flush fmt ();
      `Error (`Msg (Buffer.contents b))

  type t = {
    request_shutdown: unit M.Ivar.t;
    on_shutdown: unit M.Ivar.t;
  }

  let shutdown ~t () =
    M.Ivar.fill t.request_shutdown ();
    M.Ivar.read t.on_shutdown

  let ( >>|= ) m f = m >>= function
    | `Ok x -> f x
    | `Error y -> return (`Error y)

  let listen ~process ~switch:port ~queue:name () =
    let token = Printf.sprintf "%d" (Unix.getpid ()) in
    M.connect port >>= fun c ->
    Connection.rpc c (In.Login token) >>|= fun (_: string) ->
    Connection.rpc c (In.CreatePersistent name) >>|= fun _ ->

    let request_shutdown = M.Ivar.create () in
    let on_shutdown = M.Ivar.create () in
    let t = { request_shutdown; on_shutdown } in

    let rec loop c from =
      let transfer = {
        In.from = from;
        timeout = timeout;
        queues = [ name ];
      } in
      let frame = In.Transfer transfer in
      let message = Connection.rpc c frame in
      any [ map (fun _ -> ()) message; M.Ivar.read request_shutdown ] >>= fun () ->
      if is_determined (M.Ivar.read request_shutdown) then begin
        M.Ivar.fill on_shutdown ();
        return (`Ok ())
      end else begin
        message >>= function
        | `Error e ->
          M.connect port >>= fun c ->
          Connection.rpc c (In.Login token) >>|= fun (_: string) ->
          loop c from
        | `Ok raw ->
          let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
          begin match transfer.Out.messages with
            | [] -> loop c from
            | m :: ms ->
              iter
                (fun (i, m) ->
                   process m.Message.payload >>= fun response ->
                   begin
                     match m.Message.kind with
                     | Message.Response _ ->
                       return () (* configuration error *)
                     | Message.Request reply_to ->
                       let request = In.Send(reply_to, { Message.kind = Message.Response i; payload = response }) in
                       Connection.rpc c request >>= fun _ ->
                       return ()
                   end >>= fun () ->
                   let request = In.Ack i in
                   Connection.rpc c request >>= fun _ ->
                   return ()
                ) transfer.Out.messages >>= fun () ->
              loop c (Some transfer.Out.next)
          end
      end in
    let _ = loop c None in
    return (`Ok t)
end

(* The following type is deprecated, used only by legacy Unix clients *)
type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b
