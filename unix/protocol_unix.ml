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

open Message_switch_core.Protocol


let with_lock m f =
  Mutex.lock m;
  try
    let r = f () in
    Mutex.unlock m;
    r
  with e ->
    Mutex.unlock m;
    raise e

module IO = struct

  let whoami () = Printf.sprintf "%s:%d"
      (Filename.basename Sys.argv.(0)) (Unix.getpid ())

  module IO = struct
    type 'a t = 'a
    let ( >>= ) a f = f a

    let return a = a

    type ic = in_channel
    type oc = out_channel
    type conn = unit

    let read_line ic =
      try
        let line = input_line ic in
        let last = String.length line - 1 in
        let line = if line.[last] = '\r' then String.sub line 0 last else line in
        Some line
      with _ -> None

    let read_into_exactly ic buf ofs len =
      try
        really_input ic buf ofs len; true
      with _ -> false
    let _read_exactly ic len =
      let buf = Bytes.create len in
      read_into_exactly ic buf 0 len >>= function
      | true -> return (Some buf)
      | false -> return None

    let read ic n =
      let buf = Bytes.make n '\000' in
      let actually_read = input ic buf 0 n in
      if actually_read = n
      then Bytes.unsafe_to_string buf
      else Bytes.sub_string buf 0 actually_read

    let write oc x =
      output_string oc x; flush oc

    let connect path =
      let sockaddr = Unix.ADDR_UNIX(path) in
      let result = ref None in
      while !result = None do
        (* The system may invalidate the socket after a connect failure, therefore
           we must create a fresh fd for every iteration of the loop *)
        let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        try
          let () = Unix.connect fd sockaddr in
          let ic = Unix.in_channel_of_descr fd in
          let oc = Unix.out_channel_of_descr fd in
          result := Some (ic, oc)
        with Unix.Unix_error((Unix.ECONNREFUSED | Unix.ENOENT), _cmd, _) ->
          Unix.close fd;
          (* wait for the server to start *)
          Thread.delay 5.
           | e ->
             Unix.close fd;
             raise e
      done;
      match !result with
      | None -> assert false
      | Some x -> x

    let disconnect (ic, oc) =
      close_in ic;
      close_out oc

    let flush _oc = ()
  end
  include IO

  module Ivar = struct
    type 'a t = {
      mutable v: 'a option;
      m: Mutex.t;
      c: Condition.t;
    }

    let create () = {
      v = None;
      m = Mutex.create ();
      c = Condition.create ();
    }

    let fill r x =
      with_lock r.m
        (fun () ->
           r.v <- Some x;
           Condition.signal r.c
        )

    let read r =
      with_lock r.m
        (fun () ->
           while r.v = None do
             Condition.wait r.c r.m
           done;
           match r.v with
           | Some x -> x
           | None -> assert false
        )
  end

  module Mutex = struct
    type t = Mutex.t

    let create = Mutex.create

    let with_lock = with_lock
  end

  module Clock = struct
    type _timer = Protocol_unix_scheduler.t

    let started = ref false
    let started_m = Mutex.create ()

    let run_after timeout f =
      with_lock started_m
        (fun () ->
           if not !started then begin
             Protocol_unix_scheduler.start ();
             started := true
           end
        );
      Protocol_unix_scheduler.(one_shot (Delta timeout) "rpc" f)

    let cancel = Protocol_unix_scheduler.cancel
  end
end


module Connection = Message_switch_core.Make.Connection(IO)

module Opt = struct
  let iter f = function
    | None -> ()
    | Some x -> f x
  let map f = function
    | None -> None
    | Some x -> Some (f x)
end

let (>>|=) m f = match m with
  | `Error e -> `Error e
  | `Ok x -> f x

module Client = struct

  type 'a io = 'a

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
    | `Timeout
    | `Queue_deleted of string
  ]

  let (>>|=) m f = match m with
    | `Ok x -> f x
    | `Error (`Message_switch `Failed_to_read_response) -> `Error (`Message_switch `Failed_to_read_response)
    | `Error (`Message_switch `Unsuccessful_response) -> `Error (`Message_switch `Unsuccessful_response)
    | `Error (`Message_switch `Timeout) -> `Error (`Message_switch `Timeout)
    | `Error (`Message_switch (`Queue_deleted name)) -> `Error (`Message_switch (`Queue_deleted name))

  type 'a result = ('a, [ `Message_switch of error]) Message_switch_core.Mresult.result

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

  type t = {
    mutable requests_conn: (IO.ic * IO.oc);
    mutable events_conn: (IO.ic * IO.oc);
    requests_m: IO.Mutex.t;
    wakener: (Message_switch_core.Protocol.message_id, Message_switch_core.Protocol.Message.t result IO.Ivar.t) Hashtbl.t;
    reply_queue_name: string;
  }

  let disconnect ~t () =
    IO.disconnect t.requests_conn;
    IO.disconnect t.events_conn

  let connect switch =
    let token = IO.whoami () in
    let reconnect () =
      let requests_conn = IO.connect switch in
      Connection.rpc requests_conn (In.Login token)
      >>|= fun (_: string) ->
      let events_conn = IO.connect switch in
      Connection.rpc events_conn (In.Login token)
      >>|= fun (_: string) ->
      `Ok (requests_conn, events_conn) in

    reconnect ()
    >>|= fun (requests_conn, events_conn) ->
    let wakener = Hashtbl.create 10 in
    Connection.rpc requests_conn (In.CreateTransient token)
    >>|= fun reply_queue_name ->
    let requests_m = IO.Mutex.create () in
    let t = { requests_conn; events_conn; requests_m; wakener; reply_queue_name } in

    let (_ : Thread.t) =
      let rec loop from =
        let timeout = 30. in
        let transfer = {
          In.from = from;
          timeout = timeout;
          queues = [ reply_queue_name ]
        } in
        match (
          let frame = In.Transfer transfer in
          Connection.rpc t.events_conn frame
          >>|= fun raw ->
          (try `Ok (Out.transfer_of_rpc (Jsonrpc.of_string raw))
           with _e -> `Error (`Message_switch `Failed_to_read_response))
          >>|= fun transfer ->
          match transfer.Out.messages with
          | [] -> `Ok from
          | _ :: _ ->
            begin match List.fold_left (fun acc (i, m) -> match acc, i, m with
              | `Error e, _, _ -> `Error e
              | `Ok (), i, m ->
                 (* If the Ack doesn't belong to us then assume it's another thread *)
                 IO.Mutex.with_lock requests_m (fun () ->
                     match m.Message.kind with
                     | Message.Response j ->
                       if Hashtbl.mem wakener j then begin
                         Connection.rpc t.events_conn (In.Ack i)
                         >>|= fun (_: string) ->
                         IO.Ivar.fill (Hashtbl.find wakener j) (`Ok m);
                         `Ok ()
                       end else begin
                         Printf.printf "no wakener for id %s,%Ld\n%!" (fst i) (snd i);
                         `Ok ()
                       end
                     | Message.Request _ -> `Ok ()
                   )
              ) (`Ok ()) transfer.Out.messages with
              | `Ok () -> `Ok (Some transfer.Out.next)
              | `Error _ -> `Ok from (* repeat *)
              end
        ) with
        | `Ok from ->
          loop from
        | `Error _ ->
          reconnect ()
          >>|= fun (requests_conn, events_conn) ->
          t.requests_conn <- requests_conn;
          t.events_conn <- events_conn;
          loop from in
      Thread.create loop None in
    `Ok t

  (* Maintain at most one connection per process *)
  let connect =
    let c = ref None in
    let m = Mutex.create () in
    fun ~switch () ->
      IO.Mutex.with_lock m (fun () ->
          match !c with
          | Some x -> `Ok x
          | None ->
            connect switch
            >>|= fun c' ->
            c := Some c';
            `Ok c'
        )

  let rpc ~t:c ~queue:dest_queue_name ?timeout ~body:x () =
    let t = IO.Ivar.create () in
    let timer = Opt.map (fun timeout ->
        IO.Clock.run_after timeout (fun () -> IO.Ivar.fill t (`Error (`Message_switch `Timeout)))
      ) timeout in

    let rec loop () =
      match with_lock c.requests_m
          (fun () ->
             Connection.rpc c.requests_conn (In.CreatePersistent dest_queue_name)
             >>|= fun (_: string) ->
             let msg = In.Send(dest_queue_name, {
                 Message.payload = x;
                 kind = Message.Request c.reply_queue_name
               }) in
             Connection.rpc c.requests_conn msg
             >>|= fun (id: string) ->
             match message_id_opt_of_rpc (Jsonrpc.of_string id) with
             | None ->
               `Error (`Message_switch (`Queue_deleted dest_queue_name))
             | Some mid ->
               Hashtbl.add c.wakener mid t;
               `Ok mid
          )
      with
      | `Ok x -> `Ok x
      | `Error (`Message_switch (`Queue_deleted _)) as e -> e
      | `Error _ ->
        Thread.delay 5.;
        loop () in
    loop ()
    >>|= fun id ->
    (* now block waiting for our response *)
    match IO.Ivar.read t with
    | `Ok response ->
      (* release resources *)
      Opt.iter IO.Clock.cancel timer;
      IO.Mutex.with_lock c.requests_m (fun () -> Hashtbl.remove c.wakener id);
      `Ok response.Message.payload
    | `Error e -> `Error e

  let list ~t:c ~prefix ?(filter=`All) () =
    IO.Mutex.with_lock c.requests_m
      (fun () ->
         Connection.rpc c.requests_conn (In.List(prefix, filter))
         >>|= fun result ->
         `Ok (Out.string_list_of_rpc (Jsonrpc.of_string result))
      )

  let ack ~t:c ~message:(name, id) () =
    IO.Mutex.with_lock c.requests_m
      (fun () ->
         Connection.rpc c.requests_conn (In.Ack(name, id))
         >>|= fun (_: string) ->
         `Ok ()
      )

  let diagnostics ~t:c () =
    IO.Mutex.with_lock c.requests_m
      (fun () ->
         Connection.rpc c.requests_conn In.Diagnostics
         >>|= fun (result: string) ->
         `Ok (Diagnostics.t_of_rpc (Jsonrpc.of_string result))
      )

  let trace ~t:c ?(from=0L) ?(timeout=0.) () =
    IO.Mutex.with_lock c.requests_m
      (fun () ->
         Connection.rpc c.requests_conn (In.Trace(from, timeout))
         >>|= fun (result: string) ->
         `Ok (Out.trace_of_rpc (Jsonrpc.of_string result))
      )

  let shutdown ~t:c () =
    IO.Mutex.with_lock c.requests_m
      (fun () ->
        Connection.rpc c.requests_conn In.Shutdown
        >>|= fun (_: string) ->
        IO.IO.return (`Ok ())
      )

  let destroy ~t ~queue:queue_name () =
    IO.Mutex.with_lock t.requests_m
      (fun () ->
        Connection.rpc t.requests_conn (In.Destroy queue_name)
        >>|= fun (_: string) ->
        IO.IO.return (`Ok ())
      )
end

module Server = struct
  type 'a io = 'a

  type error = [
    | `Failed_to_read_response
    | `Unsuccessful_response
  ]

  type 'a result = ('a, [ `Message_switch of error ]) Message_switch_core.Mresult.result

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

  type t = unit

  let listen ~process ~switch ~queue:name () =
    let open IO.IO in
    let token = IO.whoami () in
    let reconnect () =
      let request_conn = IO.connect switch in
      Connection.rpc request_conn (In.Login token)
      >>|= fun (_: string) ->
      let reply_conn = IO.connect switch in
      Connection.rpc reply_conn (In.Login token)
      >>|= fun (_: string) ->
      Connection.rpc request_conn (In.Login token)
      >>|= fun (_: string) ->
      `Ok (request_conn, reply_conn) in

    reconnect ()
    >>|= fun ((request_conn, _reply_conn) as connections) ->
    (* Only allow one reply RPC at a time (no pipelining) *)
    let mutex = IO.Mutex.create () in

    Connection.rpc request_conn (In.CreatePersistent name)
    >>|= fun (_: string) ->

    let rec loop ((request_conn, reply_conn) as connections) from =
      let transfer = {
        In.from = from;
        timeout = Message_switch_core.Protocol.timeout;
        queues = [ name ];
      } in
      let frame = In.Transfer transfer in
      Connection.rpc request_conn frame >>= function
      | `Error _e ->
        IO.Mutex.with_lock mutex reconnect
        >>|= fun connections ->
        loop connections from
      | `Ok raw ->
        let transfer = Out.transfer_of_rpc (Jsonrpc.of_string raw) in
        begin match transfer.Out.messages with
          | [] -> loop connections from
          | _ :: _ ->
            List.iter
              (fun (i, m) ->
                 let (_: Thread.t) = Thread.create
                     (fun () ->
                        let response =
                          try
                            process m.Message.payload
                          with e ->
                            Printexc.to_string e in
                        response >>= fun response ->
                        begin
                          match m.Message.kind with
                          | Message.Response _ ->
                            (* response where a request should be: configuration error? *)
                            return ()
                          | Message.Request reply_to ->
                            let request = In.Send(reply_to, { Message.kind = Message.Response i; payload = response }) in
                            IO.Mutex.with_lock mutex (fun () -> Connection.rpc reply_conn request) >>= fun _ ->
                            return ()
                        end >>= fun () ->
                        let request = In.Ack i in
                        IO.Mutex.with_lock mutex (fun () -> Connection.rpc reply_conn request) >>= fun _ ->
                        ()
                     ) () in ()
              ) transfer.Out.messages;
            loop connections (Some transfer.Out.next)
        end in
    let (_: Thread.t) = Thread.create (loop connections) None in
    `Ok ()

  let shutdown ~t () =
    failwith "Shutdown is unimplemented"
end
