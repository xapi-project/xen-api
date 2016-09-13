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
(*

  This module provides a facility to make XML-RPC/HTTPS requests to a remote
  server, and time out the request, killing the stunnel instance if needs be.
  It's used by the WLB code, but could be extended to be used elsewhere in the
  future, hopefully.

  There is a single request-handling thread, onto which requests are
  marshalled.  The thinking here is that there is little point sending
  multiple requests to the WLB server in parallel -- it's unlikely to respond
  any quicker, and this way we reduce the chance of DoS.

  Each request that comes in (on its own task-handling thread) queues the
  request onto the request-handling thread, and then launches a watcher
  thread to handle the timeout.

  If the timeout expires, then the stunnel instance handling the RPC is
  killed, to make cleanup quicker (i.e. to avoid waiting for the TCP timeout).
*)

open Stdext
open Printf
open Threadext

module D = Debug.Make(struct let name = "remote_requests" end)
open D

exception Timed_out
exception Internal_error

type response =
  | Success | Exception of exn | NoResponse

type queued_request = {
  task : API.ref_task;
  verify_cert : bool;
  host : string;
  port : int;
  request : Http.Request.t;
  handler : Http.Response.t -> Unix.file_descr -> unit;
  resp : response ref;
  resp_mutex : Mutex.t;
  resp_cond : Condition.t;
  enable_log : bool;
}

let make_queued_request task verify_cert host port request handler
    resp resp_mutex resp_cond enable_log =
  {
    task = task;
    verify_cert = verify_cert;
    host = host;
    port = port;
    request = request;
    handler = handler;
    resp = resp;
    resp_mutex = resp_mutex;
    resp_cond = resp_cond;
    enable_log = enable_log;
  }

let shutting_down = ref false
let request_queue : queued_request list ref = ref []
let request_mutex = Mutex.create()
let request_cond = Condition.create()

let signal_result' req result () =
  if !(req.resp) = NoResponse then
    begin
      req.resp := result;
      Condition.signal req.resp_cond
    end

let signal_result req result =
  Mutex.execute req.resp_mutex (signal_result' req result)

let watcher_thread = function
  | (__context, timeout, delay, req) ->
    ignore (Delay.wait delay timeout);
    Mutex.execute req.resp_mutex
      (fun () ->
         if !(req.resp) = NoResponse then
           begin
             warn "Remote request timed out";
             let resources = Locking_helpers.Thread_state.get_acquired_resources_by_task req.task in
             List.iter Locking_helpers.kill_resource resources;
             signal_result' req (Exception Timed_out) ()
           end)

let handle_request req =
  try
    let open Xmlrpc_client in
    let transport = SSL(SSL.make ~verify_cert:req.verify_cert ~task_id:(Ref.string_of req.task) (), req.host, req.port) in
    with_transport transport
      (with_http req.request
         (fun (response, s) ->
            req.handler response s;
            signal_result req Success
         )
      )
  with
  | exn ->
    if req.enable_log then
      warn "Exception handling remote request %s: %s" (Opt.default "" req.request.Http.Request.body)
        (ExnHelper.string_of_exn exn);
    signal_result req (Exception exn)

let handle_requests () =
  while Mutex.execute request_mutex (fun () -> not !shutting_down) do
    try
      let req =
        Mutex.execute request_mutex
          (fun () ->
             while !request_queue = [] do
               Condition.wait request_cond request_mutex;
             done;
             let q = !request_queue in
             request_queue := List.tl q;
             List.hd q)
      in
      handle_request req
    with
    | exn ->
      error "Exception in handle_requests thread!  %s"
        (ExnHelper.string_of_exn exn);
      Thread.delay 30.
  done

let start_watcher __context timeout delay req =
  ignore (Thread.create watcher_thread (__context, timeout, delay, req))

let queue_request req =
  Mutex.execute request_mutex
    (fun () ->
       request_queue := req :: !request_queue;
       Condition.signal request_cond)

let perform_request ~__context ~timeout ~verify_cert ~host ~port
    ~request ~handler ~enable_log =
  let task = Context.get_task_id __context in
  let resp = ref NoResponse in
  let resp_mutex = Mutex.create() in
  let resp_cond = Condition.create() in
  Mutex.execute resp_mutex
    (fun () ->
       let delay = Delay.make () in
       let req =
         make_queued_request
           task verify_cert host port request handler
           resp resp_mutex resp_cond enable_log
       in
       start_watcher __context timeout delay req;
       queue_request req;

       Condition.wait resp_cond resp_mutex;
       Delay.signal delay;

       match !resp with
       | Success ->
         ()
       | Exception exn ->
         raise exn
       | NoResponse ->
         error "No response in perform_request!";
         raise Internal_error)

let stop_request_thread () =
  Mutex.execute request_mutex
    (fun () ->
       shutting_down := true;
       Condition.signal request_cond)

let read_response result response s =
  try
    result := Unixext.string_of_fd s
  with
  | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
    raise Xmlrpc_client.Connection_reset

let send_test_post ~__context ~host ~port ~body =
  try
    let result = ref "" in
    let request = Xapi_http.http_request ~keep_alive:false ~body
        ~headers:["Host", host] Http.Post "/" in
    perform_request ~__context ~timeout:30.0 ~verify_cert:true
      ~host ~port:(Int64.to_int port) ~request
      ~handler:(read_response result) ~enable_log:true;
    !result
  with
  | Timed_out ->
    raise (Api_errors.Server_error
             (Api_errors.wlb_timeout, ["30.0"]))
  | Stunnel.Stunnel_verify_error reason ->
    raise (Api_errors.Server_error
             (Api_errors.ssl_verify_error, [reason]))
