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

(**
 * @group Command-Line Interface (CLI)
*)
module D = Debug.Make (struct
  let name = "cli"
end)

open D
module Unixext = Xapi_stdext_unix.Unixext
open Cli_protocol
open Client

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

let internal_error fmt =
  Printf.ksprintf
    (fun str ->
      error "%s" str ;
      raise Api_errors.(Server_error (internal_error, [str]))
    )
    fmt

let log_exn_continue msg f x =
  try f x
  with e -> debug "Ignoring exception: %s while %s" (Printexc.to_string e) msg

exception Cli_failure of string

(** call [callback task_record] on every update to the task, until it completes or fails *)
let track callback rpc (session_id : API.ref_session) task =
  let use_event_next = !Constants.use_event_next in
  let classes =
    if use_event_next then
      ["task"]
    else
      [Printf.sprintf "task/%s" (Ref.string_of task)]
  in
  finally
    (fun () ->
      let finished = ref false in
      while not !finished do
        if use_event_next then
          Client.Event.register ~rpc ~session_id ~classes ;
        try
          (* Need to check once after registering to avoid a race *)
          finished :=
            Client.Task.get_status ~rpc ~session_id ~self:task <> `pending ;
          let token = ref "" in
          while not !finished do
            let events =
              if use_event_next then
                let events =
                  Event_types.events_of_rpc (Client.Event.next ~rpc ~session_id)
                in
                List.map Event_helper.record_of_event events
              else
                let event_from =
                  Event_types.event_from_of_rpc
                    (Client.Event.from ~rpc ~session_id ~classes ~token:!token
                       ~timeout:30.
                    )
                in
                token := event_from.token ;
                List.map Event_helper.record_of_event event_from.events
            in
            List.iter
              (function
                | Event_helper.Task (t, Some t_rec) when t = task ->
                    callback t_rec
                | _ ->
                    ()
                )
              events ;
            let matches = function
              | Event_helper.Task (t, Some t_rec) ->
                  t = task && t_rec.API.task_status <> `pending
              | _ ->
                  false
            in
            finished := List.fold_left ( || ) false (List.map matches events)
          done
        with
        | Api_errors.Server_error (code, _)
        when code = Api_errors.events_lost
        ->
          debug "Caught EVENTS_LOST; reregistering" ;
          Client.Event.unregister ~rpc ~session_id ~classes
      done
    )
    (fun () -> Client.Event.unregister ~rpc ~session_id ~classes)

let result_from_task rpc session_id remote_task =
  match Client.Task.get_status ~rpc ~session_id ~self:remote_task with
  | `cancelling | `cancelled ->
      raise
        (Api_errors.Server_error
           (Api_errors.task_cancelled, [Ref.string_of remote_task])
        )
  | `pending ->
      failwith "wait_for_task_completion failed; task is still pending"
  | `success ->
      ()
  | `failure ->
      let error_info =
        Client.Task.get_error_info ~rpc ~session_id ~self:remote_task
      in
      let trace =
        Client.Task.get_backtrace ~rpc ~session_id ~self:remote_task
      in
      let exn =
        match error_info with
        | code :: params ->
            Api_errors.Server_error (code, params)
        | [] ->
            Failure
              (Printf.sprintf "Task failed but no error recorded: %s"
                 (Ref.string_of remote_task)
              )
      in
      Backtrace.(add exn (t_of_sexp (Sexplib.Sexp.of_string trace))) ;
      raise exn

(** Use the event system to wait for a specific task to complete (succeed, failed or be cancelled) *)
let wait_for_task_completion = track (fun _ -> ())

module P = Cli_progress_bar.Make (struct
  type t = float

  let to_float x = x
end)

let wait_for_task_completion_with_progress fd =
  let p = P.create 80 0. 1. in
  track (fun t ->
      let progress_updated = P.update p t.API.task_progress in
      if progress_updated then
        marshal fd
          (Command (PrintStderr (Printf.sprintf "\r%s" (P.string_of_bar p)))) ;
      if t.API.task_status <> `pending then (
        marshal fd (Command (PrintStderr "\n")) ;
        marshal fd (Command (PrintStderr (P.summarise p)))
      )
  )

let track_http_operation ?use_existing_task ?(progress_bar = false) fd rpc
    session_id (make_command : API.ref_task -> command) label =
  (* Need to associate the operation with a task so we can check for failure *)
  let task_id =
    match use_existing_task with
    | None ->
        Client.Task.create ~rpc ~session_id ~label ~description:""
    | Some t ->
        t
  in
  finally
    (fun () ->
      marshal fd (Command (make_command task_id)) ;
      let response = ref (Response Wait) in
      let receive_heartbeats =
        Thread.create
          (fun () ->
            while !response = Response Wait do
              response := unmarshal fd
            done
          )
          ()
      in
      (* Wait for the task to complete *)
      ( if progress_bar then
          wait_for_task_completion_with_progress fd
        else
          wait_for_task_completion
      )
        rpc session_id task_id ;
      Thread.join receive_heartbeats ;
      if !response = Response OK then
        if Client.Task.get_status ~rpc ~session_id ~self:task_id = `success then (
          let result = Client.Task.get_result ~rpc ~session_id ~self:task_id in
          debug "result was [%s]" result ;
          result
        ) else
          match Client.Task.get_error_info ~rpc ~session_id ~self:task_id with
          | [] ->
              raise Api_errors.(Server_error (internal_error, []))
          | err :: params ->
              raise Api_errors.(Server_error (err, params))
      else (
        debug "client-side reports failure" ;
        (* Debug info might have been written into the task, let's see if there is some *)
        Thread.delay 1. ;
        (* Bit of a race here - we can't simply wait for the task to be completed since *)
        (* 'response failed' doesn't indicate whether it managed to talk to the handler *)
        (* or not, so we don't know if the handler got the task_id to complete. The     *)
        (* import/export commands get round this by setting a negative progress, and    *)
        (* using this as an indicator that the handler never got the task. All handlers *)
        (* would need to use this mechanism if we want to check for it here. For now a  *)
        (* delay of 1 will do... *)
        match Client.Task.get_error_info ~rpc ~session_id ~self:task_id with
        | [] ->
            raise Api_errors.(Server_error (client_error, []))
        | err :: params ->
            raise Api_errors.(Server_error (err, params))
      )
    )
    (fun () ->
      (* if we created our own task then destroy it again; if the task was supplied to us then don't destroy it --
         	  if clients pass a task in on the command-line then they are responsible for destroying *)
      match use_existing_task with
      | None ->
          log_exn_continue "destroying task"
            (fun self -> Client.Task.destroy ~rpc ~session_id ~self)
            task_id
      | Some _ ->
          ()
    )

(* Rewrite the provisioning XML fragment to create all disks on a new, specified SR *)
let rewrite_provisioning_xml rpc session_id new_vm sr_uuid =
  let rewrite_xml xml newsrname =
    let rewrite_disk = function
      | Xml.Element ("disk", params, []) ->
          Xml.Element
            ( "disk"
            , List.map
                (fun (x, y) -> if x <> "sr" then (x, y) else ("sr", newsrname))
                params
            , []
            )
      | x ->
          x
    in
    match xml with
    | Xml.Element ("provision", [], disks) ->
        Xml.Element ("provision", [], List.map rewrite_disk disks)
    | x ->
        x
  in
  let other_config = Client.VM.get_other_config ~rpc ~session_id ~self:new_vm in
  if List.mem_assoc "disks" other_config then (
    let xml = Xml.parse_string (List.assoc "disks" other_config) in
    Client.VM.remove_from_other_config ~rpc ~session_id ~self:new_vm
      ~key:"disks" ;
    let newdisks = rewrite_xml xml sr_uuid in
    Client.VM.add_to_other_config ~rpc ~session_id ~self:new_vm ~key:"disks"
      ~value:(Xml.to_string newdisks)
  )

let get_default_sr_uuid rpc session_id =
  let pool = List.hd (Client.Pool.get_all ~rpc ~session_id) in
  let sr = Client.Pool.get_default_SR ~rpc ~session_id ~self:pool in
  try
    Some (Client.SR.get_uuid ~rpc ~session_id ~self:sr)
    (* throws an exception if not found *)
  with _ -> None

(* Given a string that might be a ref, lookup ref in cache and print uuid/name-label where possible *)
let ref_convert x =
  let module Ref_index = Xapi_database.Ref_index in
  match Ref_index.lookup x with
  | None ->
      x
  | Some ir -> (
      ir.Ref_index.uuid
      ^
      match ir.Ref_index.name_label with None -> "" | Some x -> " (" ^ x ^ ")"
    )

(* Marshal an API-style server-error *)
let get_server_error code params =
  let ( let* ) = Option.bind in
  let* error = Hashtbl.find_opt Datamodel.errors code in
  (* There ought to be a bijection between parameters mentioned in
     datamodel.ml and those in the exception but this is unchecked and
     false in some cases, defined here. *)
  let required =
    if code = Api_errors.vms_failed_to_cooperate then
      List.map (fun _ -> "VM") params
    else
      error.Datamodel_types.err_params
  in
  (* For the rest we attempt to pretty-print the list even when it's short/long *)
  let rec pp_params = function
    | t :: ts, v :: vs ->
        (t ^ ": " ^ v) :: pp_params (ts, vs)
    | [], v :: vs ->
        ("<extra>: " ^ v) :: pp_params ([], vs)
    | t :: ts, [] ->
        (t ^ ": <unknown>") :: pp_params (ts, [])
    | [], [] ->
        []
  in
  let errparams = pp_params (required, List.map ref_convert params) in
  Some (error.Datamodel_types.err_doc, errparams)

let server_error (code : string) (params : string list) sock =
  match get_server_error code params with
  | None ->
      marshal sock (Command (Error (code, List.map ref_convert params)))
  | Some (e, l) ->
      marshal sock (Command (PrintStderr (e ^ "\n"))) ;
      List.iter (fun pv -> marshal sock (Command (PrintStderr (pv ^ "\n")))) l

let user_says_yes fd =
  marshal fd (Command (Print "Type 'yes' to continue")) ;
  marshal fd (Command Prompt) ;
  let response =
    match unmarshal fd with
    | Blob (Chunk len) ->
        debug "Reading a chunk of %ld bytes" len ;
        Unixext.really_read_string fd (Int32.to_int len)
    | _ ->
        failwith "Protocol error"
  in
  (match unmarshal fd with Blob End -> () | _ -> failwith "Protocol error") ;
  let result = String.lowercase_ascii (Astring.String.trim response) = "yes" in
  if not result then
    marshal fd (Command (Print ("Aborted (you typed: '" ^ response ^ "')"))) ;
  result

type someone =
  | Master  (** I want to talk to the master *)
  | SpecificHost of API.ref_host
      (** I want to talk to [h] (who may be the master *)

(** Return a uri prefix which will cause the CLI to talk to either the
    	master or to a specific host (which may be the master). This will
    	work even when the management interface is disabled. *)
let rec uri_of_someone rpc session_id = function
  | Master ->
      (* See ocaml/xe-cli/newcli.ml:parse_url *)
      ""
  | SpecificHost h ->
      let pool = List.hd (Client.Pool.get_all ~rpc ~session_id) in
      let pool_master = Client.Pool.get_master ~rpc ~session_id ~self:pool in
      if h = pool_master then
        uri_of_someone rpc session_id Master
      else
        let address = Client.Host.get_address ~rpc ~session_id ~self:h in
        Uri.(make ~scheme:"https" ~host:address () |> to_string)

let error_of_exn e =
  match e with
  | Api_errors.Server_error (e, l) ->
      (e, l)
  | e ->
      (Api_errors.internal_error, [Printexc.to_string e])

let string_of_exn exn =
  let e, l = error_of_exn exn in
  Printf.sprintf "%s: [ %s ]" e (String.concat "; " l)

let get_pool ~rpc ~session_id =
  match Client.Pool.get_all ~rpc ~session_id with
  | [] ->
      internal_error "Remote host does not belong to a pool."
  | pool :: _ ->
      pool

let get_master ~rpc ~session_id =
  let pool = get_pool ~rpc ~session_id in
  Client.Pool.get_master ~rpc ~session_id ~self:pool
