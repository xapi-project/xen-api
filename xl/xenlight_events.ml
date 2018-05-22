open Xenlight
open Xenops_utils
open Async

module D = Debug.Make(struct let name = "libxl_events" end)
open D

module Mailbox = struct
  (* A single-item mailbox for asynchronous communication between threads *)

  type 'a t = {
    m: Mutex.t;
    c: Condition.t;
    mutable mail: 'a option;
  }

  (* Create a new mailbox *)
  let create () =
    {m = Mutex.create (); c = Condition.create (); mail = None}

  (* Wait for mail, and return when received (blocking) *)
  let receive box =
    Mutex.execute box.m (fun () ->
        while box.mail = None do
          Condition.wait box.c box.m
        done;
        match box.mail with
        | Some v -> v
        | None -> failwith "Mailbox.receive: mail disappeared"
      )

  (* Post mail (non-blocking) *)
  let post box mail =
    Mutex.execute box.m (fun () ->
        box.mail <- Some mail;
      );
    Condition.signal box.c
end

(* event callbacks *)

let event_occurs_callback user event =
  let open Event in
  let ty = match event.xl_type with
    | Domain_shutdown _ -> "domain shutdown"
    | Domain_death -> "domain death"
    | Disk_eject _ -> "disk eject"
    | Operation_complete _ -> "operation complete"
    | Domain_create_console_available -> "domain create console available"
  in
  debug "EVENT occurred: %s, callback user %s, event user %Ld" ty user event.for_user

let event_disaster_callback user event_type msg errnoval =
  debug "EVENT disaster: %s, user %s" msg user

(* async callbacks *)

let async f =
  debug "ASYNC call";
  let box = Mailbox.create () in
  let result = f ?async:(Some box) () in
  debug "ASYNC call returned";
  let ret = Mailbox.receive box in
  debug "ASYNC synced with callback";
  match ret with
  | None ->
    result
  | Some e ->
    debug "libxl raised %s" (Xenlight.string_of_error e);
    raise (Error (e, "async call"))

let async_callback ~result ~user =
  debug "ASYNC callback";
  Mailbox.post user result;
  debug "ASYNC sent event notification"

(* event registration and main loop *)

let fds = ref []
let fds_m = Mutex.create ()

let timeouts = ref []
let timeouts_m = Mutex.create ()

let (interrupt_in, interrupt_out) = Unix.pipe ()

let (+++) = Int64.add
let (---) = Int64.sub

let earliest_timeout () =
  Mutex.execute timeouts_m (fun () ->
      match !timeouts with
      | [] -> None
      | hd :: tl -> Some hd
    )

let event_loop_start ctx =
  debug "EVENTLOOP: Starting event loop!";
  while true do
    let fds_in = !fds in
    let fds_in' = List.map (fun (_, a, b, _) -> a, b) fds_in in
    let now = Int64.of_float (Unix.gettimeofday () *. 1000.) in
    let timeout = earliest_timeout () in
    let timeout_ms = match timeout with
      | None -> -1
      | Some (_, s, us, _) when s = 0L && us = 0L -> 0
      | Some (_, s, us, _) -> (Int64.mul s 1000L) +++ (Int64.div us 1000L) --- now |> Int64.to_int
    in
    debug "EVENTLOOP: calling poll with timeout value %d (ms)" timeout_ms;
    let (interrupt, fds_out), rc = Poll.poll ((interrupt_in, [POLLIN]), fds_in') timeout_ms in
    if rc < 0 then
      warn "EVENTLOOP: poll error %d" rc
    else if rc = 0 then begin
      Opt.iter (fun (token, _, _, for_libxl) ->
          debug "EVENTLOOP: timeout occurred, token = %s" token;
          Mutex.execute timeouts_m (fun () -> timeouts := List.filter (fun (token', _, _, _) -> token <> token') !timeouts);
          osevent_occurred_timeout ctx for_libxl
        ) timeout
    end else begin
      (* callbacks into libxl *)
      List.iter2 (fun (token, fd, events, for_libxl) revents ->
          if revents <> [] then begin
            debug "EVENTLOOP: fd event occurred, token = %s" token;
            osevent_occurred_fd ctx for_libxl fd events revents
          end
        ) fds_in fds_out;
      (* if the poll was interrupted by an fd update, clear the signal *)
      if interrupt <> [] then begin
        debug "EVENTLOOP: poll interrupted for update";
        let buf = Bytes.create 32 in
        ignore (Unix.read interrupt_in buf 0 32)
      end
    end
  done

let event_loop_init ctx =
  let fd_counter = ref 0 in
  let timeout_counter = ref 0 in
  let fd_register _ fd events for_libxl =
    let token = Mutex.execute fds_m (fun () ->
        fd_counter := !fd_counter + 1;
        let token = "fd" ^ (string_of_int !fd_counter) in
        debug "EVENTREG: registering fd, token = %s" token;
        fds := (token, fd, events, for_libxl) :: !fds;
        token
      ) in
    ignore (Unix.write interrupt_out (Bytes.of_string "r") 0 1);
    token
  in
  let fd_modify _ fd token events =
    debug "EVENTREG: modifying fd, token = %s" token;
    let rec replace = function
      | [] -> []
      | (_, fd', _, for_libxl) :: tl when fd' = fd -> (token, fd, events, for_libxl) :: tl
      | hd :: tl -> hd :: replace tl
    in
    Mutex.execute fds_m (fun () -> fds := replace !fds);
    ignore (Unix.write interrupt_out (Bytes.of_string "m") 0 1);
    token
  in
  let fd_deregister _ fd token =
    debug "EVENTREG: deregistering fd, token = %s" token;
    Mutex.execute fds_m (fun () -> fds := List.filter (fun (_, fd', _, _) -> fd <> fd') !fds);
    ignore (Unix.write interrupt_out (Bytes.of_string "d") 0 1)
  in
  let timeout_register _ s us for_libxl =
    let token = Mutex.execute timeouts_m (fun () ->
        timeout_counter := !timeout_counter + 1;
        let token = "timeout" ^ (string_of_int !timeout_counter) in
        debug "EVENTREG: registering timeout (%Lds, %Ldus), token = %s" s us token;
        timeouts := (token, s, us, for_libxl) :: !timeouts;
        token
      ) in
    ignore (Unix.write interrupt_out (Bytes.of_string "t") 0 1);
    token
  in
  let timeout_fire_now _ token =
    debug "EVENTREG: triggering timeout, token = %s" token;
    let rec pop (x, ac) = function
      | [] -> x, ac
      | (token', _, _, for_libxl) :: tl when token' = token -> Some for_libxl, (List.rev tl @ ac)
      | hd :: tl -> pop (x, hd :: ac) tl
    in
    Mutex.execute timeouts_m (fun () ->
        match pop (None, []) !timeouts with
        | Some for_libxl, timeouts' ->
          timeouts := (token, 0L, 0L, for_libxl) :: (List.rev timeouts')
        | None, _ ->
          error "EVENTREG: cannot find timeout entry for token %s" token;
          failwith "invalid token"
      );
    ignore (Unix.write interrupt_out (Bytes.of_string "t") 0 1);
    token
  in
  debug "EVENTREG: Registering event hooks";
  Unix.set_nonblock interrupt_out;
  let _ = osevent_register_hooks ctx ~user:0 ~fd_register ~fd_modify ~fd_deregister ~timeout_register ~timeout_fire_now in
  let _ = event_register_callbacks ctx ~user:"xenopsd-event" ~event_occurs_callback ~event_disaster_callback in
  let _ = async_register_callback ~async_callback in
  Thread.create (fun () -> event_loop_start ctx) ()

