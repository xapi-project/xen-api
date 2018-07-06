open! Core_kernel
open! Import

module Time_ns = struct
  include Time_ns

  external format : float -> string -> string = "core_kernel_time_ns_format"

  (* We use a more pleasant format than [Core_kernel.Time_ns.sexp_of_t],
     which has to be messier for round trippability. *)
  let sexp_of_t t =
    [%sexp (format (t |> to_span_since_epoch |> Span.to_sec) "%Y-%m-%dT%H:%M:%S%z"
            : string)]
  ;;
end

module Alarm = struct
  include Timing_wheel_ns.Alarm

  let is_null t = phys_equal t (null ())
end

module Alarm_precision = Timing_wheel_ns.Alarm_precision

let default_timing_wheel_config =
  (* 1/8th of a millisecond alarm_precision seems sufficient to avoid having many alarms
     in the same interval, which avoids quadratic insertion sort when firing alarms.  And
     the level bits give us levels of >1s, >1m, >1h, >1d.  See test in
     [../test/test_synchronous_time_source.ml]. *)
  Timing_wheel_ns.Config.create
    ~alarm_precision:(Alarm_precision.(div about_one_millisecond ~pow2:3))
    ~level_bits:(Timing_wheel_ns.Level_bits.create_exn [ 13; 6; 6; 5 ])
    ()
;;

type callback = unit -> unit

module T1 = struct
  module Event = struct
    module Status = struct
      type t = Types.Event.Status.t =
        | Aborted   (* will never run *)
        | Fired     (* in [fired_events], ready to run *)
        | Happening (* currently running the callback *)
        | Happened  (* callback ran *)
        | Scheduled (* in the timing wheel *)
      [@@deriving compare, sexp_of]

      let transition_is_allowed ~from ~to_ =
        match from, to_ with
        | Fired,     Aborted   (* aborted *)
        | Fired,     Happening (* started running callback *)
        | Happening, Happened  (* finished running callback *)
        | Happening, Scheduled (* for repeating events *)
        | Scheduled, Aborted   (* aborted *)
        | Scheduled, Fired     (* moved from timing wheel to [fired_events] *)
          -> true
        | (Aborted
          | Fired
          | Happening
          | Happened
          | Scheduled), _
          -> false
      ;;
    end

    type t = Types.Event.t =
      { (* [alarm] is non-null iff the event is in the timing wheel. *)
        mutable alarm      : Job_or_event.t Alarm.t
      ; mutable at         : Time_ns.t
      ; callback           : unit -> unit
      ; execution_context  : Execution_context.t
      ; mutable interval   : Time_ns.Span.t option
      (* [next_fired] is a singly-linked list of fired events, linked via [next_fired].
         An event is added to the list when it fires, either because it is added with
         a time in the past, or because time advances.  [advance_by_alarms] iterates
         over the events in [next_fired] and runs them, emptying the list. *)
      ; mutable next_fired : t
      ; mutable status     : Status.t }
    [@@deriving fields]

    (* [none] is used to indicate the end of the singly-linked list of fired events. *)
    let rec none =
      { alarm             = Alarm.null ()
      ; at                = Time_ns.min_value
      ; callback          = (fun () -> assert false)
      ; execution_context = Execution_context.main
      ; interval          = None
      ; next_fired        = none
      ; status            = Aborted }
    ;;

    let is_none t = phys_equal t none
    let is_some t = not (is_none t)

    let sexp_of_t { alarm             = _
                  ; at
                  ; callback          = _
                  ; execution_context = _
                  ; interval
                  ; next_fired        = _
                  ; status } =
      [%message
        ""
          (status : Status.t)
          (at : Time_ns.t)
          (interval : Time_ns.Span.t option)]
    ;;

    let invariant t =
      Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
        let check f = Invariant.check_field t f in
        Fields.iter
          ~alarm:(check (fun alarm ->
            [%test_result: bool] (Alarm.is_null alarm)
              ~expect:(
                match t.status with
                | Aborted | Fired | Happening | Happened -> true
                | Scheduled -> false)))
          ~at:ignore
          ~callback:ignore
          ~execution_context:ignore
          ~interval:ignore
          ~next_fired:ignore
          ~status:ignore)
    ;;

    let compare_at t1 t2 = Time_ns.compare t1.at t2.at

    let set_status t to_ =
      let from = t.status in
      if not (Status.transition_is_allowed ~from ~to_)
      then (
        raise_s [%message [%here] "bug -- set_status transition not allowed"
                            (from : Status.t) (to_ : Status.t) ~event:(t : t)]);
      t.status <- to_;
    ;;
  end

  module Job_or_event = struct
    include Job_or_event

    let sexp_of_t t =
      let open Job_or_event.Match in
      let K k = kind t in
      match k, project k t with
      | Event, event -> [%sexp (event : Event.t)]
      | Job, _ ->
        (* We don't display the [Job.t]s in [events] because those are
           pool pointers, which are uninformative. *)
        [%message "<Job.t>"]
    ;;
  end

  type -'rw t = 'rw Types.Time_source.t1 =
    { (* [advance_errors] accumulates errors raised by alarms run by
         [advance_by_alarms]. *)
      mutable advance_errors : Error.t list
    ; (* [am_advancing] is true only during [advance_by_alarms], and is used to cause
         callbacks to raise if they call [advance_by_alarms]. *)
      mutable am_advancing   : bool
    ; events                 : Job_or_event.t Timing_wheel_ns.t
    (* [fired_events] is the front of the singly linked list of fired events, which is
       stored in increasing order of [Event.at]. *)
    ; mutable fired_events   : Event.t
    (* We store [handle_fired] in [t] to avoid allocating it every time we call
       [advance_clock]. *)
    ; handle_fired           : Job_or_event.t Alarm.t -> unit
    ; is_wall_clock          : bool
    ; scheduler              : Scheduler0.t }
  [@@deriving fields]

  let sexp_of_t _ { advance_errors  = _
                  ; am_advancing    = _
                  ; events
                  ; fired_events    = _
                  ; handle_fired    = _
                  ; is_wall_clock
                  ; scheduler       = _ } =
    let now = Timing_wheel_ns.now events in
    if is_wall_clock
    then [%message "wall_clock" (now : Time_ns.t)]
    else (
      let all_events = ref [] in
      Timing_wheel_ns.iter events ~f:(fun alarm ->
        all_events := (Alarm.at events alarm, Alarm.value events alarm) :: !all_events);
      let events =
        List.sort !all_events ~compare:(fun (at1, _) (at2, _) -> Time_ns.compare at1 at2)
        |> List.map ~f:snd
      in
      [%message
        ""
          (now : Time_ns.t)
          (events : Job_or_event.t list)])
  ;;

  let timing_wheel_now t = Timing_wheel_ns.now t.events

  let invariant (type rw) (t : rw t) =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~advance_errors:ignore
        ~am_advancing:ignore
        ~events:(check (fun events ->
          Timing_wheel_ns.invariant ignore events;
          Timing_wheel_ns.iter events ~f:(fun alarm ->
            let job_or_event = Alarm.value events alarm in
            let open Job_or_event.Match in
            let K k = kind job_or_event in
            match k, project k job_or_event with
            | Job   , job -> Job.invariant job
            | Event , event ->
              assert (phys_equal alarm event.alarm);
              [%test_result: Time_ns.t] event.at ~expect:(Alarm.at events alarm);
              [%test_result: Event.Status.t] event.status ~expect:Scheduled)))
        ~fired_events:(check (fun (fired_events : Event.t) ->
          let current = ref fired_events in
          while Event.is_some !current do
            assert (Time_ns.( <= ) !current.at (timing_wheel_now t));
            let next = !current.next_fired in
            if Event.is_some next then (assert (Time_ns.( <= ) !current.at next.at));
            current := next;
          done))
        ~handle_fired:ignore
        ~is_wall_clock:ignore
        ~scheduler:ignore)
  ;;
end

open T1

type t = read T1.t [@@deriving sexp_of]

let invariant = invariant

module Read_write = struct
  type t = read_write T1.t [@@deriving sexp_of]

  let invariant = invariant
end

let is_wall_clock t = t.is_wall_clock

let read_only (t : [> read] T1.t) = (t :> t)

(* [fire t event] sets [event.status = Fired] and inserts [event] into [t.fired_events] in
   sorted time order. *)
let fire t (event : Event.t) =
  (match event.status with
   | Scheduled -> ()
   | Aborted | Fired | Happening | Happened -> assert false);
  Event.set_status event Fired;
  event.alarm <- Alarm.null ();
  let prev = ref Event.none in
  let current = ref t.fired_events in
  while Event.is_some !current && Time_ns.( < ) !current.at event.at do
    prev := !current;
    current := !current.next_fired;
  done;
  event.next_fired <- !current;
  if Event.is_none !prev
  then (t.fired_events <- event)
  else (!prev.next_fired <- event);
;;

let alarm_precision t = Timing_wheel_ns.alarm_precision t.events

let now t =
  if t.is_wall_clock
  then (Time_ns.now ())
  else (timing_wheel_now t)
;;

let timing_wheel_now = timing_wheel_now

let schedule t (event : Event.t) =
  event.alarm <-
    Timing_wheel_ns.add t.events ~at:event.at (event |> Job_or_event.of_event);
;;

module Event = struct
  include Event

  let add t ~at ~interval ~callback =
    let event =
      { alarm             = Alarm.null ()
      ; at
      ; callback
      ; execution_context = t.scheduler.current_execution_context
      ; interval
      ; next_fired        = none
      ; status            = Scheduled } in
    if Time_ns.( <= ) at (timing_wheel_now t)
    then (fire t event)
    else (schedule t event);
    event
  ;;

  let at t at callback = add t ~at ~interval:None ~callback

  let after t span callback = at t (Time_ns.after (now t) span) callback

  let at_intervals t span callback =
    let alarm_precision = alarm_precision t in
    if Time_ns.Span.( < ) span alarm_precision
    then (
      raise_s [%message "at_intervals got span smaller than alarm precision"
                          (span : Time_ns.Span.t)
                          (alarm_precision : Time_ns.Span.t)]);
    add t ~at:(now t) ~interval:(Some span) ~callback
  ;;

  module Abort_result = struct
    type t =
      | Ok
      | Currently_happening
      | Previously_aborted
      | Previously_happened
    [@@deriving sexp_of]
  end

  let abort t (event : Event.t) : Abort_result.t =
    match event.status with
    | Aborted   -> Previously_aborted
    | Happened  -> Previously_happened
    | Happening ->
      if Option.is_none event.interval
      then Currently_happening
      else (event.interval <- None; Ok)
    | Fired ->
      Event.set_status event Aborted;
      Ok
    | Scheduled ->
      Event.set_status event Aborted;
      Timing_wheel_ns.remove t.events event.alarm;
      event.alarm <- Alarm.null ();
      Ok
  ;;

  let abort_if_possible t event = ignore (abort t event : Abort_result.t)

  let abort_exn t event =
    match abort t event with
    | Ok -> ()
    | reason ->
      raise_s [%message "[Synchronous_time_source.abort_exn] cannot abort event"
                          (reason : Abort_result.t)]
  ;;
end

let run_after        t span callback = ignore (Event.after        t span callback : Event.t)
let run_at           t at   callback = ignore (Event.at           t at   callback : Event.t)
let run_at_intervals t span callback = ignore (Event.at_intervals t span callback : Event.t)

type send_exn
  =  Monitor0.t
  -> ?backtrace:[ `Get | `This of Backtrace.t ]
  -> exn
  -> unit

let run_fired_events t ~(send_exn : send_exn option) =
  let current_execution_context = t.scheduler.current_execution_context in
  while Event.is_some t.fired_events do
    let event = t.fired_events in
    t.fired_events <- event.next_fired;
    event.next_fired <- Event.none;
    match event.status with
    | Aborted -> ()
    | Happened | Happening | Scheduled -> assert false
    | Fired ->
      Event.set_status event Happening;
      let status : Event.Status.t =
        (* We set the execution context so that [event.callback] runs in the same context
           that was in place when [event] was created. *)
        Scheduler0.set_execution_context t.scheduler event.execution_context;
        match event.callback () with
        | exception exn ->
          (match send_exn with
           | None -> t.advance_errors <- Error.of_exn exn :: t.advance_errors
           | Some send_exn ->
             let backtrace = Backtrace.get () in
             send_exn event.execution_context.monitor exn ~backtrace:(`This backtrace));
          Happened
        | () ->
          match event.interval with
          | None -> Happened
          | Some interval ->
            event.at <-
              Time_ns.next_multiple ()
                ~base:event.at ~after:(timing_wheel_now t) ~interval;
            schedule t event;
            Scheduled
      in
      Event.set_status event status;
  done;
  Scheduler0.set_execution_context t.scheduler current_execution_context;
;;

let advance_clock t ~to_ ~send_exn =
  Timing_wheel_ns.advance_clock t.events ~to_ ~handle_fired:t.handle_fired;
  run_fired_events t ~send_exn;
;;

let fire_past_alarms t ~send_exn =
  Timing_wheel_ns.fire_past_alarms t.events ~handle_fired:t.handle_fired;
  run_fired_events t ~send_exn;
;;

let advance t ~to_ ~send_exn =
  advance_clock    t ~to_ ~send_exn;
  fire_past_alarms t      ~send_exn;
;;

let advance_by_alarms t ~to_ =
  let send_exn = None in
  if t.am_advancing
  then (raise_s [%message "cannot call [advance_by_alarms] from callback"]);
  t.am_advancing <- true;
  (match t.advance_errors with
   | [] -> ()
   | _ -> t.advance_errors <- []);
  run_fired_events t ~send_exn;
  let continue = ref true in
  while !continue do
    if Timing_wheel_ns.is_empty t.events
    then (continue := false)
    else (
      let next_alarm_fires_at = Timing_wheel_ns.next_alarm_fires_at_exn t.events in
      if Time_ns.( >= ) next_alarm_fires_at to_
      then (continue := false)
      else (
        (* We use the actual alarm time, rather than [next_alarm_fires_at], so as not to
           expose (or accumulate errors associated with) the precision of
           [Timing_wheel_ns]. *)
        advance t
          ~to_:(Timing_wheel_ns.max_alarm_time_in_min_interval_exn t.events)
          ~send_exn))
  done;
  advance t ~to_ ~send_exn;
  t.am_advancing <- false;
  match t.advance_errors with
  | [] -> Ok ()
  | errors ->
    t.advance_errors <- [];
    Error (Error.of_list errors)
;;
