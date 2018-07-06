open Core
open Import

module Schedule = Core.Schedule_v4_deprecated
include Schedule

(* To make sure time sources are passed through the entire implementation, we first shadow
   all wall-clock functionality, and make time-sources required arguments in the entire
   implementation.  Below the implementation, we then write wrappers that provide the
   default wall-clock time source. *)
open Require_explicit_time_source
module Time_source = struct
  include Time_source
  let wall_clock = [`Do_not_use_wall_clock]
  let _is_not_actually_used = wall_clock
end

type ('tag, 'output) pipe_emit =
  | Transitions
    : ('tag, 'tag Event.transition) pipe_emit
  | Transitions_and_tag_changes
    :  ('tag -> 'tag -> bool)
    -> ('tag, [ 'tag Event.transition | 'tag Event.tag_change ]) pipe_emit

let run_loop_for_testing_only time_source filter_event init =
  let r, w = Pipe.create () in
  let current_pending_event = ref None in
  upon (Pipe.closed w) (fun () ->
    Option.iter !current_pending_event ~f:(fun event ->
      Time_source.Event.abort_if_possible event ()));
  let res, sequence =
    match init with
    | `Started_in_range (tags, sequence) -> `Started_in_range (tags, r), sequence
    | `Started_out_of_range sequence     -> `Started_out_of_range r, sequence
  in
  let finished () = return (`Finished ()) in
  don't_wait_for begin
    Deferred.repeat_until_finished sequence
      (fun seq ->
         if Pipe.is_closed w
         then finished ()
         else begin
           match Sequence.next seq with
           | None               -> finished ()
           | Some (event, next) ->
             let time, event_opt = filter_event event in
             let event = Time_source.Event.at time_source (Time_ns.of_time time) in
             current_pending_event := Some event;
             Time_source.Event.fired event
             >>= function
             | Aborted  () -> finished ()
             | Happened () ->
               current_pending_event := None;
               let repeat = return (`Repeat next) in
               begin match event_opt with
               | None       -> repeat
               | Some event ->
                 Pipe.write_when_ready w ~f:(fun write -> write event)
                 >>= function
                 | `Closed -> finished ()
                 | `Ok ()  -> repeat
               end
         end)
    >>| fun () -> Pipe.close w
  end;
  res, `For_testing (current_pending_event, w)
;;

let run_loop time_source filter_map_event init =
  fst (run_loop_for_testing_only time_source filter_map_event init)

let get_reader = function
  | `Started_in_range (_, r) -> r
  | `Started_out_of_range r -> r
;;

type ('tag, 'output) pipe =
  [ `Started_in_range of 'tag list * 'output Pipe.Reader.t
  | `Started_out_of_range of 'output Pipe.Reader.t ]

let to_pipe (type tag) (type output) t ~start_time ~emit ~time_source ()
  : (tag, output) pipe =
  match (emit : (tag, output) pipe_emit) with
  | Transitions_and_tag_changes f ->
    let filter_map_event event =
      match event with
      | `No_change_until_at_least (_, time) -> time, None
      | `Enter (time, _)
      | `Leave time
      | `Change_tags (time, _) as event -> time, Some event
    in
    run_loop time_source filter_map_event
      (to_endless_sequence t ~start_time
         ~emit:(Transitions_and_tag_changes f))
  | Transitions ->
    let filter_map_event event =
      match event with
      | `No_change_until_at_least (_, time) -> time, None
      | `Enter (time, _)
      | `Leave time as event -> time, Some event
    in
    run_loop time_source filter_map_event
      (to_endless_sequence t ~start_time ~emit:Transitions)
;;

let next_transition_filtered t ~stop ~f ~time_source
      ?(after = Time_source.now time_source |> Time_ns.to_time) () =
  let r = get_reader (to_pipe t ~time_source ~start_time:after ~emit:Transitions ()) in
  upon (stop) (fun () -> Pipe.close_read r);
  let pipe = Pipe.filter_map r ~f in
  Pipe.read pipe
  >>= fun result ->
  Pipe.close_read pipe;
  match result with
  | `Eof  -> Deferred.never ()
  | `Ok v -> return v
;;

let next_enter t ~time_source ?after () =
  let f = function
    | `Enter (time, _) -> Some time
    | `Leave _         -> None
  in
  next_transition_filtered t ~f ~time_source ?after ()
;;

let next_leave t ~time_source ?after () =
  let f = function
    | `Enter _    -> None
    | `Leave time -> Some time
  in
  next_transition_filtered t ~time_source ~f ?after ()
;;

let next_event t ~event ~stop ~time_source ?after () =
  match event with
  | `Enter -> next_enter t ~stop ~time_source ?after ()
  | `Leave -> next_leave t ~stop ~time_source ?after ()
;;

module Every = struct
  let fill_optional_ivar optional_ivar x =
    Option.iter optional_ivar ~f:(fun i -> Ivar.fill i x)
  ;;

  let call_with_enter_and_get_leave ~enter ~tags ~monitor f =
    let leave_ivar = Ivar.create () in
    within ~monitor:monitor (fun () -> f ~tags ~enter ~leave:(Ivar.read leave_ivar));
    leave_ivar
  ;;

  let run_loop t ~f
        ~name
        ~start_time
        ~stop
        ~start_in_range_is_enter
        ~continue_on_error
        ~emit
        ~time_source
    =
    let start_event = Time_source.Event.at time_source (Time_ns.of_time start_time) in
    let stop_choice =
      choice stop (fun () ->
        Time_source.Event.abort_if_possible start_event
          `only_stop_choice_can_abort;
        Deferred.unit)
    in
    let start_choice =
      choice (Time_source.Event.fired start_event)
        (fun (Aborted `only_stop_choice_can_abort | Happened _) ->
           assert (not (Deferred.is_determined stop));
           let monitor = Monitor.create ~name () in
           let enter_and_get_leave ~enter ~tags =
             call_with_enter_and_get_leave ~enter ~tags ~monitor f
           in
           let pipe, initial_tags =
             match to_pipe t ~time_source ~start_time ~emit () with
             | `Started_out_of_range pipe     -> pipe, None
             | `Started_in_range (tags, pipe) -> pipe, Some tags
           in
           let should_quit () =
             Deferred.is_determined stop
             || (Monitor.has_seen_error monitor && not continue_on_error)
           in
           let quit _ = Pipe.close_read pipe in
           upon stop quit;
           if not continue_on_error then
             upon (Monitor.get_next_error monitor) quit;
           let init =
             Option.map initial_tags ~f:(fun tags ->
               if start_in_range_is_enter
               then enter_and_get_leave ~enter:start_time ~tags
               else Ivar.create ())
           in
           Pipe.fold_without_pushback pipe ~init ~f:(fun optional_leave_ivar transition ->
             if should_quit ()
             then begin
               quit ();
               optional_leave_ivar
             end else
               match transition with
               | `Enter (enter, tags) ->
                 assert (Option.is_none optional_leave_ivar);
                 Some (enter_and_get_leave ~enter ~tags)
               | `Change_tags (enter, tags) ->
                 assert (Option.is_some optional_leave_ivar);
                 fill_optional_ivar optional_leave_ivar enter;
                 Some (enter_and_get_leave ~enter ~tags)
               | `Leave time ->
                 assert (Option.is_some optional_leave_ivar);
                 fill_optional_ivar optional_leave_ivar time;
                 None)
           >>| fun (Some _ | None) -> ())
    in
    Deferred.join (choose [ stop_choice ; start_choice ])
  ;;

  let enter_without_pushback t ~time_source
        ?(start                   = Time_source.now time_source |> Time_ns.to_time)
        ?(stop                    = Deferred.never ())
        ?(continue_on_error       = true)
        ?(start_in_range_is_enter = true)
        f =
    run_loop t
      ~name:"Schedule.every_enter"
      ~start_time:start
      ~stop
      ~continue_on_error
      ~start_in_range_is_enter
      ~emit:Transitions
      ~time_source
      ~f:(fun ~tags:_ -> f)
    |> don't_wait_for
  ;;

  let tag_change_without_pushback t ~time_source
        ?(start                   = Time_source.now time_source |> Time_ns.to_time)
        ?(stop                    = Deferred.never ())
        ?(continue_on_error       = true)
        ?(start_in_range_is_enter = true)
        ~tag_equal
        f =
    run_loop t ~f
      ~name:"Schedule.every_tag_change"
      ~start_time:start
      ~stop
      ~continue_on_error
      ~start_in_range_is_enter
      ~emit:(Transitions_and_tag_changes tag_equal)
      ~time_source
    |> don't_wait_for
  ;;

end

type 'a every_enter_callback = enter:Time.t -> leave:Time.t Deferred.t -> 'a

let every_enter_without_pushback = Every.enter_without_pushback
let every_tag_change_without_pushback = Every.tag_change_without_pushback

let do_nothing_on_pushback ~enter:_ ~leave:_ = ()

let every_enter
      t
      ~time_source
      ?start
      ?stop
      ?(continue_on_error = true)
      ?(start_in_range_is_enter = true)
      ?(on_pushback=do_nothing_on_pushback)
      on_enter
  =
  let prior_event_finished = ref (return ()) in
  every_enter_without_pushback
    t
    ~time_source
    ?start
    ?stop
    ~continue_on_error
    ~start_in_range_is_enter
    (fun ~enter ~leave ->
       if Deferred.is_determined !prior_event_finished
       then begin
         if not continue_on_error
         then prior_event_finished := on_enter ~enter ~leave
         else
           don't_wait_for (
             try_with (fun () ->
               let finished = on_enter ~enter ~leave in
               prior_event_finished := finished;
               finished)
             >>| function
             | Ok () -> ()
             | Error e ->
               prior_event_finished := Deferred.unit;
               raise e)
       end
       else on_pushback ~enter ~leave)
;;

let do_nothing_on_tag_change_pushback ~tags:_ ~enter:_ ~leave:_ = ()

let every_tag_change
      t
      ~time_source
      ?start
      ?stop
      ?(continue_on_error = true)
      ?(start_in_range_is_enter = true)
      ?(on_pushback=do_nothing_on_tag_change_pushback)
      ~tag_equal
      on_tag_change
  =
  let prior_event_finished = ref (return ()) in
  every_tag_change_without_pushback
    t
    ~time_source
    ?start
    ?stop
    ~continue_on_error
    ~start_in_range_is_enter
    ~tag_equal
    (fun ~tags ~enter ~leave ->
       if Deferred.is_determined !prior_event_finished
       then begin
         if not continue_on_error
         then prior_event_finished := on_tag_change ~tags ~enter ~leave
         else
           don't_wait_for (
             try_with (fun () ->
               let finished = on_tag_change ~tags ~enter ~leave in
               prior_event_finished := finished;
               finished)
             >>| function
             | Ok () -> ()
             | Error e ->
               prior_event_finished := Deferred.unit;
               raise e)
       end
       else on_pushback ~tags ~enter ~leave)
;;

(* Now we re-introduce wall-clock functions and write wrappers so we can export functions
   that default to using wall-clock time. *)
open Core
open Import

let to_pipe t ~start_time ~emit ?(time_source = Time_source.wall_clock ()) () =
  to_pipe t ~start_time ~emit ~time_source ()

let next_event t ~event ~stop ?(time_source = Time_source.wall_clock ()) ?after () =
  next_event t ~event ~stop ~time_source ?after ()

let every_enter_without_pushback
      t
      ?(time_source = Time_source.wall_clock ())
      ?start
      ?stop
      ?continue_on_error
      ?start_in_range_is_enter
      callback
  =
  every_enter_without_pushback
    t
    ~time_source
    ?start
    ?stop
    ?continue_on_error
    ?start_in_range_is_enter
    callback

let every_enter
      t
      ?(time_source = Time_source.wall_clock ())
      ?start
      ?stop
      ?continue_on_error
      ?start_in_range_is_enter
      ?on_pushback
      on_enter
  =
  every_enter
    t
    ~time_source
    ?start
    ?stop
    ?continue_on_error
    ?start_in_range_is_enter
    ?on_pushback
    on_enter

let every_enter_without_pushback
      t
      ?(time_source = Time_source.wall_clock ())
      ?start
      ?stop
      ?continue_on_error
      ?start_in_range_is_enter
      f
  =
  every_enter_without_pushback
    t
    ~time_source
    ?start
    ?stop
    ?continue_on_error
    ?start_in_range_is_enter
    f

let every_tag_change_without_pushback
      t
      ?(time_source = Time_source.wall_clock ())
      ?start
      ?stop
      ?continue_on_error
      ?start_in_range_is_enter
      ~tag_equal
      f
  =
  every_tag_change_without_pushback
    t
    ~time_source
    ?start
    ?stop
    ?continue_on_error
    ?start_in_range_is_enter
    ~tag_equal
    f

let every_tag_change
      t
      ?(time_source = Time_source.wall_clock ())
      ?start
      ?stop
      ?continue_on_error
      ?start_in_range_is_enter
      ?on_pushback
      ~tag_equal
      on_tag_change
  =
  every_tag_change
    t
    ~time_source
    ?start
    ?stop
    ?continue_on_error
    ?start_in_range_is_enter
    ?on_pushback
    ~tag_equal
    on_tag_change

let%test_module "test run loop semenatics" =
  (module struct
    let async_unit_test = Thread_safe.block_on_async_exn

    let test_filter_events = function
      | `No_change_until_at_least (_, time) -> time, None
      | `Enter (time, _)
      | `Leave time as event -> time, Some event
    ;;


    let%test_unit "resource cleanup on end of sequence" =
      let test_schedule = In_zone (Time.Zone.utc, Mins [5]) in
      let test_start_time  = Time.of_string "2015-01-01 01:00:00" in
      let time_source = Time_source.wall_clock () in
      async_unit_test (fun () ->
        let seq =
          match to_endless_sequence test_schedule ~start_time:test_start_time ~emit:Transitions with
          | `Started_in_range (tags, seq) -> `Started_in_range (tags, Sequence.take seq 1)
          | `Started_out_of_range seq -> `Started_out_of_range (Sequence.take seq 1)
        in
        let res, `For_testing (current_pending_event, w) =
          run_loop_for_testing_only time_source test_filter_events seq
        in
        let r = get_reader res in
        Pipe.fold r ~init:() ~f:(fun () _ -> return ())
        >>| fun () ->
        assert (Pipe.is_closed r);
        assert (Pipe.is_closed w);
        assert (!current_pending_event = None))
    ;;

    let%test_unit "resource cleanup on pipe close" =
      let test_start_time = Time.add (Time.now ()) (Time.Span.of_hr 24.) in
      (* The following is too far in the future in 32bit *)
      (* let test_start_time = Time.of_string "2030-01-01 01:01:00" in *)
      let test_schedule = In_zone (Time.Zone.utc, Never) in
      let time_source = Time_source.wall_clock () in
      async_unit_test (fun () ->
        let res, `For_testing (current_pending_event, w) =
          run_loop_for_testing_only time_source test_filter_events
            (to_endless_sequence test_schedule
               ~start_time:test_start_time  ~emit:Transitions)
        in
        let r = get_reader res in
        Pipe.close_read r;
        Time_source.Event.fired (Option.value_exn !current_pending_event)
        >>= function
        | Happened () -> assert false
        | Aborted  () ->
          Pipe.closed w)
    ;;
  end)

