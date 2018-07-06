module Async_schedule = Schedule_v5
module Core_schedule = Core.Schedule_v5
open Core
open Import

let async_unit_test = Thread_safe.block_on_async_exn

let%test_module _ =
  (module (struct
    open Async_schedule

    (* proves that the type equality is maintained *)
    let _ = [ Core_schedule.Always; Async_schedule.Always ]

    let next_schedule = In_zone (Time.Zone.utc, Mins [5])

    let run_schedule =
      In_zone
        (Time.Zone.utc,
         Or [ Tag (1.0, Mins  [5])
            ; Tag (2.0, Hours [1])
            ])

    let after =
      Time.of_date_ofday
        ~zone:Time.Zone.utc
        (Date.of_string "2015-01-01")
        (Time.Ofday.create ~hr:0 ~min:4 ~sec:59 ())
    ;;

    let expected_time ~hr ~min ~sec =
      Time.of_date_ofday
        ~zone:Time.Zone.utc
        (Date.of_string "2015-01-01")
        (Time.Ofday.create ~hr ~min ~sec ())
    ;;

    let expected_enter  = expected_time ~hr:0 ~min:5 ~sec:0
    let expected_leave  = expected_time ~hr:0 ~min:6 ~sec:0
    let mid             = expected_time ~hr:0 ~min:5 ~sec:30
    let expected_enter2 = expected_time ~hr:1 ~min:0 ~sec:0
    let expected_leave2 = expected_time ~hr:1 ~min:5 ~sec:0
    let expected_enter3 = expected_time ~hr:1 ~min:5 ~sec:0
    let expected_leave3 = expected_time ~hr:1 ~min:6 ~sec:0
    let expected_enter4 = expected_time ~hr:1 ~min:6 ~sec:0
    let expected_leave4 = expected_time ~hr:2 ~min:0 ~sec:0
    ;;

    type 'a read_exactly =
      [ `Eof | `Fewer of 'a Queue.t | `Exactly of 'a Queue.t ]
    [@@deriving sexp_of, compare]

    let%test_unit "every_enter_without_pushback" =
      async_unit_test (fun () ->
        let enter_reader, enter_writer = Pipe.create () in
        let leave_reader, leave_writer = Pipe.create () in
        every_enter_without_pushback run_schedule ~start:after (fun ~enter ~leave ->
          don't_wait_for
            (Pipe.write enter_writer enter
             >>= fun () ->
             leave
             >>= fun leave_time ->
             Pipe.write leave_writer leave_time));
        Pipe.read_exactly enter_reader ~num_values:2
        >>= fun enters ->
        [%test_result: Time.t read_exactly]
          enters
          ~expect:(`Exactly (Queue.of_list [ expected_enter ; expected_enter2 ]));
        Pipe.read_exactly leave_reader ~num_values:2
        >>| fun leaves ->
        [%test_result: Time.t read_exactly]
          leaves
          ~expect:(`Exactly (Queue.of_list [ expected_leave ; expected_leave4 ])))
    ;;

    let wait_for_deferred_errors () =
      Clock.after Time.Span.millisecond

    let%test_unit "every_enter_without_pushback, immediate stop" =
      async_unit_test (fun () ->
        every_enter_without_pushback run_schedule ~start:after ~stop:Deferred.unit
          (fun ~enter:_ ~leave:_ ->
             assert false);
        wait_for_deferred_errors ())
    ;;

    let%test_unit "every_enter_without_pushback, stop between enter and leave" =
      async_unit_test (fun () ->
        let has_been_called = ref false in
        let stop_ivar = Ivar.create () in
        let stop = Ivar.read stop_ivar in
        every_enter_without_pushback run_schedule ~start:after ~stop (fun ~enter:_ ~leave ->
          if !has_been_called
          then assert false
          else
            begin
              has_been_called := true;
              Ivar.fill stop_ivar ();
              upon leave (fun _ -> assert false)
            end);
        stop
        >>= fun () ->
        wait_for_deferred_errors ())

    let%test_unit "every_enter_without_pushback, stop after leave" =
      async_unit_test (fun () ->
        let stop_ivar = Ivar.create () in
        let stop = Ivar.read stop_ivar in
        every_enter_without_pushback run_schedule ~start:after ~stop (fun ~enter:_ ~leave ->
          upon leave (fun _ -> Ivar.fill_if_empty stop_ivar ()));
        stop
        >>= fun () ->
        wait_for_deferred_errors ())

    let%test_unit "every_enter_without_pushback, continue after exceptions" =
      async_unit_test (fun () ->
        let enter_reader, enter_writer = Pipe.create () in
        let monitor = Monitor.create () in
        Monitor.detach monitor;
        within ~monitor (fun () ->
          every_enter_without_pushback run_schedule ~start:after ~continue_on_error:true
            (fun ~enter ~leave:_ ->
               Pipe.write_without_pushback enter_writer enter;
               failwith "fail"));
        Pipe.read_exactly enter_reader ~num_values:2
        >>| fun enters ->
        [%test_result: Time.t read_exactly]
          enters
          ~expect:(`Exactly (Queue.of_list [ expected_enter ; expected_enter2 ])))
    ;;

    let%test_unit "every_enter_without_pushback, fail immediately" =
      async_unit_test (fun () ->
        let monitor0 = Monitor.current () in
        let enter_reader, enter_writer = Pipe.create () in
        let monitor = Monitor.create () in
        Monitor.detach monitor;
        within ~monitor (fun () ->
          every_enter_without_pushback run_schedule ~start:after ~continue_on_error:false
            (fun ~enter ~leave ->
               Pipe.write_without_pushback enter_writer enter;
               upon leave (fun _ -> within ~monitor:monitor0 (fun () -> assert false));
               failwith "fail"));
        Pipe.read_exactly enter_reader ~num_values:1
        >>= fun enters ->
        [%test_result: Time.t read_exactly]
          enters
          ~expect:(`Exactly (Queue.of_list [ expected_enter ]));
        upon (Pipe.read enter_reader) (fun _ -> assert false);
        wait_for_deferred_errors ())
    ;;

    let%test_unit "every_enter, pushback" =
      async_unit_test (fun  () ->
        let stop = Ivar.create () in
        every_enter run_schedule
          ~start:after
          ~stop:(Ivar.read stop)
          ~continue_on_error:true
          ~on_pushback:(fun ~enter:_ ~leave:_ -> Ivar.fill_if_empty stop ())
          (fun ~enter:_ ~leave:_ -> Deferred.never ());
        choose [ choice (Clock.after (sec 1.)) (fun () -> `timeout)
               ; choice (Ivar.read stop) (fun () -> `stopped)
               ]
        >>| function
        | `stopped -> ()
        | `timeout -> assert false
      )
    ;;

    let%test_unit "every_enter, exn + continue_on_error => no pushback" =
      async_unit_test (fun  () ->
        let enter_count = ref 0 in
        let pushback_count = ref 0 in
        don't_wait_for (
          Monitor.handle_errors (fun () ->
            every_enter run_schedule
              ~start:after
              ~continue_on_error:true
              ~on_pushback:(fun ~enter:_ ~leave:_ ->
                Int.incr pushback_count
              )
              (fun ~enter:_ ~leave:_ ->
                 Int.incr enter_count;
                 failwith "blowing up on purpose"
              );
            Deferred.unit)
            (fun (_:exn) -> ()));
        Clock.after (sec 1.)
        >>= fun () ->
        [%test_result: int] ~expect:0 !pushback_count;
        [%test_pred: int] (fun c -> c >= 1) !enter_count;
        Deferred.unit
      )
    ;;

    let%test_unit "every_tag_change, exn + continue_on_error => no pushback" =
      async_unit_test (fun  () ->
        let enter_count = ref 0 in
        let pushback_count = ref 0 in
        don't_wait_for (
          Monitor.handle_errors (fun () ->
            every_tag_change run_schedule
              ~tag_equal:Float.equal
              ~start:after
              ~continue_on_error:true
              ~on_pushback:(fun ~tags:_ ~enter:_ ~leave:_ ->
                Int.incr pushback_count
              )
              (fun ~tags:_ ~enter:_ ~leave:_ ->
                 Int.incr enter_count;
                 failwith "blowing up on purpose"
              );
            Deferred.unit)
            (fun (_:exn) -> ()));
        Clock.after (sec 1.)
        >>= fun () ->
        [%test_result: int] ~expect:0 !pushback_count;
        [%test_pred: int] (fun c -> c >= 1) !enter_count;
        Deferred.unit
      )
    ;;

    let%test_unit "every_tag_change_without_pushback" =
      async_unit_test (fun () ->
        let enter_reader, enter_writer = Pipe.create () in
        let leave_reader, leave_writer = Pipe.create () in
        every_tag_change_without_pushback run_schedule ~start:after ~tag_equal:Float.equal
          (fun ~tags ~enter ~leave ->
             don't_wait_for
               (Pipe.write enter_writer (List.sort tags ~compare:Float.compare, enter)
                >>= fun () ->
                leave
                >>= fun leave_time ->
                Pipe.write leave_writer leave_time));
        Pipe.read_exactly enter_reader ~num_values:4
        >>= fun enters ->
        [%test_result: (float list * Time.t) read_exactly]
          enters
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ [1.0],     expected_enter
                  ; [2.0],     expected_enter2
                  ; [1.0;2.0], expected_enter3
                  ; [2.0],     expected_enter4
                  ]));
        Pipe.read_exactly leave_reader ~num_values:4
        >>| fun leaves ->
        [%test_result: Time.t read_exactly]
          leaves
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ expected_leave
                  ; expected_leave2
                  ; expected_leave3
                  ; expected_leave4
                  ])))
    ;;

    let%test_unit "every_tag_change, pushback" =
      async_unit_test (fun () ->
        let stop = Ivar.create () in
        every_tag_change run_schedule ~start:after ~stop:(Ivar.read stop) ~tag_equal:Float.equal
          ~on_pushback:(fun ~tags:_ ~enter:_ ~leave:_ -> Ivar.fill_if_empty stop ())
          (fun ~tags:_ ~enter:_ ~leave:_ -> Deferred.never ());
        choose [ choice (Clock.after (sec 1.)) (fun () -> `timeout)
               ; choice (Ivar.read stop) (fun () -> `stopped)
               ]
        >>| function
        | `timeout -> assert false
        | `stopped -> ())
    ;;

    let%test_unit "every_tag_change_without_pushback, enter in-range" =
      async_unit_test (fun () ->
        let enter_reader, enter_writer = Pipe.create () in
        let leave_reader, leave_writer = Pipe.create () in
        every_tag_change_without_pushback run_schedule ~start:mid ~tag_equal:Float.equal
          ~start_in_range_is_enter:true
          (fun ~tags ~enter ~leave ->
             don't_wait_for
               (Pipe.write enter_writer (List.sort tags ~compare:Float.compare, enter)
                >>= fun () ->
                leave
                >>= fun leave_time ->
                Pipe.write leave_writer leave_time));
        Pipe.read_exactly enter_reader ~num_values:2
        >>= fun enters ->
        [%test_result: (float list * Time.t) read_exactly]
          enters
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ [1.0], mid
                  ; [2.0], expected_enter2
                  ]));
        Pipe.read_exactly leave_reader ~num_values:2
        >>| fun leaves ->
        [%test_result: Time.t read_exactly]
          leaves
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ expected_leave
                  ; expected_leave2
                  ])))
    ;;

    let%test_unit "every_tag_change_without_pushback, don't enter in-range" =
      async_unit_test (fun () ->
        let enter_reader, enter_writer = Pipe.create () in
        let leave_reader, leave_writer = Pipe.create () in
        every_tag_change_without_pushback run_schedule ~start:mid ~tag_equal:Float.equal
          ~start_in_range_is_enter:false
          (fun ~tags ~enter ~leave ->
             don't_wait_for
               (Pipe.write enter_writer (List.sort tags ~compare:Float.compare, enter)
                >>= fun () ->
                leave
                >>= fun leave_time ->
                Pipe.write leave_writer leave_time));
        Pipe.read_exactly enter_reader ~num_values:1
        >>= fun enters ->
        [%test_result: (float list * Time.t) read_exactly]
          enters
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ [2.0], expected_enter2
                  ]));
        Pipe.read_exactly leave_reader ~num_values:1
        >>| fun leaves ->
        [%test_result: Time.t read_exactly]
          leaves
          ~expect:
            (`Exactly
               (Queue.of_list
                  [ expected_leave2
                  ])))
    ;;

    let%test_unit "every_enter, don't start too early" =
      async_unit_test (fun () ->
        let start = Time.add (Time.now ()) Time.Span.day in
        every_enter Always ~start
          ~on_pushback:(fun ~enter:_ ~leave:_ -> assert false)
          (fun ~enter:_ ~leave:_ -> assert false);
        wait_for_deferred_errors ())

    let%test_unit "every_tag_change_without_pushback, don't start too early" =
      async_unit_test (fun () ->
        let start = Time.add (Time.now ()) Time.Span.day in
        every_tag_change_without_pushback Always ~start ~tag_equal:Pervasives.(=)
          (fun ~tags:_ ~enter:_ ~leave:_ -> assert false);
        wait_for_deferred_errors ())

    let%test_unit "next_enter" =
      async_unit_test (fun () ->
        let next = next_event next_schedule ~event:`Enter ~stop:(Deferred.never ())
                     ~after () in
        Clock.with_timeout (sec 1.) next
        >>| function
        | `Timeout     -> failwith "next_enter timed out"
        | `Result time -> assert (time = expected_enter))
    ;;

    let%test_unit "next_leave" =
      async_unit_test (fun () ->
        let next = next_event next_schedule ~event:`Leave
                     ~stop:(Deferred.never ()) ~after () in
        Clock.with_timeout (sec 1.) next
        >>| function
        | `Timeout     -> failwith "next_enter timed out"
        | `Result time -> assert (time = expected_leave))
    ;;

    let next_representable_time t =
      Time.to_span_since_epoch t
      |> Time.Span.to_sec
      |> Float.one_ulp `Up
      |> Time.Span.of_sec
      |> Time.of_span_since_epoch
    ;;

    let zone = Time.Zone.utc
    let date = Date.create_exn
    let time = Time.of_date_ofday ~zone
    let ofday = Time.Ofday.create

    let%test_unit "to_pipe" =
      async_unit_test (fun () ->
        let start date = time date (ofday ~hr:9 ())  in
        let stop date = time date (ofday ~hr:16 ()) in
        let expected =
          [ `Enter (start (date ~y:2014 ~m:Nov ~d:17), [ `Start ])
          ; `Leave (next_representable_time (start (date ~y:2014 ~m:Nov ~d:17)))
          ; `Enter (stop (date ~y:2014 ~m:Nov ~d:17), [ `Stop ])
          ; `Leave (next_representable_time (stop (date ~y:2014 ~m:Nov ~d:17)))
          ; `Enter (start (date ~y:2014 ~m:Nov ~d:18), [ `Start ])
          ; `Leave (next_representable_time (start (date ~y:2014 ~m:Nov ~d:18)))
          ; `Enter (stop (date ~y:2014 ~m:Nov ~d:18), [ `Stop ])
          ; `Leave (next_representable_time (stop (date ~y:2014 ~m:Nov ~d:18)))
          ; `Enter (start (date ~y:2014 ~m:Nov ~d:19), [ `Start ])
          ; `Leave (next_representable_time (start (date ~y:2014 ~m:Nov ~d:19)))
          ; `Enter (stop (date ~y:2014 ~m:Nov ~d:19), [ `Stop ])
          ; `Leave (next_representable_time (stop (date ~y:2014 ~m:Nov ~d:19)))
          ; `Enter (start (date ~y:2014 ~m:Nov ~d:20), [ `Start ])
          ; `Leave (next_representable_time (start (date ~y:2014 ~m:Nov ~d:20)))
          ; `Enter (stop (date ~y:2014 ~m:Nov ~d:20), [ `Stop ])
          ; `Leave (next_representable_time (stop (date ~y:2014 ~m:Nov ~d:20)))
          ; `Enter (start (date ~y:2014 ~m:Nov ~d:21), [ `Start ])
          ; `Leave (next_representable_time (start (date ~y:2014 ~m:Nov ~d:21)))
          ; `Enter (stop (date ~y:2014 ~m:Nov ~d:21), [ `Stop ])
          ; `Leave (next_representable_time (stop (date ~y:2014 ~m:Nov ~d:21)))
          ]
        in
        let weekdays = Weekdays [ Mon; Tue; Wed; Thu; Fri ] in
        let schedule =
          In_zone
            (Time.Zone.utc
            , Or
                [ Tag
                    (`Start
                    , And
                        [ weekdays
                        ; At [ Time.Ofday.create ~hr:9 () ] ])
                ; Tag
                    (`Stop
                    , And
                        [ weekdays
                        ; At [ Time.Ofday.create ~hr:16 () ] ])
                ])
        in
        let start_time = Time.of_string "2014-11-15 15:54:00Z" in
        let ( `Started_in_range (_, p)
            | `Started_out_of_range p ) =
          to_pipe schedule ~start_time ~emit:Transitions ()
        in
        Pipe.read_exactly p ~num_values:20
        >>| function
        | `Eof
        | `Fewer _   -> failwith "pipe unexpectedly closed"
        | `Exactly q ->
          [%test_result:
            [ `Enter of Time.t * [ `Start | `Stop ] list
            | `Leave of Time.t ] list]
            ~expect:expected
            (Queue.to_list q))
    ;;

    (* everything below this point exists to fulfill the type signature of Schedule to
       remind us to consider adding a unit test whenever we add a new function. *)
    type zoned = Core_schedule.zoned = Zoned
    type unzoned = Core_schedule.unzoned = Unzoned

    module Inclusive_exclusive = struct
      type t = Core_schedule.Inclusive_exclusive.t = Inclusive | Exclusive
      let compare = Inclusive_exclusive.compare
    end

    type ('a, 'b) t = ('a, 'b) Core_schedule.t =
      | In_zone       : Time.Zone.t * (unzoned, 'b) t                     -> (zoned, 'b) t
      | Tag           : 'b * ('a, 'b) t                                   -> ('a, 'b) t
      | And           : ('a, 'b) t list                                   -> ('a, 'b) t
      | Or            : ('a, 'b) t list                                   -> ('a, 'b) t
      | Not           : ('a, 'b) t                                        -> ('a, 'b) t
      | If_then_else  : (('a, 'b) t * ('a, 'b) t * ('a, 'b) t)            -> ('a, 'b) t
      | Shift         : Time.Span.t * ('a, 'b) t                          -> ('a, 'b) t
      | Between       : (Inclusive_exclusive.t * Time.Ofday.t)
                        * (Inclusive_exclusive.t * Time.Ofday.t)           -> (unzoned, 'b) t
      | Zoned_between : (Inclusive_exclusive.t * Time.Zone.t * Time.Ofday.t)
                        * (Inclusive_exclusive.t * Time.Zone.t * Time.Ofday.t) -> (zoned, 'b) t
      | At            : Time.Ofday.t list                                 -> (unzoned, 'b) t
      | Secs          : int list                                          -> (unzoned, 'b) t
      | Mins          : int list                                          -> (unzoned, 'b) t
      | Hours         : int list                                          -> (unzoned, 'b) t
      | Weekdays      : Day_of_week.t list                                -> (unzoned, 'b) t
      | Days          : int list                                          -> (unzoned, 'b) t
      | Weeks         : int list                                          -> (unzoned, 'b) t
      | Months        : Month.t list                                      -> (unzoned, 'b) t
      | On            : Date.t list                                       -> (unzoned, 'b) t
      | Before        : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
      | After         : (Inclusive_exclusive.t * (Date.t * Time.Ofday.t)) -> (unzoned, 'b) t
      | Always        : ('a, 'b) t
      | Never         : ('a, 'b) t

    type 'b zoned_t = 'b Core_schedule.zoned_t

    module Event = struct include Event end

    type ('tag, 'a) emit = ('tag, 'a) Core_schedule.emit =
      | Transitions
        : ('tag, [ Event.no_change | 'tag Event.transition ]) emit
      | Transitions_and_tag_changes
        :  ('tag -> 'tag -> bool)
        -> ('tag, [ Event.no_change | 'tag Event.transition | 'tag Event.tag_change]) emit

    type ('tag, 'output) pipe_emit = ('tag, 'output) Async_schedule.pipe_emit =
      | Transitions
        : ('tag, 'tag Event.transition) pipe_emit
      | Transitions_and_tag_changes
        :  ('tag -> 'tag -> bool)
        -> ('tag, [ 'tag Event.transition | 'tag Event.tag_change ]) pipe_emit

    let to_pipe = to_pipe
    let next_event = next_event
    let next_enter_between = next_enter_between
    let next_leave_between = next_leave_between
    let to_endless_sequence = to_endless_sequence
    let map_tags = map_tags
    let fold_tags = fold_tags
    let all_tags = all_tags
    let tags = tags
    let includes = includes
    let compare_zoned = compare_zoned
    let compare_unzoned = compare_unzoned
    let compare = compare
    let to_string_zoned = to_string_zoned
    let sexp_of_zoned_t = sexp_of_zoned_t
    type nonrec 'a every_enter_callback = 'a every_enter_callback
    let every_enter = every_enter
    let every_tag_change_without_pushback = every_tag_change_without_pushback
    let every_enter_without_pushback = every_enter_without_pushback
    let every_tag_change = every_tag_change

    module Stable = struct
      module V4_deprecated = struct
        include Core_schedule.Stable.V4_deprecated
      end

      module V5 = struct
        include Core_schedule.Stable.V5
      end
    end
  end
           : module type of Async_schedule))
