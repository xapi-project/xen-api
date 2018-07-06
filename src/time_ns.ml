[%%import "config.h"]
open! Import
open Std_internal

let arch_sixtyfour = Sys.word_size = 64
let round_nearest = Float.int63_round_nearest_exn

let float x = Int63.to_float x

(* This signature constraint is semi-temporary and serves to make the implementation more
   type-safe (so the compiler can help us more).  It would go away if we broke the
   implementation into multiple files. *)
module Span : sig
  (* Note that the [sexp] below is implemented only for some debug text later in this
     module. It is not exposed in the mli. *)
  type t = Int63.t [@@deriving hash, typerep, sexp]

  include Time_ns_intf.Span with type t := t
end = struct
  (* [Span] is basically a [Int63].  It even silently ignores overflow. *)
  module T = struct
    type t = Int63.t (* nanoseconds *)
    [@@deriving hash, bin_io, typerep]

    let compare = Int63.compare
    let equal   = Int63.equal
    let zero    = Int63.zero

    include (Int63 : Comparable.Infix with type t := t)
  end

  include T

  module Parts = struct
    type t =
      { sign : Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    [@@deriving sexp]

    let compare = Poly.compare
  end

  let nanosecond  = Int63.of_int 1
  let microsecond = Int63.(of_int 1000 * nanosecond)
  let millisecond = Int63.(of_int 1000 * microsecond)
  let second      = Int63.(of_int 1000 * millisecond)
  let minute      = Int63.(of_int 60 * second)
  let hour        = Int63.(of_int 60 * minute)
  let day         = Int63.(of_int 24 * hour)

  (* Beyond [min_value..max_value], not every microsecond can be represented as a [float]
     number of seconds.  (In fact, it is around 135y, but we leave a small margin.)

     In the presence of silently ignored overflow, note that [t] is not actually bound to
     stay between these limits. *)
  let max_value = Int63.(of_int 135 * of_int 365 * day)
  let min_value = Int63.neg max_value

  let create
        ?sign:(sign_  = Sign.Pos) (* rebind so not shadowed by [open Int63] below *)
        ?day:(days    = 0)
        ?(hr          = 0)
        ?min:(minutes = 0)
        ?(sec         = 0)
        ?(ms          = 0)
        ?(us          = 0)
        ?(ns          = 0)
        () =
    let open Int63 in
    let t =
      of_int days    * day
      + of_int hr      * hour
      + of_int minutes * minute
      + of_int sec     * second
      + of_int ms      * millisecond
      + of_int us      * microsecond
      + of_int ns      * nanosecond
    in
    match sign_ with Neg -> neg t | Pos | Zero -> t
  ;;

  let to_parts t =
    let open Int63 in
    let mag = abs t in
    { Parts.
      sign = if t < zero then Neg else if t > zero then Pos else Zero
    ; hr = to_int_exn (mag / hour)
    ; min = to_int_exn ((rem mag hour) / minute)
    ; sec = to_int_exn ((rem mag minute) / second)
    ; ms = to_int_exn ((rem mag second) / millisecond)
    ; us = to_int_exn ((rem mag millisecond) / microsecond)
    ; ns = to_int_exn ((rem mag microsecond) / nanosecond)
    }
  ;;

  let of_parts { Parts. sign; hr; min; sec; ms; us; ns } =
    create ~sign ~hr ~min ~sec ~ms ~us ~ns ()
  ;;

  let of_ns       f = round_nearest f
  let of_int63_ns i = i
  let of_int_us   i = Int63.(of_int i * microsecond)
  let of_int_ms   i = Int63.(of_int i * millisecond)
  let of_int_sec  i = Int63.(of_int i * second)
  let of_us       f = round_nearest (f *. float microsecond)
  let of_ms       f = round_nearest (f *. float millisecond)
  let of_sec      f = round_nearest (f *. float second)
  let of_min      f = round_nearest (f *. float minute)
  let of_hr       f = round_nearest (f *. float hour)
  let of_day      f = round_nearest (f *. float day)

  let of_sec_with_microsecond_precision sec =
    let us = round_nearest (sec *. 1e6) in
    of_int63_ns Int63.(us * of_int 1000)
  ;;

  let to_ns       t = float t
  let to_int63_ns t =       t
  let to_us       t = float t /. float microsecond
  let to_ms       t = float t /. float millisecond
  let to_sec      t = float t /. float second
  let to_min      t = float t /. float minute
  let to_hr       t = float t /. float hour
  let to_day      t = float t /. float day
  let to_int_us   t = Int63.(to_int_exn (t / microsecond))
  let to_int_ms   t = Int63.(to_int_exn (t / millisecond))
  let to_int_sec  t = Int63.(to_int_exn (t / second))

  let%test _ [@tags "64-bits-only"] = Int.(>) (to_int_sec Int63.max_value) 0 (* and doesn't raise *)

  let of_int_ns =
    if arch_sixtyfour
    then fun i -> of_int63_ns (Int63.of_int i)
    else fun _ -> failwith "Time_ns.Span.of_int_ns: unsupported on 32bit machines"

  let to_int_ns =
    if arch_sixtyfour
    then fun t -> Int63.to_int_exn (to_int63_ns t)
    else fun _ -> failwith "Time_ns.Span.to_int_ns: unsupported on 32bit machines"

  let (+)         t u = Int63.(+) t u
  let (-)         t u = Int63.(-) t u
  let abs             = Int63.abs
  let neg             = Int63.neg
  let scale       t f = round_nearest (float t *. f)
  let scale_int63 t i = Int63.( * ) t i
  let scale_int   t i = scale_int63 t (Int63.of_int i)
  let div             = Int63.( /% )
  let (/)         t f = round_nearest (float t /. f)
  let (//)            = Int63.(//)
  let max             = Int63.max
  let min             = Int63.min

  let to_proportional_float t = Int63.to_float t

  let%test_module "overflow silently" =
    (module struct
      let doesn't_raise = Fn.non Exn.does_raise

      let%test "+ range up"   = doesn't_raise (fun () -> max_value +     nanosecond)
      let%test "+ range down" = doesn't_raise (fun () -> min_value + neg nanosecond)
      let%test "+ overflow"   = doesn't_raise (fun () -> max_value +     max_value)
      let%test "+ underflow"  = doesn't_raise (fun () -> min_value +     min_value)
      let%test "- range down" = doesn't_raise (fun () -> min_value -     nanosecond)
      let%test "- range up"   = doesn't_raise (fun () -> max_value - neg nanosecond)
      let%test "- underflow"  = doesn't_raise (fun () -> min_value -     max_value)
      let%test "- overflow"   = doesn't_raise (fun () -> max_value -     min_value)

      let%test_module "intermediate ( * )" =
        (module struct
          let wrap_days =
            let margin_ns = Int63.(-) min_value Int63.min_value in
            Int63.(max_value / day + one + margin_ns / day + one)
          ;;
          let%test_unit "wrap_days" =
            [%test_pred: Int63.t]
              (Int63.between ~low:min_value ~high:(Int63.neg nanosecond))
              Int63.(wrap_days * day)
          ;;
          let wrap_days_int = Int63.to_int_exn wrap_days
          let%test "scale_int63" = doesn't_raise (fun () -> scale_int63 day wrap_days)
          let%test "scale_int"   = doesn't_raise (fun () -> scale_int   day wrap_days_int)
          let%test "create"      = doesn't_raise (fun () -> create ()  ~day:wrap_days_int)
        end)
      ;;
    end)
  ;;

  (* The conversion code here is largely copied from [Core_kernel.Span] and edited to remove
     some of the stable versioning details. This makes it a little easier to think about
     and we get a compatible sexp format that can subsequently live in [Core_kernel] and
     [Async_kernel] *)
  module Alternate_sexp = struct
    type nonrec t = t

    let of_string (s:string) =
      try
        begin match s with
        | "" -> failwith "empty string"
        | _  ->
          let float n =
            match (String.drop_suffix s n) with
            | "" -> failwith "no number given"
            | s  ->
              let v = Float.of_string s in
              Validate.maybe_raise (Float.validate_ordinary v);
              v
          in
          let len = String.length s in
          match s.[Int.(-) len 1] with
          | 's' ->
            if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'm'
            then of_ms (float 2)
            else if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'u'
            then of_us (float 2)
            else if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'n'
            then of_ns (float 2)
            else of_sec (float 1)
          | 'm' -> of_min (float 1)
          | 'h' -> of_hr (float 1)
          | 'd' -> of_day (float 1)
          | _ ->
            failwith "Time spans must end in ns, us, ms, s, m, h, or d."
        end
      with exn ->
        failwithf "Span.of_string could not parse '%s': %s" s (Exn.to_string exn) ()

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom x ->
        (try of_string x
         with exn -> of_sexp_error (Exn.to_string exn) sexp)
      | Sexp.List _ ->
        of_sexp_error "Time_ns.Span.t_of_sexp sexp must be an Atom" sexp


    let to_string (t:T.t) =
      let string suffix float =
        (* This is the same float-to-string conversion used in [Float.sexp_of_t].  It's
           like [Float.to_string], but may leave off trailing period. *)
        !Sexplib.Conv.default_string_of_float float ^ suffix
      in
      let abs_t = abs t in
      if abs_t < microsecond then string "ns" (to_ns t)
      else if abs_t < millisecond then string "us" (to_us t)
      else if abs_t < second then string "ms" (to_ms t)
      else if abs_t < minute then string "s" (to_sec t)
      else if abs_t < hour then string "m" (to_min t)
      else if abs_t < day then string "h" (to_hr t)
      else string "d" (to_day t)

    let sexp_of_t t = Sexp.Atom (to_string t)
  end

  let sexp_of_t = Alternate_sexp.sexp_of_t
  let t_of_sexp = Alternate_sexp.t_of_sexp

  include Comparable.Validate_with_zero(struct
      include T
      let sexp_of_t = Alternate_sexp.sexp_of_t
      let t_of_sexp = Alternate_sexp.t_of_sexp
    end)

  let of_unit_of_time u =
    match (u : Unit_of_time.t) with
    | Nanosecond  -> nanosecond
    | Microsecond -> microsecond
    | Millisecond -> millisecond
    | Second      -> second
    | Minute      -> minute
    | Hour        -> hour
    | Day         -> day
  ;;

  let to_unit_of_time t : Unit_of_time.t =
    let abs_t = abs t in
    if abs_t >= day         then Day         else
    if abs_t >= hour        then Hour        else
    if abs_t >= minute      then Minute      else
    if abs_t >= second      then Second      else
    if abs_t >= millisecond then Millisecond else
    if abs_t >= microsecond then Microsecond else
      Nanosecond
  ;;

  let%test_module _ = (module struct
    let ( * ) = Int63.( * )
    let of_int = Int63.of_int

    let round_trip t = [%test_result: t] (of_parts (to_parts t)) ~expect:t
    let eq t expect =
      [%test_result: t] t ~expect;
      [%test_result: Parts.t] (to_parts t) ~expect:(to_parts expect);
      round_trip t

    let%test_unit _ = eq (create ~us:2            ()) (of_int 2    * microsecond)
    let%test_unit _ = eq (create ~min:3           ()) (of_int 3    * minute)
    let%test_unit _ = eq (create ~ms:4            ()) (of_int 4    * millisecond)
    let%test_unit _ = eq (create ~sec:5           ()) (of_int 5    * second)
    let%test_unit _ = eq (create ~hr:6            ()) (of_int 6    * hour)
    let%test_unit _ = eq (create ~day:7           ()) (of_int 7    * day)
    let%test_unit _ = eq (create ~us:8 ~sign:Neg  ()) (of_int (-8) * microsecond)
    let%test_unit _ = eq (create ~ms:9 ~sign:Zero ()) (of_int 9    * millisecond)
    let%test_unit _ =
      eq (create ~us:3 ~ns:242 () |> to_sec |> of_sec_with_microsecond_precision)
        (of_int 3 * microsecond)
    let%test_unit _ =
      for _ = 1 to 1_000_000 do
        let t =
          (Int63.of_int64_exn (Random.int64 (Int63.to_int64 max_value)))
          + if Random.bool () then zero else min_value
        in
        round_trip t
      done

    let round_trip parts =
      [%test_result: Parts.t] (to_parts (of_parts parts)) ~expect:parts
    let eq parts expect =
      [%test_result: Parts.t] parts ~expect;
      [%test_result: t] (of_parts parts) ~expect:(of_parts expect);
      round_trip parts

    let%test_unit _ =
      eq (to_parts (create ~sign:Neg ~hr:2 ~min:3 ~sec:4 ~ms:5 ~us:6 ~ns:7 ()))
        { Parts. sign = Neg; hr = 2; min = 3; sec = 4; ms = 5; us = 6; ns = 7 }
    let%test_unit _ = round_trip (to_parts (create ~hr:25 ()))
    let%test_unit _ =
      let hr =
        match Word_size.word_size with
        | W32 -> Int.max_value
        | W64 -> Int64.to_int_exn 2217989799822798757L
      in
      round_trip (to_parts (create ~hr ()))
  end)

  (* Functions required by [Robustly_comparable]: allows for [epsilon] granularity.

     A microsecond is a reasonable granularity because there is very little network
     activity that can be measured to sub-microsecond resolution. *)
  let epsilon = microsecond
  let (>=.) t u = t >= Int63.(u - epsilon)
  let (<=.) t u = t <= Int63.(u + epsilon)
  let (=.) t u = Int63.(abs (t - u)) <= epsilon
  let (>.) t u = t > Int63.(u + epsilon)
  let (<.) t u = t < Int63.(u - epsilon)
  let (<>.) t u = Int63.(abs (t - u)) > epsilon
  let robustly_compare t u = if t <. u then -1 else if t >. u then 1 else 0

  [%%ifdef JSC_ARCH_SIXTYFOUR]
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero" [@@noalloc]
  [%%else]
  external since_unix_epoch_or_zero : unit -> t
    = "core_kernel_time_ns_gettime_or_zero"
  [%%endif]

  [%%ifdef JSC_POSIX_TIMERS]
  let [@inline never] gettime_failed () = failwith "clock_gettime(CLOCK_REALTIME) failed"
  [%%else]
  let [@inline never] gettime_failed () = failwith "gettimeofday failed"
  [%%endif]

  let since_unix_epoch () =
    let t = since_unix_epoch_or_zero () in
    if t <> zero then t else gettime_failed ()
  ;;

  let random ?state () =
    Int63.random ?state (max_value     + Int63.one) -
    Int63.random ?state (neg min_value + Int63.one)
  ;;

  let%test_unit "random smoke" =
    let state = Random.State.make [| |] in
    for _ = 1 to 1000 do
      ignore (random ~state () : t)
    done
  ;;
end

type t = Span.t (* since the Unix epoch (1970-01-01 00:00:00 UTC) *)
[@@deriving bin_io, compare, hash, typerep]

include (Span : Comparable.Infix with type t := t)

let now = Span.since_unix_epoch

let equal = Span.equal

let min_value = Span.min_value
let max_value = Span.max_value

let epoch        = Span.zero
let add          = Span.(+)
let sub          = Span.(-)
let diff         = Span.(-)
let abs_diff t u = Span.abs (diff t u)
let max          = Span.max
let min          = Span.min

let to_span_since_epoch t = t
let of_span_since_epoch s = s

let to_int63_ns_since_epoch t : Int63.t = Span.to_int63_ns (to_span_since_epoch t)
let of_int63_ns_since_epoch i = of_span_since_epoch (Span.of_int63_ns i)

let to_int_ns_since_epoch =
  if arch_sixtyfour
  then fun t -> Int63.to_int_exn (to_int63_ns_since_epoch t)
  else fun _ -> failwith "Time_ns.to_int_ns_since_epoch: unsupported on 32bit machines"

let of_int_ns_since_epoch =
  if arch_sixtyfour
  then fun i -> of_int63_ns_since_epoch (Int63.of_int i)
  else fun _ -> failwith "Time_ns.of_int_ns_since_epoch: unsupported on 32bit machines"

let [@inline never] raise_next_multiple_got_nonpositive_interval interval =
  failwiths "Time_ns.next_multiple got nonpositive interval" interval [%sexp_of: Span.t]
;;

let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
  if Span.(<=) interval Span.zero
  then raise_next_multiple_got_nonpositive_interval interval;
  let base_to_after = diff after base in
  if Span.(<) base_to_after Span.zero
  then base  (* [after < base], choose [k = 0]. *)
  else begin
    let next =
      add base (Span.scale_int63 interval (Span.div base_to_after interval))
    in
    if next > after || (can_equal_after && next = after)
    then next
    else add next interval
  end
;;

let random ?state () = Span.random ?state ()

let%test_unit "random smoke" =
  let state = Random.State.make [| |] in
  for _ = 1 to 1000 do
    ignore (random ~state () : t)
  done
;;

module Utc : sig
  val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t
  val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
end = struct
  (* a recreation of the system call gmtime specialized to the fields we need that also
     doesn't rely on Unix. *)
  let to_date_and_span_since_start_of_day t =
    let open Int63.O in
    let (!<) i = Int63.of_int_exn i in
    let (!>) t = Int63.to_int_exn t in
    let ns_since_epoch  = to_int63_ns_since_epoch t   in
    let ns_per_day      = !<86_400 * !<1_000_000_000  in
    let approx_days_from_epoch = ns_since_epoch / ns_per_day in
    let days_from_epoch =
      if t < !<0 && approx_days_from_epoch * ns_per_day <> ns_since_epoch
      then approx_days_from_epoch - !<1
      else approx_days_from_epoch
    in
    let ns_since_start_of_day = ns_since_epoch - (ns_per_day * days_from_epoch) in
    let date =
      Date0.Days.add_days Date0.Days.unix_epoch !>days_from_epoch
      |> Date0.Days.to_date
    in
    let span_since_start_of_day = Span.of_int63_ns ns_since_start_of_day in
    date, span_since_start_of_day
  ;;

  let of_date_and_span_since_start_of_day date span_since_start_of_day =
    assert (Span.( >= ) span_since_start_of_day Span.zero
            && Span.( < ) span_since_start_of_day Span.day);
    let days_from_epoch =
      Date0.Days.diff (Date0.Days.of_date date) Date0.Days.unix_epoch
    in
    let span_in_days_since_epoch = Span.scale_int Span.day days_from_epoch in
    let span_since_epoch = Span.( + ) span_in_days_since_epoch span_since_start_of_day in
    of_span_since_epoch span_since_epoch
  ;;
end

module Alternate_sexp = struct
  type nonrec t = t

  module Ofday_as_span = struct
    open Int.O

    let seconds_to_string seconds_span =
      let seconds = Span.to_int_sec seconds_span in
      let h = seconds / 3600 in
      let m = (seconds / 60) % 60 in
      let s = seconds % 60 in
      sprintf "%02d:%02d:%02d" h m s

    let two_digit_of_string string =
      assert (String.length string = 2
              && String.for_all string ~f:Char.is_digit);
      Int.of_string string

    let seconds_of_string seconds_string =
      match String.split seconds_string ~on:':' with
      | [ h_string ; m_string ; s_string ] ->
        let h = two_digit_of_string h_string in
        let m = two_digit_of_string m_string in
        let s = two_digit_of_string s_string in
        Span.of_int_sec ((((h * 60) + m) * 60) + s)
      | _ -> assert false

    let ns_of_100_ms = 100_000_000
    let ns_of_10_ms  =  10_000_000
    let ns_of_1_ms   =   1_000_000
    let ns_of_100_us =     100_000
    let ns_of_10_us  =      10_000
    let ns_of_1_us   =       1_000
    let ns_of_100_ns =         100
    let ns_of_10_ns  =          10
    let ns_of_1_ns   =           1

    let sub_second_to_string sub_second_span =
      let open Int.O in
      let ns = Span.to_int63_ns sub_second_span |> Int63.to_int_exn in
      if ns = 0
      then ""
      else begin
        if ns % ns_of_100_ms = 0 then sprintf ".%01d" (ns / ns_of_100_ms) else
        if ns % ns_of_10_ms  = 0 then sprintf ".%02d" (ns / ns_of_10_ms)  else
        if ns % ns_of_1_ms   = 0 then sprintf ".%03d" (ns / ns_of_1_ms)   else
        if ns % ns_of_100_us = 0 then sprintf ".%04d" (ns / ns_of_100_us) else
        if ns % ns_of_10_us  = 0 then sprintf ".%05d" (ns / ns_of_10_us)  else
        if ns % ns_of_1_us   = 0 then sprintf ".%06d" (ns / ns_of_1_us)   else
        if ns % ns_of_100_ns = 0 then sprintf ".%07d" (ns / ns_of_100_ns) else
        if ns % ns_of_10_ns  = 0 then sprintf ".%08d" (ns / ns_of_10_ns)  else
          sprintf ".%09d" ns
      end

    let sub_second_of_string string =
      if String.is_empty string
      then Span.zero
      else begin
        let digits = String.chop_prefix_exn string ~prefix:"." in
        assert (String.for_all digits ~f:Char.is_digit);
        let multiplier =
          match String.length digits with
          | 1 -> ns_of_100_ms
          | 2 -> ns_of_10_ms
          | 3 -> ns_of_1_ms
          | 4 -> ns_of_100_us
          | 5 -> ns_of_10_us
          | 6 -> ns_of_1_us
          | 7 -> ns_of_100_ns
          | 8 -> ns_of_10_ns
          | 9 -> ns_of_1_ns
          | _ -> assert false
        in
        Span.of_int63_ns (Int63.of_int (Int.of_string digits * multiplier))
      end

    let to_string span =
      assert (Span.( >= ) span Span.zero && Span.( < ) span Span.day);
      let seconds_span = span |> Span.to_int_sec |> Span.of_int_sec in
      let sub_second_span = Span.( - ) span seconds_span in
      seconds_to_string seconds_span ^ sub_second_to_string sub_second_span

    let of_string string =
      let len = String.length string in
      let prefix_len = 8 in (* "HH:MM:DD" *)
      let suffix_len = len - prefix_len in
      let seconds_string    = String.sub string ~pos:0          ~len:prefix_len in
      let sub_second_string = String.sub string ~pos:prefix_len ~len:suffix_len in
      let seconds_span    = seconds_of_string    seconds_string    in
      let sub_second_span = sub_second_of_string sub_second_string in
      Span.( + ) seconds_span sub_second_span
  end
  let to_string t =
    let date, span_since_start_of_day = Utc.to_date_and_span_since_start_of_day t in
    Date0.to_string date
    ^ " "
    ^ Ofday_as_span.to_string span_since_start_of_day
    ^ "Z"

  let of_string string =
    let date_string, ofday_string_with_zone =
      String.lsplit2_exn string ~on:' '
    in
    let ofday_string =
      String.chop_suffix_exn ofday_string_with_zone ~suffix:"Z"
    in
    let date = Date0.of_string date_string in
    let ofday = Ofday_as_span.of_string ofday_string in
    Utc.of_date_and_span_since_start_of_day date ofday

  include Sexpable.Of_stringable (struct
      type nonrec t = t
      let to_string = to_string
      let of_string = of_string
    end)

  module Stable = struct
    module V1 = struct
      (* see tests in lib/core_kernel/test/test_time_ns that ensure stability of this
         representation *)
      type nonrec t = t [@@deriving bin_io, compare, sexp]
    end
  end
end

let%test_module "next_multiple" =
  (module struct

    let test can_equal_after interval_ns after_ns =
      let base = epoch in
      let interval = Span.of_int63_ns (Int63.of_int interval_ns) in
      let after = of_int63_ns_since_epoch (Int63.of_int64_exn after_ns) in
      let result = next_multiple ~can_equal_after ~interval ~base ~after () in
      let lower_bound, upper_bound =
        let after_interval = add after interval in
        if can_equal_after
        then after, sub after_interval Span.nanosecond
        else add after Span.nanosecond, after_interval
      in
      if result < lower_bound || result > upper_bound then
        raise_s
          [%message
            "result out of bounds"
              (can_equal_after : bool)
              (interval        : Span.t)
              (base            : Alternate_sexp.t)
              (after           : Alternate_sexp.t)
              (result          : Alternate_sexp.t)
              (lower_bound     : Alternate_sexp.t)
              (upper_bound     : Alternate_sexp.t)]

    (* The below tests all failed in a previous implementation of [next_multiple], due to
       the use of floating point division rather than integer division. *)
    let%test_unit _ = test true         71 1666750235549516973L
    let%test_unit _ = test true       4398 1232807081547132235L
    let%test_unit _ = test false    702561 1233263206897519979L
    let%test_unit _ = test true         65 1813146216102385742L
    let%test_unit _ = test false      3376 1430224273339105389L
    let%test_unit _ = test true         25 1289744875932860592L
    let%test_unit _ = test true       2640 1289026286379471964L
    let%test_unit _ = test true    7062861 1582734990009845838L
    let%test_unit _ = test false  26123810 1509919129138733390L
    let%test_unit _ = test false      1076 1514456253942665045L
    let%test_unit _ = test false  47873597 1567592770350241609L
    let%test_unit _ = test true        147 1794365064173405211L
    let%test_unit _ = test true      37416 1703355717287748172L
    let%test_unit _ = test false        11 1627963384978464309L
    let%test_unit _ = test true     362857 1477941666514490237L
    let%test_unit _ = test true         74 1835379421104268809L
    let%test_unit _ = test false        95 1518869409078948499L
    let%test_unit _ = test false       152 1774086601023993329L
    let%test_unit _ = test true    2963474 1177784542849146405L
    let%test_unit _ = test false        30 1322037015396216447L
    let%test_unit _ = test true         25 1686952462277171285L
    let%test_unit _ = test false  77747994 1232530693599997021L
    let%test_unit _ = test true         39 1418422346766901525L
    let%test_unit _ = test true         20 1164906391254697606L
    let%test_unit _ = test false 492686918 1350478871564364650L
    let%test_unit _ = test false   5626939 1254841457643911520L
    let%test_unit _ = test true    1189147 1566503665916540724L
    let%test_unit _ = test false  97968678 1202922821174442071L
    let%test_unit _ = test false        20 1241457243504201837L
    let%test_unit _ = test true         99 1063228554057138547L
    let%test_unit _ = test true         73 1127965283765790199L
    let%test_unit _ = test true      92513 1423525486630794877L
    let%test_unit _ = test true  208946207 1512896538257529954L
    let%test_unit _ = test true        558 1304902428047905868L
    let%test_unit _ = test true         27 1454760325484042946L
    let%test_unit _ = test true    9511417 1224625971869008380L
    let%test_unit _ = test true    1112121 1486628785456556405L
    let%test_unit _ = test true         36 1226843097592112449L
    let%test_unit _ = test true         60 1299700152825201828L
    let%test_unit _ = test true     114032 1507179377240642938L
    let%test_unit _ = test true      27905 1379112115218849615L
    let%test_unit _ = test true  368860702 1318925554630500136L
    let%test_unit _ = test true       1636 1670399627434728314L
    let%test_unit _ = test false        27 1735798120119522136L
    let%test_unit _ = test true         14 1880325938102084694L
    let%test_unit _ = test true        155 1488215974636830525L
    let%test_unit _ = test true   14319914 1298824542911254370L
    let%test_unit _ = test true         94 1961333441294309841L
    let%test_unit _ = test true        321 1191344461619096942L
    let%test_unit _ = test true     706626 1179098309603309142L
    let%test_unit _ = test true          5 1180517413083401326L
    let%test_unit _ = test false  30523434 1471069600394063742L
    let%test_unit _ = test false 106875447 1789919428848820069L
    let%test_unit _ = test true         28 1013606888178097611L
    let%test_unit _ = test true       5178 1168893256723816286L
    let%test_unit _ = test true  146907740 1402240657577530746L
    let%test_unit _ = test true  127125596 1332881548503325287L
    let%test_unit _ = test true      46691 1526532096462597222L
    let%test_unit _ = test true       1603 1745157292595832416L
    let%test_unit _ = test true  141650492 1779813912846436672L
    let%test_unit _ = test false        20 1916060142837991511L
    let%test_unit _ = test false        27 1366845916494697310L
    let%test_unit _ = test true         61 1572832513125636690L
    let%test_unit _ = test false     11254 1301465801253970270L
    let%test_unit _ = test true    2817556 1220217790200673585L
    let%test_unit _ = test true   46399240 1371834303096963699L
    let%test_unit _ = test true   10280275 1199022106578060117L
    let%test_unit _ = test true     163667 1277585249492511350L
    let%test_unit _ = test true  441771131 1865810978398941565L
    let%test_unit _ = test true   22561070 1535418639166874210L
    let%test_unit _ = test true     677456 1356038574036607058L
    let%test_unit _ = test true        109 1102385187927169659L
    let%test_unit _ = test true        169 1592923082707947954L
    let%test_unit _ = test false   2150725 1769663126416348286L
    let%test_unit _ = test true        159 1051696934142612937L
    let%test_unit _ = test true         29 1844613926625333568L
    let%test_unit _ = test true         30 1361000119652263049L
    let%test_unit _ = test false     21058 1323116357214603127L
    let%test_unit _ = test true    1163794 1221604356987291502L
    let%test_unit _ = test false        30 1040042732593079852L
    let%test_unit _ = test false       106 1997585750801910583L
    let%test_unit _ = test true         78 1292467707712256145L
    let%test_unit _ = test false    882992 1557796972319309155L
    let%test_unit _ = test false      1821 1973683565069601822L
    let%test_unit _ = test false     34661 1737515124214074993L
    let%test_unit _ = test true      91661 1525765679206225703L
    let%test_unit _ = test false        55 1287656410542943084L
    let%test_unit _ = test true         25 1144756873630117512L
    let%test_unit _ = test true     121625 1374589039260879728L
    let%test_unit _ = test false        55 1970197704905173942L
    let%test_unit _ = test true         17 1013158341065700634L
    let%test_unit _ = test true       5176 1352936504880492660L
    let%test_unit _ = test true         12 1955810895023292883L
    let%test_unit _ = test true   67034967 1556142079069258330L
    let%test_unit _ = test true     690258 1241013338154557567L
    let%test_unit _ = test false   5606142 1356689387566170970L
    let%test_unit _ = test true        548 1613807159903275820L
    let%test_unit _ = test true         13 1425941806049471918L
    let%test_unit _ = test false 155572024 1398827221896378979L
    let%test_unit _ = test true  938925403 1550277848520025471L
    let%test_unit _ = test false  13058335 1306567871862304618L
    let%test_unit _ = test true          2 1997152439817382933L
    let%test_unit _ = test true  131456077 1809241097498435420L
    let%test_unit _ = test true          5 1531223674910420761L
    let%test_unit _ = test false      1125 1175905228832358761L
    let%test_unit _ = test true        350 1573261556955534963L
    let%test_unit _ = test false        21 1529314545697532312L
    let%test_unit _ = test false     11816 1222083468556908088L
    let%test_unit _ = test true      86085 1436391155125371248L
    let%test_unit _ = test true   75063667 1395675403046737786L
    let%test_unit _ = test false        67 1765632860861960357L
    let%test_unit _ = test false    184086 1232986716459688821L
    let%test_unit _ = test true         53 1643034916467763402L
    let%test_unit _ = test true        164 1931973285029689763L
    let%test_unit _ = test true         10 1317304422397637720L
    let%test_unit _ = test true      12566 1421417764422298993L
    let%test_unit _ = test true  122903121 1389456412090860886L
    let%test_unit _ = test false   3831308 1617363073756443917L
    let%test_unit _ = test true       2274 1256309428080267889L
    let%test_unit _ = test true         69 1975893988922224788L
    let%test_unit _ = test true  460408083 1956390486383825465L
    let%test_unit _ = test true         20 1294502403828905377L
    let%test_unit _ = test true      75279 1210517500455430679L
    let%test_unit _ = test false       335 1184433858378833746L
    let%test_unit _ = test false     94523 1420732229891051641L
    let%test_unit _ = test false        16 1310464979299616987L
    let%test_unit _ = test true       5886 1602668327390189086L
    let%test_unit _ = test false      9584 1532134444641007990L
    let%test_unit _ = test true         17 1362463965931411147L
    let%test_unit _ = test false         2 1693027090042722358L
    let%test_unit _ = test false 228135731 1462077890315132778L
    let%test_unit _ = test false        11 1018644923234572949L
    let%test_unit _ = test false    132723 1582399817588675962L
    let%test_unit _ = test false      3667 1506604922540283994L
    let%test_unit _ = test true  265541944 1695560402922008138L
    let%test_unit _ = test true        310 1875190738574556027L
    let%test_unit _ = test true    8570918 1184809728498232683L
    let%test_unit _ = test false  16536379 1490415593503829866L
    let%test_unit _ = test false  32222516 1519021258420540539L
    let%test_unit _ = test true  152467451 1255624172539661165L
    let%test_unit _ = test true         13 1803425272409148050L
    let%test_unit _ = test true         26 1021777264383583552L
    let%test_unit _ = test true         11 1400486869768403422L
    let%test_unit _ = test true     229637 1410589173350489612L
    let%test_unit _ = test true         32 1960302290555348647L
    let%test_unit _ = test false 349881185 1831970413297175407L
    let%test_unit _ = test false  35457345 1967569813691929674L
    let%test_unit _ = test false        16 1556051447243676249L
    let%test_unit _ = test false 302933078 1816140399596962652L
    let%test_unit _ = test true    3609444 1802393395129668217L

  end)

module Stable = struct
  module Alternate_sexp = Alternate_sexp.Stable
end
