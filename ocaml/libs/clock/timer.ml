type t = {elapsed: Mtime_clock.counter; duration: Mtime.Span.t}

type countdown = Remaining of Mtime.Span.t | Expired of Mtime.Span.t

let span_is_shorter a ~than:b = Mtime.Span.compare a b < 0

let span_is_longer a ~than:b = Mtime.Span.compare a b > 0

let start ~duration = {elapsed= Mtime_clock.counter (); duration}

let duration {duration; _} = duration

let elapsed t = Mtime_clock.count t.elapsed

let remaining t =
  let elapsed = Mtime_clock.count t.elapsed in
  let difference = Mtime.Span.abs_diff elapsed t.duration in
  if span_is_shorter elapsed ~than:t.duration then
    Remaining difference
  else
    Expired difference

let has_expired t =
  let elapsed = Mtime_clock.count t.elapsed in
  not (span_is_shorter elapsed ~than:t.duration)

let shorten_by dur t =
  let duration =
    if span_is_longer dur ~than:t.duration then
      Mtime.Span.zero
    else
      Mtime.Span.abs_diff dur t.duration
  in
  {t with duration}

let extend_by dur t =
  let duration = Mtime.Span.add dur t.duration in
  {t with duration}

let pp =
  let open Fmt in
  record
    [
      field "elapsed" elapsed Mtime.Span.pp
    ; field "duration" duration Mtime.Span.pp
    ]

(* Conversion functions *)

(* Rounding errors when there are more than 2^44 seconds, or about ~55 years.
   *)
let span_to_s span = Mtime.Span.to_float_ns span |> fun ns -> ns *. 1e-9

let s_to_span s = Mtime.Span.of_float_ns (s *. 1e9 |> Float.round)
