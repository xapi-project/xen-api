(*
 * Copyright (c) Cloud Software Group, Inc
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

let string_of_severity =
  let open Opentelemetry_proto.Logs in
  function
  | Severity_number_unspecified
  | Severity_number_trace
  | Severity_number_trace2
  | Severity_number_trace3
  | Severity_number_trace4 ->
      (Logs.Debug, Some "TRACE")
  | Severity_number_debug
  | Severity_number_debug2
  | Severity_number_debug3
  | Severity_number_debug4 ->
      (Logs.Debug, None)
  | Severity_number_info ->
      ( Logs.App
      , None
        (* matches the reverse mapping in opentelemetry.logs for Logs.App *)
      )
  | Severity_number_info2 | Severity_number_info3 | Severity_number_info4 ->
      (Logs.Info, None)
  | Severity_number_warn
  | Severity_number_warn2
  | Severity_number_warn3
  | Severity_number_warn4 ->
      (Logs.Warning, None)
  | Severity_number_error
  | Severity_number_error2
  | Severity_number_error3
  | Severity_number_error4 ->
      (Logs.Error, None)
  | Severity_number_fatal
  | Severity_number_fatal2
  | Severity_number_fatal3
  | Severity_number_fatal4 ->
      (Logs.Error, Some "FATAL")

open Opentelemetry

let ptime_of_nano ns =
  let d = Int64.div ns Timestamp_ns.ns_in_a_day |> Int64.to_int
  and ps = Int64.mul 1000L @@ Int64.rem ns Timestamp_ns.ns_in_a_day in
  Ptime.unsafe_of_d_ps (d, ps)

let pp_timestamp_ns = Fmt.using ptime_of_nano @@ Ptime.pp_rfc3339 ~frac_s:9 ()

let timestamp_style = `Faint

(*let pp_line m ?header ?tags fmt = m ?header ?tags fmt*)

open Opentelemetry_proto.Common

(** See https://ocaml.org/manual/5.3/api/Format_tutorial.html#1_Refinementonhovboxes *)
let structural_braces pp ppf = Fmt.pf ppf "@[<1>{%a@;<0 -1>}@]" pp

let rec pp_any_value ppf = function
  | String_value s ->
      (* format embedded newlines as Fmt.cut, useful for stacktraces *)
      Fmt.(text |> hbox) ppf (String.trim s)
  | Bool_value b ->
      Fmt.bool ppf b
  | Int_value i ->
      Fmt.int64 ppf i
  | Double_value d ->
      Fmt.float ppf d
  | Array_value {values} ->
      Fmt.(Dump.list pp_any_value ppf values)
  | Kvlist_value {values} ->
      (structural_braces pp_key_value_list) ppf values
  | Bytes_value b ->
      Format.pp_print_bytes ppf b

and pp_key_value ppf {key; value} =
  match value with
  | None ->
      Fmt.string ppf key
  | Some value ->
      Fmt.field key Fun.id pp_any_value ppf value

and pp_key_value_list ppf = Fmt.(list ~sep:comma pp_key_value) ppf

let pp_body = Fmt.option pp_any_value

let create_backend
    ?(severity = Opentelemetry_proto.Logs.Severity_number_unspecified)
    ?(formatter = Fmt.stderr) () =
  (module struct
    open Opentelemetry_proto.Logs

    let pp_line ~time_unix_nano ~span_id fmt =
      (* vertical pretty printers would always insert a newline,
     to avoid them we need to override the newline printing functions *)
      let b = Buffer.create 80 in
      let buf = Fmt.with_buffer ~like:formatter b in
      Format.pp_set_margin buf 10000 ;
      Format.(
        pp_set_formatter_out_functions buf
          {
            (pp_get_formatter_out_functions buf ()) with
            out_newline=
              (fun () -> if Fmt.utf_8 buf then Buffer.add_string b "↵")
          ; out_indent= (fun n -> if n >= 1 then Buffer.add_char b ' ')
          }
      ) ;
      let k buf =
        Fmt.flush buf () ;
        Fmt.pf formatter "[%a|%a] %a@,"
          Fmt.(styled timestamp_style pp_timestamp_ns)
          time_unix_nano
          Fmt.(styled timestamp_style Span_id.pp)
          ( if Bytes.length span_id > 0 then
              span_id |> Span_id.of_bytes
            else
              Span_id.dummy
          )
          Fmt.buffer b
      in
      Format.kfprintf k buf fmt

    let log_record (t : log_record) =
      (* attributes are ignored *)
      if t.severity_number >= severity then (
        let level, severity = string_of_severity t.severity_number in

        let severity =
          if t.severity_text <> "" then
            Some t.severity_text
          else
            severity
        in
        pp_line ~time_unix_nano:t.time_unix_nano ~span_id:t.span_id " %a@ %a"
          Logs_fmt.pp_header (level, severity) pp_body t.body ;
        if t.severity_number >= Severity_number_info then Fmt.flush formatter ()
      )

    let scope_logs t =
      List.iter log_record t.Opentelemetry_proto.Logs.log_records

    let resource_logs t = List.iter scope_logs t.scope_logs

    let send_logs =
      Collector.
        {
          send=
            (fun (msg : Opentelemetry_proto.Logs.resource_logs list) ~ret ->
              msg |> List.iter resource_logs ;
              ret ()
            )
        }

    open Opentelemetry_proto.Trace

    let status t = t.status

    let attributes t = t.attributes

    let events t = t.events |> List.rev

    let style_ok = `Green

    let style_error = `Red

    let ok = Fmt.(if_utf_8 (any "✓") (any "OK"))

    let error = Fmt.(if_utf_8 (any "✗") (any "ERROR"))

    let unset = Fmt.any "?"

    let pp_status_status_code ppf = function
      | Status_code_ok ->
          ok ppf ()
      | Status_code_error ->
          error ppf ()
      | Status_code_unset ->
          unset ppf ()

    let pp_status ppf t =
      let style =
        match t.code with
        | Status_code_ok ->
            style_ok
        | Status_code_error ->
            style_error
        | Status_code_unset ->
            `None
      in

      Fmt.pf ppf "%a %a"
        (Fmt.styled style pp_status_status_code)
        t.code Fmt.string t.message

    let pp_span_event ppf (t : Event.t) =
      Fmt.pf ppf " @[%s@ %a@]" t.name pp_key_value_list t.attributes

    let style_span = `Bold

    let is_error = function
      | Some {code= Status_code_error; _} ->
          true
      | Some _ | None ->
          false

    let span t =
      if is_error t.status || severity <= Severity_number_debug then (
        let duration =
          (Int64.sub t.end_time_unix_nano t.start_time_unix_nano
          |> Int64.to_float
          )
          *. 1e-9
        in
        pp_line ~time_unix_nano:t.start_time_unix_nano ~span_id:t.span_id
          "@[<1>%a@ %a@ %a@]"
          Fmt.(styled style_span string)
          t.name
          Fmt.(option pp_status)
          (status t) pp_key_value_list (attributes t) ;
        let () =
          events t
          |> List.iter @@ fun ev ->
             pp_line ~time_unix_nano:ev.time_unix_nano ~span_id:t.span_id "%a"
               pp_span_event ev ;
             pp_line ~time_unix_nano:t.end_time_unix_nano ~span_id:t.span_id
               "[duration: %+10.6fs]" duration
        in
        if is_error t.status then Fmt.flush formatter ()
      )

    let scope_spans t = t.spans |> List.iter span

    let resource_spans t = t.scope_spans |> List.iter scope_spans

    let send_trace =
      Collector.
        {
          send=
            (fun msg ~ret ->
              msg |> List.iter resource_spans ;
              ret ()
            )
        }

    open Opentelemetry_proto.Metrics

    let metric = ignore

    let scope_metrics t = t.metrics |> List.iter metric

    let resource_metrics t = t.scope_metrics |> List.iter scope_metrics

    let send_metrics =
      Collector.
        {
          send=
            (fun msg ~ret ->
              msg |> List.iter resource_metrics ;
              ret ()
            )
        }

    let signal_emit_gc_metrics () = ()

    let on_tick = Atomic.make (AList.make ())

    let execute f = f ()

    let flush () = Fmt.flush formatter ()

    let tick () =
      Atomic.get on_tick |> AList.get |> List.iter execute ;
      flush ()

    let cleanup () = flush ()

    let set_on_tick_callbacks = Atomic.set on_tick end
  : Opentelemetry.Collector.BACKEND
)

let () =
  let m = Mutex.create () in
  let lock () = Mutex.lock m and unlock () = Mutex.unlock m in
  Opentelemetry.Lock.set_mutex ~lock ~unlock

let cols () =
  try
    let ch = Unix.open_process_args_in "tput" [|"tput"; "cols"|] in
    let finally () =
      let (_ : Unix.process_status) = Unix.close_process_in ch in
      ()
    in
    Option.map int_of_string
    @@ Fun.protect ~finally
    @@ fun () -> In_channel.input_line ch
  with _ -> None

let () =
  (* set pretty printer margin according to actual terminal width, if known *)
  cols ()
  |> Option.iter @@ fun margin ->
     Format.pp_set_margin Fmt.stdout margin ;
     Format.pp_set_margin Fmt.stderr margin

let with_setup ?severity ?formatter ?enable () f =
  let backend = create_backend ?severity ?formatter () in
  Collector.with_setup_debug_backend ?enable backend () f
