(*
 * Copyright (c) Cloud Software Group
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
[@@@ocaml.warning "-27"]

let u x = raise Observer_helpers.(Observer_error (Errors.Unimplemented x))

module Observer = struct
  type context = unit

  let create ctx ~dbg ~uuid ~name_label ~attributes ~endpoints ~enabled =
    u "Observer.create"

  let destroy ctx ~dbg ~uuid = u "Observer.destroy"

  let set_enabled ctx ~dbg ~uuid ~enabled = u "Observer.set_enabled"

  let set_attributes ctx ~dbg ~uuid ~attributes = u "Observer.set_attributes"

  let set_endpoints ctx ~dbg ~uuid ~endpoints = u "Observer.set_endpoints"

  let init ctx ~dbg = u "Observer.init"

  let set_trace_log_dir ctx ~dbg ~dir = u "Observer.set_trace_log_dir"

  let set_export_interval ctx ~dbg ~interval = u "Observer.set_export_interval"

  let set_max_spans ctx ~dbg ~spans = u "Observer.set_max_spans"

  let set_max_traces ctx ~dbg ~traces = u "Observer.set_max_traces"

  let set_max_file_size ctx ~dbg ~file_size = u "Observer.set_max_file_size"

  let set_host_id ctx ~dbg ~host_id = u "Observer.set_host_id"

  let set_compress_tracing_files ctx ~dbg ~enabled =
    u "Observer.set_compress_tracing_files"
end
