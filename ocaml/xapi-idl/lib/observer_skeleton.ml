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

let unimplemented x =
  raise Observer_helpers.(Observer_error (Errors.Unimplemented x))

module Observer = struct
  type context = unit

  let create ctx ~dbg ~uuid ~name_label ~attributes ~endpoints ~enabled =
    unimplemented __FUNCTION__

  let destroy ctx ~dbg ~uuid = unimplemented __FUNCTION__

  let set_enabled ctx ~dbg ~uuid ~enabled = unimplemented __FUNCTION__

  let set_attributes ctx ~dbg ~uuid ~attributes = unimplemented __FUNCTION__

  let set_endpoints ctx ~dbg ~uuid ~endpoints = unimplemented __FUNCTION__

  let init ctx ~dbg = unimplemented __FUNCTION__

  let set_trace_log_dir ctx ~dbg ~dir = unimplemented __FUNCTION__

  let set_export_interval ctx ~dbg ~interval = unimplemented __FUNCTION__

  let set_export_chunk_size ctx ~dbg ~size = unimplemented __FUNCTION__

  let set_max_spans ctx ~dbg ~spans = unimplemented __FUNCTION__

  let set_max_traces ctx ~dbg ~traces = unimplemented __FUNCTION__

  let set_max_depth ctx ~dbg ~depth = unimplemented __FUNCTION__

  let set_max_file_size ctx ~dbg ~file_size = unimplemented __FUNCTION__

  let set_host_id ctx ~dbg ~host_id = unimplemented __FUNCTION__

  let set_compress_tracing_files ctx ~dbg ~enabled = unimplemented __FUNCTION__
end
