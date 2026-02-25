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

let should_sample ?parent_scope ?trace_id:_ ?kind:_ ?(attrs = []) ?links:_
    ?trace_state name =
  let parent_sampled =
    match parent_scope with
    | None ->
        (* run_with_progress is too verbose, but we want to log everything else
        for debugging *)
        name <> "run_with_progress"
    | Some scope ->
        Scope.is_sampled scope
  in
  let decision =
    if Opentelemetry.Collector.has_backend () then
      (* for tail-based sampling to work, this must not be DROP *)
      if parent_sampled then
        Sampling.RECORD_AND_SAMPLE
      else
        RECORD_ONLY
    else
      (* without a backend these wouldn't go anywhere, so don't record them in the first place *)
      DROP
  in
  let attrs =
    if decision = DROP then
      []
    else
      attrs
  in
  Sampling.{decision; attrs; trace_state}

let get_description () = "ParentBasedOrRecordOnly"
