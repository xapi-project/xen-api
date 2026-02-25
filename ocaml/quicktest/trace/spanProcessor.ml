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

let ( let@ ) t f = Option.iter f t

let on_start _scope = ()

let is_error t =
  match Scope.status t with
  | Some {code= Status_code_error; _} ->
      true
  | _ ->
      false

let on_end t =
  if not Scope.(is_sampled t) then begin
    if is_error t then
      (* tail-based sampling: upgrade RECORD_ONLY to RECORD_AND_SAMPLE on error *)
      Scope.set_decision t RECORD_AND_SAMPLE
  end

let force_flush () =
  let@ (module B) = Opentelemetry.Collector.get_backend () in
  B.tick ()

let shutdown () =
  (* backend cleanup is done elsewhere already, so only flush *)
  force_flush ()
