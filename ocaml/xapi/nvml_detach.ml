(*
 * Copyright (C) 2026 Cloud Software Group
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

module D = Debug.Make (struct let name = "nvml_detach" end)

open D

type how = Stop_gpumon | Detach_nvml

let how ~detach_switch ~pass_through =
  if detach_switch && not pass_through then
    Detach_nvml
  else
    Stop_gpumon

module type NVML = sig
  val is_attached : unit -> bool

  val detach : unit -> unit

  val attach : unit -> unit
end

module Make (N : NVML) = struct
  let is_attached () = try Ok (N.is_attached ()) with e -> Error e

  let with_detached f =
    match is_attached () with
    | Error e ->
        (* gpumon is stopped or restarting, so it holds nothing to release *)
        warn "%s: gpumon does not answer (%s); running without a detach"
          __FUNCTION__ (Printexc.to_string e) ;
        f ()
    | Ok false ->
        debug "%s: NVML is detached; nothing to do" __FUNCTION__ ;
        f ()
    | Ok true ->
        (* Both xenopsd and xapi call /usr/lib/nvidia/sriov-manage, which
           stops and restarts gpumon around a VF change, and a restarted
           gpumon attaches by itself. So ask again before re-attaching, and
           never let the re-attach mask what f returned or raised. *)
        let detached = ref false in
        let attach () =
          debug "%s: about to attach NVML" __FUNCTION__ ;
          try N.attach ()
          with e ->
            error
              "%s: NVML attach failed (%s); gpumon stays detached unless it \
               still handles the request"
              __FUNCTION__ (Printexc.to_string e)
        in
        let reattach () =
          match is_attached () with
          | Ok true when !detached ->
              warn
                "%s: NVML was attached again while f ran (gpumon restarted?): \
                 the card was not released for all of it"
                __FUNCTION__
          | Ok true ->
              debug "%s: NVML is still attached after the failed detach"
                __FUNCTION__
          | Ok false ->
              attach ()
          | Error e ->
              (* A restarted gpumon attaches by itself, but a stuck one stays
                 detached once it resumes: attach anyway, as attaching twice
                 is harmless. Done here, under the caller's lock, rather than
                 in the background, so that it cannot reach gpumon in the
                 middle of the next start's detach. *)
              warn "%s: gpumon does not answer after f (%s); attaching anyway"
                __FUNCTION__ (Printexc.to_string e) ;
              attach ()
        in
        Fun.protect ~finally:reattach (fun () ->
            debug "%s: about to detach NVML" __FUNCTION__ ;
            ( try
                N.detach () ;
                detached := true
              with e ->
                (* Only vGPU starts come here (see [how]), and a vGPU does
                   not need the card released: carry on *)
                warn "%s: NVML detach failed (%s); running without a detach"
                  __FUNCTION__ (Printexc.to_string e)
            ) ;
            f ()
        )
end
