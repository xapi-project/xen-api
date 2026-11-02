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

open Xapi_stdext_unix.Lock_rw

type t = unit

let create () = ()

let waiter () span =
  let timer = Clock.Timer.start ~duration:span in
  let stop () = Clock.Timer.has_expired timer in
  let wait ~retry ~blockers:_ =
    let try_once () =
      match retry () with true -> Ok () | false -> Error `Busy
    in
    if stop () then
      Error `Busy (* no time left: the caller's try was the only one *)
    else
      Threadext.retry
        ~delay:Mtime.Span.(10 * ms)
        ~max_delay:Mtime.Span.(50 * ms)
        ~stop try_once
  in
  {stop; wait}
