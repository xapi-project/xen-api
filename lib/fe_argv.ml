(*
 * Copyright (C) Citrix Systems Inc.
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


(* state that we incrementally change *)
type argv =
  { argv: string list                       (** command line in rev order *)
  ; fds:  (string * Unix.file_descr) list   (** open files *)
  }


  (* This is a state monad *)
type 'a t = argv -> 'a * argv

let return x = fun s -> x, s
let bind m f = fun s -> match m s with x, s' -> f x s'
let (>>=)    = bind

let empty =
  { argv = []
  ; fds  = []
  }

let run t       = t empty
let argv   argv = List.rev argv.argv
let fd_map argv = argv.fds

module Add = struct

  let arg x =
    fun s -> (), { s with argv = x :: s.argv }

  let many xs =
    fun s -> (), { s with argv = List.rev xs @ s.argv }

  let each f xs =
    xs |> List.map f |> List.concat |> many

  let fmt fmt =
    Printf.kprintf
      (fun str -> fun s -> (), { s with argv = str :: s.argv })
      fmt

  let file_descr uuid fd =
    fun s -> (), { s with fds = (uuid, fd) :: s.fds }

end
