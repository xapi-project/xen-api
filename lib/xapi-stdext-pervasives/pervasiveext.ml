(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(** apply the clean_f function after fct function has been called.
 * Even if fct raises an exception, clean_f is applied
*)

let src =
  Logs.Src.create "pervasiveext"
    ~doc:"logs from Xapi_stdext_pervasives.Pervasiveext"

let finally fct clean_f =
  let result =
    try fct ()
    with exn ->
      Backtrace.is_important exn ;
      ( try
          (* We catch and log exceptions raised by clean_f to avoid shadowing
             the original exception raised by fct *)
          clean_f ()
        with cleanup_exn ->
          Logs.warn ~src (fun m ->
              m
                "finally: Error while running cleanup after failure of main \
                 function: %s"
                (Printexc.to_string cleanup_exn))
      ) ;
      raise exn
  in
  clean_f () ; result

let maybe_with_default d f v = Option.fold ~none:d ~some:f v

let may f v = Option.map f v

let default d v = Option.value ~default:d v

let maybe f v = Option.iter f v

(** execute fct ignoring exceptions *)
let ignore_exn fct = try fct () with _ -> ()

(** if not bool ignore exceptions raised by fct () *)
let reraise_if bool fct = if bool then fct () else ignore_exn fct

(* non polymorphic ignore function *)
let ignore_int v =
  let (_ : int) = v in
  ()

let ignore_int64 v =
  let (_ : int64) = v in
  ()

let ignore_int32 v =
  let (_ : int32) = v in
  ()

let ignore_string v =
  let (_ : string) = v in
  ()

let ignore_float v =
  let (_ : float) = v in
  ()

let ignore_bool v =
  let (_ : bool) = v in
  ()

(* To avoid some parens: *)
(* composition of functions: *)
let ( ++ ) f g x = f (g x)

(* and application *)
let ( $ ) f a = f a
