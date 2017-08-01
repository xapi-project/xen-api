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


let finally fct clean_f =
  let result =
    try
      fct ();
    with exn ->
      Backtrace.is_important exn;
      clean_f ();
      raise exn in
  clean_f ();
  result

(* Those should go into the Opt module: *)

let maybe_with_default d f v =
  match v with None -> d | Some x -> f x

(** if v is not none, apply f on it and return some value else return none. *)
let may f v = maybe_with_default None (fun x -> Some (f x)) v

(** default value to d if v is none. *) 
let default d v = maybe_with_default d (fun x -> x) v

(** apply f on v if not none *)
let maybe f v = maybe_with_default () f v

(** if bool is false then we intercept and quiten any exception *)
let reraise_if bool fct =
  try fct () with exn -> if bool then raise exn else ()

(** execute fct ignoring exceptions *)
let ignore_exn fct = try fct () with _ -> ()

(* non polymorphic ignore function *)
let ignore_int v = let (_: int) = v in ()
let ignore_int64 v = let (_: int64) = v in ()
let ignore_int32 v = let (_: int32) = v in ()
let ignore_string v = let (_: string) = v in ()
let ignore_float v = let (_: float) = v in ()
let ignore_bool v = let (_: bool) = v in ()

(* To avoid some parens: *)
(* composition of functions: *)
let (++) f g x = f (g x)

(* and application *)
let ($) f a = f a
