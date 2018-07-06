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

type t = {
  mutable n : int;
  m : Mutex.t;
  c : Condition.t;
}

let create n =
  if n <= 0 then
    invalid_arg (Printf.sprintf 
                   "Semaphore value must be positive, got %d" n);
  let m = Mutex.create ()
  and c = Condition.create () in
  { n; m; c; }

exception Inconsistent_state of string
let inconsistent_state fmt = Printf.kprintf (fun msg ->
    raise (Inconsistent_state msg)) fmt

let acquire s k =
  if k <= 0 then
    invalid_arg (Printf.sprintf 
                   "Semaphore acquisition requires a positive value, got %d" k);
  Mutex.lock s.m;
  while s.n < k do
    Condition.wait s.c s.m;
  done;
  if not (s.n >= k) then
    inconsistent_state "Semaphore value cannot be smaller than %d, got %d" k s.n;
  s.n <- s.n - k;
  Condition.signal s.c;
  Mutex.unlock s.m

let release s k =
  if k <= 0 then
    invalid_arg (Printf.sprintf 
                   "Semaphore release requires a positive value, got %d" k);
  Mutex.lock s.m;
  s.n <- s.n + k;
  Condition.signal s.c;
  Mutex.unlock s.m

let execute_with_weight s k f =
  acquire s k;
  Xapi_stdext_pervasives.Pervasiveext.finally f
    (fun () -> release s k)

let execute s f =
  execute_with_weight s 1 f
