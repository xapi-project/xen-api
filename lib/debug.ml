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

module type BRAND = sig val name: string end

type level =
| Debug
| Warn
| Info
| Error

let disabled_modules = ref []
let disable m =
  disabled_modules := m :: !disabled_modules

let stderr key level x =
  output_string stderr (Printf.sprintf "[%s|%s] %s" key (match level with
  | Debug -> "debug"
  | Warn -> "warn"
  | Info -> "info"
  | Error -> "error") x);
    output_string stderr "\n";
    flush stderr

let syslog ?(facility=`LOG_LOCAL5) name () =
  let t = Syslog.openlog ~facility name in
  fun key level x ->
    Syslog.syslog t (match level with
      | Debug -> `LOG_DEBUG
      | Warn -> `LOG_WARNING
      | Info -> `LOG_INFO
      | Error -> `LOG_ERR
    ) (Printf.sprintf "[%s] %s" key x)

let output = ref stderr

let write key level x =
  if not(List.mem key !disabled_modules)
  then !output key level x

module Make = functor(Brand: BRAND) -> struct
  let debug fmt = Printf.ksprintf (write Brand.name Debug) fmt
  let error fmt = Printf.ksprintf (write Brand.name Error) fmt
  let info fmt = Printf.ksprintf (write Brand.name Info) fmt
  let warn fmt = Printf.ksprintf (write Brand.name Warn) fmt
end
