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

(* Test results *)

type resultdata =
  | StartTest of float list
  | SizeTest of float
  | ShutdownTest of float list
  | CloneTest of float list (* one float list per gold VM cloned *)

type result = {
  resultname : string;
  subtest : string;
  xenrtresult : float;
  rawresult : resultdata; (* Specific to the actual test *)
}

let header = "RAW"

let sep = ':'

let to_string (results:result list) =
  Printf.sprintf "%s%c%s" header sep (Marshal.to_string results [Marshal.No_sharing])

let from_string s : result list option =
  let open Xapi_stdext_std.Xstringext.String in
  if startswith header s
  then begin
    match split ~limit:2 sep s with
    | [_; r] -> Some (Marshal.from_string r 0)
    | _ -> None
  end else
    None
