(*
 * Copyright (C) 2025 Citrix Systems Inc.
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
module D = Debug.Make (struct let name = "xapi_rate_limit" end)

let bucket_table = Rate_limit.Bucket_table.create ()

let register_xapi_globs () =
  let configs = !Xapi_globs.rate_limited_clients in
  List.iter
    (fun s ->
      match String.split_on_char ':' s with
      | [user_agent; burst_s; fill_s] -> (
        match (float_of_string_opt burst_s, float_of_string_opt fill_s) with
        | Some burst_size, Some fill_rate ->
            D.debug
              "Adding user agent %s to bucket table with burst size %f and \
               fill rate %f"
              user_agent burst_size fill_rate ;
            Rate_limit.Bucket_table.add_bucket bucket_table ~user_agent
              ~burst_size ~fill_rate
        | _ ->
            D.debug "Skipping invalid numeric values in: %s\n" s
      )
      | _ ->
          D.debug "Skipping invalid item format: %s\n" s
    )
    configs
