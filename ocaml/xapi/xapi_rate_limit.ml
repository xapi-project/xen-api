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
module D = Debug.Make (struct let name = "xapi_rate_limit" end)

open Rate_limit

let bucket_table = Bucket_table.create ()

let create ~__context ~client_id ~burst_size ~fill_rate =
  if Bucket_table.mem bucket_table ~user_agent:client_id then
    raise
      Api_errors.(
        Server_error
          ( map_duplicate_key
          , ["client_id"; client_id; "client_id already registered"]
          )
      ) ;
  let uuid = Uuidx.make () in
  let ref = Ref.make () in
  let add_bucket_succeeded =
    Bucket_table.add_bucket bucket_table ~user_agent:client_id ~burst_size
      ~fill_rate
  in
  match add_bucket_succeeded with
  | true ->
      Db.Rate_limit.create ~__context ~ref ~uuid:(Uuidx.to_string uuid)
        ~client_id ~burst_size ~fill_rate ;
      ref
  | false ->
      raise
        Api_errors.(
          Server_error
            ( invalid_value
            , [
                "fill_rate"
              ; string_of_float fill_rate
              ; "Fill rate cannot be 0 or negative"
              ]
            )
        )

let destroy ~__context ~self =
  let record = Db.Rate_limit.get_record ~__context ~self in
  Bucket_table.delete_bucket bucket_table
    ~user_agent:record.rate_limit_client_id ;
  Db.Rate_limit.destroy ~__context ~self

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
            if
              not
                (Rate_limit.Bucket_table.add_bucket bucket_table ~user_agent
                   ~burst_size ~fill_rate
                )
            then
              D.error
                "Bucket creation failed for user agent %s: invalid fill rate %f"
                user_agent fill_rate
        | _ ->
            D.debug "Skipping invalid numeric values in: %s\n" s
      )
      | _ ->
          D.debug "Skipping invalid item format: %s\n" s
    )
    configs
