(*
 * Copyright (C) Cloud Software Group
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

let make_ctx ~user_agent ~client_ip =
  let open Context in
  let additional_headers =
    client_ip
    |> Option.fold ~none:[] ~some:(fun x ->
        [("STUNNEL_PROXY", Printf.sprintf "TCP6 %s another_ip 443 80" x)]
    )
  in
  let rq = {Http.Request.empty with user_agent; additional_headers} in
  (* it doesn't matter which fd is used to here, we are just satisying the
     type system. we use stderr because then we don't need to worry about
     closing it *)
  make ~origin:(Http (rq, Unix.stderr)) "text_ctx"

let test_tracked_user_agents ~agents ~expected () =
  Xapi_tracked_user_agents.reset () ;
  List.iter
    (fun user_agent ->
      let __context =
        make_ctx ~user_agent:(Some user_agent) ~client_ip:(Some "1.2.3.4")
      in
      Xapi_tracked_user_agents.track ~__context
    )
    agents ;
  let compare ua1 ua2 = String.compare (fst ua1) (fst ua2) in
  Alcotest.(check (list (pair string string)))
    "new user agents are equal to expected"
    (List.sort compare expected)
    (Xapi_tracked_user_agents.get () |> List.sort compare)

let tests =
  [
    ( "tracked_user_agents_base"
    , [
        ( "test_tracked_user_agents_base"
        , `Quick
        , test_tracked_user_agents
            ~agents:["XenCenter/2025.2.0.8315"]
            ~expected:[("XenCenter", "2025.2.0.8315")]
        )
      ; ( "test_tracked_user_agents_version_last_seen"
        , `Quick
        , test_tracked_user_agents
            ~agents:
              [
                "XenCenter/2025.2.0.8315"
              ; "XenAPI/2.15"
              ; "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
              ; "XenCenter/2025.2.0.8316"
              ]
            ~expected:
              [
                ("XenCenter", "2025.2.0.8316")
              ; ("XenAPI", "2.15")
              ; ("Mozilla", "5.0")
              ]
        )
      ; ( "test_tracked_user_agents_no_version"
        , `Quick
        , test_tracked_user_agents ~agents:["XenCenter"]
            ~expected:[("XenCenter", "")]
        )
      ; ( "test_tracked_user_agents_no_slash"
        , `Quick
        , test_tracked_user_agents
            ~agents:["XenCenter 2025.2.0.8315"]
            ~expected:[("XenCenter", "")]
        )
      ; ( "test_tracked_user_agents_exceeding_maxstrlen"
        , `Quick
        , test_tracked_user_agents
            ~agents:
              [
                "XenCenter/2025.2.0.8315.11111111111111111111111111111111111111111"
              ]
            ~expected:[]
        )
      ; ( "test_tracked_user_agents_exceeding_max_num"
        , `Quick
        , test_tracked_user_agents
            ~agents:(List.init 130 (Printf.sprintf "Agent%d/1.0"))
            ~expected:
              (List.init 128 (fun i -> (Printf.sprintf "Agent%d" (i + 2), "1.0"))
              )
        )
      ]
    )
  ]
