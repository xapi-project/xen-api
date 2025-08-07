module Date = Clock.Date

let now = Date.of_iso8601 "2020-09-22T14:57:11Z"

let future = Date.of_iso8601 "2020-09-22T15:03:13Z"

let fail_login ~__context ~uname ~originator ~now () =
  try
    Xapi_session._record_login_failure ~__context ~now ~uname ~originator
      ~record:`log_and_alert (fun () ->
        if Random.bool () then
          raise Api_errors.(Server_error (session_authentication_failed, []))
        else
          raise (Auth_signature.Auth_failure "Auth failure")
    )
  with _ -> ()

let success_login ~__context ~uname ~originator ~now () =
  Xapi_session._record_login_failure ~__context ~now ~uname ~originator
    ~record:`log_and_alert Fun.id

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

let repeat n f =
  for _ = 1 to n do
    f ()
  done

let run_unknown_client_logins () =
  let __context = make_ctx ~user_agent:None ~client_ip:None in
  repeat 50
    (success_login ~__context ~uname:(Some "good_user")
       ~originator:(Some "nice_origin") ~now
    ) ;
  repeat 10 (fail_login ~__context ~uname:None ~originator:None ~now) ;
  repeat 50
    (success_login ~__context ~uname:(Some "good_user")
       ~originator:(Some "nice_origin") ~now
    )

let run_known_client_logins () =
  let __context =
    make_ctx ~user_agent:(Some "UA") ~client_ip:(Some "4.3.2.1")
  in
  let __context_no_UA = make_ctx ~user_agent:None ~client_ip:(Some "5.4.3.2") in
  let __context_no_client_ip =
    make_ctx ~user_agent:(Some "UA") ~client_ip:None
  in
  repeat 50
    (success_login ~__context ~uname:(Some "good_user")
       ~originator:(Some "nice_origin") ~now
    ) ;
  repeat 2
    (fail_login ~__context:__context_no_UA ~uname:(Some "usr1") ~now
       ~originator:(Some "origin1")
    ) ;
  repeat 3
    (fail_login ~__context:__context_no_UA ~uname:None
       ~originator:(Some "origin2") ~now
    ) ;
  repeat 4 (fail_login ~__context ~uname:None ~originator:None ~now) ;
  repeat 6
    (fail_login ~__context:__context_no_client_ip ~uname:(Some "usr4")
       ~originator:None ~now:future
    ) ;
  let () =
    (* this client fails now and then in the future (to test timestamp) *)
    repeat 9
      (fail_login ~__context:__context_no_UA ~uname:(Some "usr5")
         ~originator:(Some "origin5") ~now
      ) ;
    repeat 1
      (fail_login ~__context:__context_no_UA ~uname:(Some "usr5")
         ~originator:(Some "origin5") ~now:future
      )
  in
  repeat 50
    (success_login ~__context ~uname:(Some "good_user")
       ~originator:(Some "nice_origin") ~now:future
    )

let test_fetching_failed_login_stats_twice_yields_none () =
  let _ = Xapi_session.get_failed_login_stats () in
  let stats = Xapi_session.get_failed_login_stats () in
  Alcotest.(check @@ option string)
    "no extra stats have been accumulated" stats None

let test_only_failed_logins_from_unknown_clients () =
  let _ = Xapi_session.get_failed_login_stats () in
  run_unknown_client_logins () ;
  let stats = Xapi_session.get_failed_login_stats () |> Option.get in
  Alcotest.(check string)
    "report talks about unknown clients only"
    {|<body>
<unknown>10</unknown>
</body>|} stats

let test_failed_logins_from_known_clients_only () =
  let _ = Xapi_session.get_failed_login_stats () in
  run_known_client_logins () ;
  let stats = Xapi_session.get_failed_login_stats () |> Option.get in
  Alcotest.(check string)
    "report talks about known clients only"
    {|<body>
<total>25</total>
<known>
<username>usr5</username>
<originator>origin5</originator>
<ip>5.4.3.2</ip>
<number>10</number>
<date>20200922T15:03:13Z</date>
</known>
<known>
<username>usr4</username>
<useragent>UA</useragent>
<number>6</number>
<date>20200922T15:03:13Z</date>
</known>
<known>
<useragent>UA</useragent>
<ip>4.3.2.1</ip>
<number>4</number>
<date>20200922T14:57:11Z</date>
</known>
</body>|}
    stats

let test_failed_logins_from_both_known_and_unknown_clients () =
  let _ = Xapi_session.get_failed_login_stats () in
  run_known_client_logins () ;
  run_unknown_client_logins () ;
  let stats = Xapi_session.get_failed_login_stats () |> Option.get in
  Alcotest.(check string)
    "report talks about unknown and known clients"
    {|<body>
<total>35</total>
<known>
<username>usr5</username>
<originator>origin5</originator>
<ip>5.4.3.2</ip>
<number>10</number>
<date>20200922T15:03:13Z</date>
</known>
<known>
<username>usr4</username>
<useragent>UA</useragent>
<number>6</number>
<date>20200922T15:03:13Z</date>
</known>
<known>
<useragent>UA</useragent>
<ip>4.3.2.1</ip>
<number>4</number>
<date>20200922T14:57:11Z</date>
</known>
<unknown>10</unknown>
</body>|}
    stats

let test_pool_metrics_user_agents ~coming ~stored ~whitelist ~expected () =
  let __context =
    make_ctx ~user_agent:(Some coming) ~client_ip:(Some "1.2.3.4")
  in
  let pool_metrics_ref = Ref.make () in
  Db.Pool_metrics.create ~__context ~ref:pool_metrics_ref
    ~uuid:(Uuidx.to_string (Uuidx.make ()))
    ~user_agents:stored ;
  let pool = Helpers.get_pool ~__context in
  Db.Pool.set_metrics ~__context ~self:pool ~value:pool_metrics_ref ;
  Xapi_globs.interested_user_agents := whitelist ;
  Xapi_session.record_user_agent ~__context ;
  let new_user_agents =
    Db.Pool_metrics.get_user_agents ~__context ~self:pool_metrics_ref
  in
  Alcotest.(check (list (pair string string)))
    "new user agents are equal to expected" expected new_user_agents

let tests =
  [
    ( "AuthFail"
    , [
        ( "test_fetching_failed_login_stats_twice_yields_none"
        , `Quick
        , test_fetching_failed_login_stats_twice_yields_none
        )
      ; ( "test_only_failed_logins_from_unknown_clients"
        , `Quick
        , test_only_failed_logins_from_unknown_clients
        )
      ; ( "test_failed_logins_from_clients"
        , `Quick
        , test_failed_logins_from_known_clients_only
        )
      ; ( "test_failed_logins_from_both_known_and_unknown_clients"
        , `Quick
        , test_failed_logins_from_both_known_and_unknown_clients
        )
      ]
    )
  ; ( "pool_metrics_user_agents"
    , [
        ( "test_pool_metrics_user_agents_base"
        , `Quick
        , test_pool_metrics_user_agents ~coming:"XenCenter/2025.2.0.8315"
            ~stored:[] ~whitelist:["XenCenter"; "XenAPI"]
            ~expected:[("XenCenter", "2025.2.0.8315")]
        )
      ; ( "test_pool_metrics_user_agents_version_update"
        , `Quick
        , test_pool_metrics_user_agents ~coming:"XenCenter/2025.2.0.8316"
            ~stored:[("XenCenter", "2025.2.0.8315")]
            ~whitelist:["XenCenter"; "XenAPI"]
            ~expected:[("XenCenter", "2025.2.0.8316")]
        )
      ; ( "test_pool_metrics_user_agents_not_in_whitelist"
        , `Quick
        , test_pool_metrics_user_agents
            ~coming:"Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
            ~stored:[("XenCenter", "2025.2.0.8315"); ("XenAPI", "2.15")]
            ~whitelist:["XenCenter"; "XenAPI"]
            ~expected:[("XenCenter", "2025.2.0.8315"); ("XenAPI", "2.15")]
        )
      ; ( "test_pool_metrics_user_agents_no_version"
        , `Quick
        , test_pool_metrics_user_agents ~coming:"XenCenter"
            ~stored:[("XenCenter", "2025.2.0.8315")]
            ~whitelist:["XenCenter"; "XenAPI"]
            ~expected:[("XenCenter", "")]
        )
      ; ( "test_pool_metrics_user_agents_invalid_format"
        , `Quick
        , test_pool_metrics_user_agents ~coming:"XenCenter 2025.2.0.8315"
            ~stored:[] ~whitelist:["XenCenter"; "XenAPI"]
            ~expected:[("XenCenter", "")]
        )
      ; ( "test_pool_metrics_user_agents_invalid_format2"
        , `Quick
        , test_pool_metrics_user_agents ~coming:"XenCenterFake/2025.2.0.8315"
            ~stored:[] ~whitelist:["XenCenter"; "XenAPI"] ~expected:[]
        )
      ; ( "test_pool_metrics_user_agents_exceeding_length"
        , `Quick
        , test_pool_metrics_user_agents
            ~coming:
              "XenCenter/2025.2.0.8315.11111111111111111111111111111111111111111"
            ~stored:[] ~whitelist:["XenCenter"; "XenAPI"] ~expected:[]
        )
      ]
    )
  ]
