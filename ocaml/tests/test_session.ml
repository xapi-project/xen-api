module Date = Xapi_stdext_date.Date

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
  ]
