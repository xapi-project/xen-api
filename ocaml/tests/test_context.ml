let make_rq stunnel_proxy =
  {
    Http.Request.empty with
    additional_headers= [("STUNNEL_PROXY", stunnel_proxy)]
  }

let test_get_client_ip () =
  let proxy_string = Printf.sprintf "TCP6 %s ::ffff:10.71.152.134 53772 443" in
  [
    ("192.168.1.1", "192.168.1.1")
  ; ("::ffff:10.71.56.24", "10.71.56.24")
  ; ( "abcd:abcd:abcd:abcd:abcd:abcd:192.168.158.190"
    , "abcd:abcd:abcd:abcd:abcd:abcd:c0a8:9ebe"
    )
  ; ("2001:0dc5:72a3:0000:0000:802e:3370:73e4", "2001:dc5:72a3::802e:3370:73e4")
  ]
  |> List.iter (fun (ip, exp) ->
      let client_ip =
        proxy_string ip |> make_rq |> Http_svr.https_client_of_req |> Option.get
      in
      Alcotest.(check string)
        "original ip string preserved" exp
        (Ipaddr.to_string client_ip)
  )

let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"

let traceparent = Printf.sprintf "00-%s-00f067aa0ba902b7-01" trace_id

let with_tracing_enabled f =
  let uuid = "e3f6a0c2-test-context-tracing" in
  Tracing.TracerProvider.create ~enabled:true ~attributes:[] ~endpoints:[]
    ~name_label:__MODULE__ ~uuid ;
  Fun.protect ~finally:(fun () -> Tracing.TracerProvider.destroy ~uuid) f

let with_http_origin ?(headers = []) f =
  let req =
    {
      Http.Request.empty with
      m= Http.Post
    ; user_agent= Some "test-agent"
    ; additional_headers= headers
    }
  in
  let s1, s2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close s1 ; Unix.close s2)
    (fun () -> f (Context.Http (req, s1)))

let check_attribute span key expected =
  Alcotest.(check (option string))
    (Printf.sprintf "attribute %s" key)
    (Some expected)
    (List.assoc_opt key (Tracing.Span.get_attributes span))

let test_tracing_disabled () =
  Alcotest.(check bool) "tracing is disabled" false (Tracing.get_observe ()) ;
  with_http_origin ~headers:[("traceparent", traceparent)] @@ fun origin ->
  let __context = Context.make ~origin "test_tracing_disabled" in
  Alcotest.(check bool)
    "no span without an enabled TracerProvider" true
    (Option.is_none (Context.tracing_of __context)) ;
  Context.with_tracing ~__context "child" (fun ctx ->
      Alcotest.(check bool)
        "with_tracing creates no span" true
        (Option.is_none (Context.tracing_of ctx))
  ) ;
  Context.complete_tracing __context

let test_tracing_enabled () =
  with_tracing_enabled @@ fun () ->
  with_http_origin ~headers:[("traceparent", traceparent)] @@ fun origin ->
  let __context = Context.make ~origin "test_tracing_enabled" in
  let span =
    match Context.tracing_of __context with
    | Some span ->
        span
    | None ->
        Alcotest.fail "expected a span with tracing enabled"
  in
  check_attribute span "xs.xapi.task.name" "test_tracing_enabled" ;
  check_attribute span "xs.xapi.task.origin" "http" ;
  check_attribute span "http.request.header.method" "POST" ;
  check_attribute span "http.request.header.user-agent" "test-agent" ;
  Alcotest.(check string)
    "incoming traceparent is adopted" trace_id
    Tracing.(
      Span.get_context span
      |> SpanContext.trace_id_of_span_context
      |> Trace_id.to_string
    ) ;
  Alcotest.(check string)
    "span of an incoming request is a server span"
    Tracing.SpanKind.(to_string Server)
    Tracing.(SpanKind.to_string (Span.get_span_kind span)) ;
  Context.with_tracing
    ~attributes:[("test.key", "test.value")]
    ~__context "child"
    (fun ctx ->
      match Context.tracing_of ctx with
      | Some child ->
          check_attribute child "test.key" "test.value" ;
          Alcotest.(check bool)
            "child span carries the task id" true
            (List.mem_assoc "xs.xapi.task.id"
               (Tracing.Span.get_attributes child)
            )
      | None ->
          Alcotest.fail "expected a child span with tracing enabled"
    ) ;
  Context.complete_tracing __context

let tests =
  [
    ( "Test_context"
    , [
        ("test_get_client_ip", `Quick, test_get_client_ip)
      ; ("test_tracing_disabled", `Quick, test_tracing_disabled)
      ; ("test_tracing_enabled", `Quick, test_tracing_enabled)
      ]
    )
  ]
