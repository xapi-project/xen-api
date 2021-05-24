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
           proxy_string ip |> make_rq |> Context._client_of_rq |> Option.get
         in
         Alcotest.(check string)
           "original ip string preserved" exp
           (Ipaddr.to_string client_ip)
     )

let tests =
  [("Test_context", [("test_get_client_ip", `Quick, test_get_client_ip)])]
