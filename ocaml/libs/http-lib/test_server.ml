let with_lock = Xapi_stdext_threads.Threadext.Mutex.execute

open Xapi_stdext_unix

let finished = ref false

let finished_m = Mutex.create ()

let finished_c = Condition.create ()

let _ =
  let port = ref 8080 in
  Arg.parse
    [("-p", Arg.Set_int port, "port to listen on")]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s\n" x)
    "A simple test HTTP server" ;
  let open Http_svr in
  let server = Server.empty () in
  Server.add_handler server Http.Get "/stop"
    (FdIO
       (fun _ s _ ->
         let r = Http.Response.to_wire_string (Http.Response.make "200" "OK") in
         Unixext.really_write_string s r ;
         with_lock finished_m (fun () ->
             finished := true ;
             Condition.signal finished_c
         )
       )
    ) ;
  Server.add_handler server Http.Post "/echo"
    (FdIO
       (fun request s _ ->
         match request.Http.Request.content_length with
         | None ->
             Unixext.really_write_string s
               (Http.Response.to_wire_string
                  (Http.Response.make "404" "content length missing")
               )
         | Some l ->
             let txt = Unixext.really_read_string s (Int64.to_int l) in
             let r =
               Http.Response.to_wire_string
                 (Http.Response.make ~body:txt "200" "OK")
             in
             Unixext.really_write_string s r
       )
    ) ;
  Server.add_handler server Http.Get "/stats"
    (FdIO
       (fun _ s _ ->
         let lines =
           List.map
             (fun (m, uri, s) ->
               Printf.sprintf "%s,%s,%d,%d\n"
                 (Http.string_of_method_t m)
                 uri s.Http_svr.Stats.n_requests s.Http_svr.Stats.n_connections
             )
             (Server.all_stats server)
         in
         let txt = String.concat "" lines in
         let r =
           Http.Response.to_wire_string (Http.Response.make ~body:txt "200" "OK")
         in
         Unixext.really_write_string s r
       )
    ) ;
  Server.add_handler server Http.Get "/query"
    (FdIO
       (fun request s _ ->
         match request.Http.Request.query with
         | (_, v) :: _ ->
             Unixext.really_write_string s
               (Http.Response.to_wire_string
                  (Http.Response.make ~body:v "200" "OK")
               )
         | _ ->
             Unixext.really_write_string s
               (Http.Response.to_wire_string
                  (Http.Response.make "404" "Query string missing")
               )
       )
    ) ;
  let ip = "0.0.0.0" in
  let inet_addr = Unix.inet_addr_of_string ip in
  let addr = Unix.ADDR_INET (inet_addr, !port) in
  let socket = Http_svr.bind ~listen_backlog:5 addr "server" in
  start ~conn_limit:1024 server socket ;
  Printf.printf "Server started on %s:%d\n" ip !port ;
  with_lock finished_m (fun () ->
      while not !finished do
        Condition.wait finished_c finished_m
      done
  ) ;
  Printf.printf "Exiting\n" ;
  stop socket
