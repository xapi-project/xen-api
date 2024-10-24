open Xapi_stdext_pervasives.Pervasiveext
open Xapi_stdext_unix
open Safe_resources

let user_agent = "test_client"

let ip = ref "127.0.0.1"

let port = ref 8080

let use_ssl = ref false

let use_fastpath = ref false

let use_framing = ref false

let with_connection ip port f =
  let inet_addr = Unix.inet_addr_of_string ip in
  let addr = Unix.ADDR_INET (inet_addr, port) in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  (try Unix.connect s addr with e -> Unix.close s ; raise e) ;
  Unixext.set_tcp_nodelay s true ;
  finally (fun () -> f s) (fun () -> Unix.close s)

let with_stunnel ip port f =
  Stunnel.with_connect ~verify_cert:None ~use_fork_exec_helper:false
    ~extended_diagnosis:false ip port
  @@ fun s ->
  let fd = s.Stunnel.fd in
  f Unixfd.(!fd)

let one ~use_fastpath ~use_framing keep_alive s =
  Http_client.rpc ~use_fastpath s
    (Http.Request.make ~frame:use_framing ~version:"1.1" ~keep_alive ~user_agent
       ~body:"hello" Http.Post "/echo"
    ) (fun response s ->
      match response.Http.Response.content_length with
      | Some l ->
          let (_ : string) = Unixext.really_read_string s (Int64.to_int l) in
          (*
					Printf.printf "Read [%s]\n" x;
					flush stdout
*)
          ()
      | None ->
          failwith "Need a content length"
  )

let query ~use_fastpath ~use_framing keep_alive s =
  let query_string = "v1,v2,v3,<>`" in
  Http_client.rpc ~use_fastpath s
    (Http.Request.make ~frame:use_framing ~version:"1.1" ~keep_alive ~user_agent
       ~query:[("k1", query_string)]
       Http.Get "/query"
    )
    (fun response s ->
      match response.Http.Response.content_length with
      | Some l ->
          let s = Unixext.really_read_string s (Int64.to_int l) in
          if s <> query_string then
            failwith "Incorrectly parsed query string"
          else
            ()
      | None ->
          failwith "Need a content length"
    )

module Normal_population = struct
  (** Stats on a normally-distributed population *)
  type t = {sigma_x: float; sigma_xx: float; n: int}

  let empty = {sigma_x= 0.; sigma_xx= 0.; n= 0}

  let sample (p : t) (x : float) : t =
    {sigma_x= p.sigma_x +. x; sigma_xx= p.sigma_xx +. (x *. x); n= p.n + 1}

  exception Unknown

  let mean (p : t) : float = p.sigma_x /. float_of_int p.n

  let sd (p : t) : float =
    if p.n = 0 then
      raise Unknown
    else
      let n = float_of_int p.n in
      sqrt ((n *. p.sigma_xx) -. (p.sigma_x *. p.sigma_x)) /. n

  let to_string (p : t) = Printf.sprintf "%.1f +/- %.1f" (mean p) (sd p)
end

let per_nsec n f =
  let start = Unix.gettimeofday () in
  let t = ref 0 in
  while Unix.gettimeofday () -. start < n do
    f () ; incr t
  done ;
  int_of_float (float_of_int !t /. n)

let threads n f =
  let results = Array.make n 0 in
  let body i () = results.(i) <- f () in
  let threads = Array.mapi (fun i _ -> Thread.create (body i) ()) results in
  Array.iter Thread.join threads ;
  Array.fold_left ( + ) 0 results

let sample n f =
  let p = ref Normal_population.empty in
  for _ = 1 to n do
    let v = f () in
    p := Normal_population.sample !p (float_of_int v)
  done ;
  !p

let ( let@ ) f x = f x

let perf () =
  let use_fastpath = !use_fastpath in
  let use_framing = !use_framing in
  let transport = if !use_ssl then with_stunnel else with_connection in
  Printf.printf "1 thread non-persistent connections:         " ;
  let nonpersistent =
    let@ () = sample 10 in
    let@ () = per_nsec 0.1 in
    transport !ip !port (one ~use_fastpath ~use_framing false)
  in
  Printf.printf "%s RPCs/sec\n%!" (Normal_population.to_string nonpersistent) ;
  Printf.printf "1 thread non-persistent connections (query): " ;
  let nonpersistent_query =
    let@ () = sample 10 in
    let@ () = per_nsec 0.1 in
    transport !ip !port (query ~use_fastpath ~use_framing false)
  in
  Printf.printf "%s RPCs/sec\n%!"
    (Normal_population.to_string nonpersistent_query) ;
  Printf.printf "10 threads non-persistent connections:       " ;
  let thread_nonpersistent =
    let@ () = sample 10 in
    let@ () = threads 10 in
    let@ () = per_nsec 0.1 in
    transport !ip !port (one ~use_fastpath ~use_framing false)
  in
  Printf.printf "%s RPCs/sec\n%!"
    (Normal_population.to_string thread_nonpersistent) ;
  Printf.printf "1 thread persistent connection:              " ;
  let persistent =
    let@ () = sample 10 in
    let@ s = transport !ip !port in
    let@ () = per_nsec 0.1 in
    one ~use_fastpath ~use_framing true s
  in
  Printf.printf "%s RPCs/sec\n%!" (Normal_population.to_string persistent) ;
  Printf.printf "10 threads persistent connections:           " ;
  let thread_persistent =
    let@ () = sample 10 in
    let@ () = threads 10 in
    let@ s = transport !ip !port in
    let@ () = per_nsec 0.1 in
    one ~use_fastpath ~use_framing true s
  in
  Printf.printf "%s RPCs/sec\n%!" (Normal_population.to_string thread_persistent)

let send_close_conn ~use_fastpath ~use_framing keep_alive s =
  try
    Http_client.rpc ~use_fastpath s
      (Http.Request.make ~frame:use_framing ~version:"1.1" ~keep_alive
         ~user_agent ~body:"hello" Http.Get "/close_conn"
      ) (fun response s ->
        match response.Http.Response.content_length with
        | Some l ->
            let _ = Unixext.really_read_string s (Int64.to_int l) in
            Printf.printf "Received a response with %Ld bytes.\n" l ;
            exit 1
        | None ->
            Printf.printf "Need a content length\n" ;
            exit 1
    )
  with Unix.Unix_error (Unix.ECONNRESET, "read", "") as e ->
    Backtrace.is_important e ;
    let bt = Backtrace.get e in
    Debug.log_backtrace e bt

let logerr () =
  (* Send a request to the server to close connection instead of replying with
     an http request, force the error to be logged *)
  Printexc.record_backtrace true ;
  Debug.log_to_stdout () ;
  Debug.set_level Syslog.Debug ;
  let use_fastpath = !use_fastpath in
  let use_framing = !use_framing in
  let transport = if !use_ssl then with_stunnel else with_connection in
  let call () =
    let@ () = Backtrace.with_backtraces in
    let@ s = transport !ip !port in
    send_close_conn ~use_fastpath ~use_framing false s
  in
  match call () with `Ok () -> () | `Error (_, _) -> ()

let () =
  Arg.parse
    [
      ("-ip", Arg.Set_string ip, "IP to connect to")
    ; ("-p", Arg.Set_int port, "port to connect")
    ; ("-fast", Arg.Set use_fastpath, "use HTTP fastpath")
    ; ("-frame", Arg.Set use_framing, "use HTTP framing")
    ; ("--ssl", Arg.Set use_ssl, "use SSL rather than plaintext")
    ; ("--perf", Arg.Unit perf, "Collect performance stats")
    ; ("--logerr", Arg.Unit logerr, "Test log on error")
    ]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s\n" x)
    "A simple test HTTP client"
