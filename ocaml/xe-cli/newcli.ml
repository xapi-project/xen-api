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
(* New cli talking to the in-server cli interface *)

open Xapi_stdext_std.Xstringext
open Xapi_stdext_pervasives
open Cli_protocol

(* Param config priorities:
   explicit cmd option > XE_XXX env variable > ~/.xe rc file > default
*)

let xapiserver = ref "127.0.0.1"

let xapiuname = ref "root"

let xapipword = ref "null"

let xapipasswordfile = ref ""

let xapiport = ref None

let traceparent = ref None

let get_xapiport ssl =
  match !xapiport with None -> if ssl then 443 else 80 | Some p -> p

let xeusessl = ref true

let xedebug = ref false

let xedebugonfail = ref false

let stunnel_processes = ref []

let debug_channel = ref None

let debug_file = ref None

let heartbeat_version = 0.2

let heartbeat_interval = 300.

let long_connection_retry_timeout = 5.

let error fmt = Printf.fprintf stderr fmt

let debug fmt =
  let printer s =
    match !debug_channel with Some c -> output_string c s | None -> ()
  in
  Printf.ksprintf printer fmt

(* usage message *)
exception Usage

let usage () =
  error
    "Usage: %s <cmd> [-s server] [-p port] ([-u username] [-pw password] or \
     [-pwf <password file>]) [--traceparent traceparent] <other arguments>\n"
    Sys.argv.(0) ;
  error
    "\n\
     A full list of commands can be obtained by running \n\
     \t%s help -s <server> -p <port>\n"
    Sys.argv.(0)

let is_localhost ip = ip = "127.0.0.1"

(* HTTP level bits and pieces *)

exception Http_parse_failure

let hdrs =
  [
    "content-length"
  ; "cookie"
  ; "connection"
  ; "transfer-encoding"
  ; "authorization"
  ; "location"
  ]

let canonicalize path =
  let open Fpath in
  match Filename.is_relative path with
  | true ->
      (Sys.getcwd () |> v) // (path |> v) |> normalize
  | false ->
      path |> v |> normalize

let end_of_string s from = String.sub s from (String.length s - from)

let strip_cr r =
  if String.length r = 0 then raise Http_parse_failure ;
  let last_char = String.sub r (String.length r - 1) 1 in
  if last_char <> "\r" then raise Http_parse_failure ;
  String.sub r 0 (String.length r - 1)

let rec read_rest_of_headers ic =
  try
    let r = input_line ic in
    let r = strip_cr r in
    if r = "" then
      []
    else (
      debug "read '%s'\n" r ;
      let hdr =
        List.find
          (fun s -> String.startswith (s ^ ": ") (String.lowercase_ascii r))
          hdrs
      in
      let value = end_of_string r (String.length hdr + 2) in
      (hdr, value) :: read_rest_of_headers ic
    )
  with
  | Not_found ->
      read_rest_of_headers ic
  | _ ->
      []

let parse_url url =
  let parse uri =
    let ( let* ) = Option.bind in
    let* scheme = Uri.scheme uri in
    let* host = Uri.host uri in
    let path = Uri.path_and_query uri |> Uri.pct_decode in
    Some (scheme, host, path)
  in
  match parse (Uri.of_string url) with
  | Some ("https", host, path) ->
      (host, path)
  | _ ->
      debug "%s: could not parse '%s'" __FUNCTION__ url ;
      (!xapiserver, url)
  | exception e ->
      debug "%s: could not parse '%s': %s" __FUNCTION__ url
        (Printexc.to_string e) ;
      (!xapiserver, url)

(* Read the password file *)
let read_pwf () =
  try
    let ic = open_in !xapipasswordfile in
    try
      xapiuname := input_line ic ;
      xapipword := input_line ic
    with End_of_file ->
      error
        "Error: password file format: expecting username on the first line, \
         password on the second line\n" ;
      exit 1
  with _ ->
    error "Error opening password file '%s'\n" !xapipasswordfile ;
    exit 1

let parse_port (x : string) =
  try
    let p = int_of_string x in
    if p < 0 || p > 65535 then failwith "illegal" ;
    p
  with _ ->
    error "Port number must be an integer (0-65535)\n" ;
    raise Usage

let get_permit_filenames args =
  List.filter_map
    (fun arg ->
      match Astring.String.cut ~sep:"=" arg with
      | Some (_, v) -> (
        match String.trim v with "" -> None | _ -> Some (v |> canonicalize)
      )
      | _ ->
          None
    )
    args

(* Extract the arguments we're interested in. Return a list of the argumets we know *)
(* nothing about. These will get passed straight into the server *)
let parse_args =
  (* Set the key to the value. Return whether the key is one we know about *)
  (* compat mode is special as the argument is passed in two places. Once  *)
  (* at the top of the message to the cli server in order to indicate that *)
  (* we need to use 'geneva style' parsing - that is, allow key = value as *)
  (* opposed to key=value. Secondly, the key then gets passed along with   *)
  (* all the others to the operations. So we need to register it's there,  *)
  (* but not strip it                                                      *)
  let reserve_args = ref [] in
  let set_keyword (k, v) =
    try
      ( match k with
      | "server" ->
          xapiserver := v
      | "port" ->
          xapiport := Some (parse_port v)
      | "username" ->
          xapiuname := v
      | "password" ->
          xapipword := v
      | "passwordfile" ->
          xapipasswordfile := v
      | "nossl" ->
          xeusessl := not (bool_of_string v)
      | "debug" -> (
          xedebug := try bool_of_string v with _ -> false
        )
      | "debugonfail" -> (
          xedebugonfail := try bool_of_string v with _ -> false
        )
      | "traceparent" ->
          traceparent := Some v
      | _ ->
          raise Not_found
      ) ;
      true
    with Not_found -> false
  in
  let parse_opt args =
    match args with
    | "-s" :: server :: xs ->
        Some ("server", server, xs)
    | "-p" :: port :: xs ->
        Some ("port", port, xs)
    | "-u" :: uname :: xs ->
        Some ("username", uname, xs)
    | "-pw" :: pw :: xs ->
        Some ("password", pw, xs)
    | "-pwf" :: pwf :: xs ->
        Some ("passwordfile", pwf, xs)
    | "--nossl" :: xs ->
        Some ("nossl", "true", xs)
    | "--debug" :: xs ->
        Some ("debug", "true", xs)
    | "--debug-on-fail" :: xs ->
        Some ("debugonfail", "true", xs)
    | "-h" :: h :: xs ->
        Some ("server", h, xs)
    | "--traceparent" :: h :: xs ->
        Some ("traceparent", h, xs)
    | _ ->
        None
  in
  let rec process_args = function
    | [] ->
        []
    | args -> (
      match parse_opt args with
      | Some (k, v, rest) ->
          if set_keyword (k, v) then process_args rest else process_eql args
      | None ->
          process_eql args
    )
  and process_eql = function
    | [] ->
        []
    | arg :: args -> (
      match Astring.String.cut ~sep:"=" arg with
      | Some (k, v) when set_keyword (k, v) ->
          process_args args
      | _ ->
          arg :: process_args args
    )
  in
  fun args ->
    let rcs = Options.read_rc () in
    let rcs_rest =
      List.map
        (fun (k, v) -> k ^ "=" ^ v)
        (List.filter (fun (k, v) -> not (set_keyword (k, v))) rcs)
    in
    let extras =
      let extra_args =
        Option.value (Sys.getenv_opt "XE_EXTRA_ARGS") ~default:""
      in
      let l = ref [] and pos = ref 0 and i = ref 0 in
      while !pos < String.length extra_args do
        if extra_args.[!pos] = ',' then (
          incr pos ;
          i := !pos
        ) else if
            !i >= String.length extra_args
            || (extra_args.[!i] = ',' && extra_args.[!i - 1] <> '\\')
          then (
          let seg = String.sub extra_args !pos (!i - !pos) in
          l := String.filter_chars seg (( <> ) '\\') :: !l ;
          incr i ;
          pos := !i
        ) else
          incr i
      done ;
      List.rev !l
    in
    let extras_rest = process_args extras in
    (*if traceparent is set as env var update it after we process the extras.*)
    Option.iter
      (fun tp -> traceparent := Some tp)
      (Sys.getenv_opt Tracing.EnvHelpers.traceparent_key) ;
    let help = ref false in
    let args' = List.filter (fun s -> s <> "-help" && s <> "--help") args in
    if List.length args' < List.length args then help := true ;
    let args_rest = process_args args in
    if !help then raise Usage ;
    let () =
      if !xapipasswordfile <> "" then read_pwf () ;
      if !xedebug then debug_channel := Some stderr ;
      if !xedebugonfail then (
        let tmpfile, tmpch = Filename.open_temp_file "xe_debug" "tmp" in
        debug_file := Some tmpfile ;
        debug_channel := Some tmpch
      )
    in
    (args_rest @ extras_rest @ rcs_rest @ !reserve_args, !traceparent)

let exit_status = ref 1

let with_open_tcp_ssl server f =
  let port = get_xapiport true in
  debug "Connecting via stunnel to [%s] port [%d]\n%!" server port ;
  (* We don't bother closing fds since this requires our close_and_exec wrapper *)
  let open Safe_resources in
  Stunnel.with_connect ~use_fork_exec_helper:false
    ~write_to_log:(fun x -> debug "stunnel: %s\n%!" x)
    ~verify_cert:None ~extended_diagnosis:(!debug_file <> None) server port
  @@ fun x ->
  let x = Stunnel.move_out_exn x in
  let ic = Unix.in_channel_of_descr (Unix.dup Unixfd.(!(x.Stunnel.fd))) in
  let oc = Unix.out_channel_of_descr (Unix.dup Unixfd.(!(x.Stunnel.fd))) in
  stunnel_processes := (x, ic, oc) :: !stunnel_processes ;
  f (ic, oc)

let with_open_tcp server f =
  if !xeusessl && not (is_localhost server) then (* never use SSL on-host *)
    with_open_tcp_ssl server f
  else
    let host = Scanf.ksscanf server (fun _ _ -> server) "[%s@]" Fun.id in
    let port = get_xapiport false in
    let addr =
      match Unix.getaddrinfo host (string_of_int port) [] with
      | [] ->
          error "No addrinfo found for host: %s, port: %d" host port ;
          raise Not_found
      | addrinfo :: _ ->
          addrinfo.Unix.ai_addr
    in
    let open Safe_resources in
    Unixfd.with_open_connection ~loc:__LOC__ addr @@ fun ufd ->
    Unixfd.with_channels ufd f

let with_open_channels f =
  let wrap chs =
    try Ok (f chs) with e -> Backtrace.is_important e ; Error e
  in
  let result =
    if is_localhost !xapiserver then
      try
        let open Safe_resources in
        Unixfd.with_open_connection
          (Unix.ADDR_UNIX (Filename.concat "/var/lib/xcp" "xapi"))
          ~loc:__LOC__
        @@ fun chs -> Unixfd.with_channels chs wrap
      with _ -> with_open_tcp !xapiserver wrap
    else
      with_open_tcp !xapiserver wrap
  in
  match result with Ok r -> r | Error e -> raise e

let http_response_code x =
  match String.split ' ' x with
  | _ :: code :: _ ->
      int_of_string code
  | _ ->
      failwith "Bad response from HTTP server"

let copy_with_heartbeat ?(block = 65536) in_ch out_ch heartbeat_fun =
  let buf = Bytes.make block '\000' in
  let last_heartbeat = ref (Unix.time ()) in
  let finish = ref false in
  while not !finish do
    let bytes = input in_ch buf 0 block in
    if bytes <> 0 then
      output out_ch buf 0 bytes
    else (
      flush out_ch ;
      finish := true
    ) ;
    let now = Unix.time () in
    if now -. !last_heartbeat >= heartbeat_interval then (
      heartbeat_fun () ;
      last_heartbeat := now
    )
  done

exception Connect_failure

exception Protocol_version_mismatch of string

exception ClientSideError of string

exception Unexpected_msg of message

exception Server_internal_error

exception Filename_not_permitted of string

let handle_unmarshal_failure ex ifd =
  match ex with
  | Unmarshal_failure (e, s) ->
      let s = s ^ Xapi_stdext_unix.Unixext.try_read_string ifd in
      debug "Read: %s\n" s ;
      if
        String.length s >= 4
        && String.uppercase_ascii (String.sub s 0 4) = "HTTP"
      then
        raise Server_internal_error
      else
        raise e
  | e ->
      raise e

let assert_filename_permitted ?(permit_cwd = false) permitted_filenames filename
    =
  let permitted_filenames =
    match permit_cwd with
    | true ->
        Filename.current_dir_name |> canonicalize |> fun x ->
        x :: permitted_filenames
    | _ ->
        permitted_filenames
  in
  let requested_file = canonicalize filename in
  match List.mem requested_file permitted_filenames with
  | false ->
      let error_message =
        Printf.sprintf
          "Blocked upload/download of the file %s, which was requested by the \
           server. The file name was not present on the command line\n"
          filename
      in
      raise (Filename_not_permitted error_message)
  | _ ->
      ()

let main_loop ifd ofd permitted_filenames =
  (* Intially exchange version information *)
  let major', minor' =
    try unmarshal_protocol ifd with
    | Unmarshal_failure (_, "") ->
        raise Connect_failure
    | e ->
        handle_unmarshal_failure e ifd
  in
  let msg =
    Printf.sprintf "Server has protocol version %d.%d. Client has %d.%d" major'
      minor' major minor
  in
  debug "%s\n%!" msg ;
  if major' <> major then
    raise (Protocol_version_mismatch msg) ;
  let with_heartbeat =
    (major' * 10) + minor' >= int_of_float (heartbeat_version *. 10.)
  in
  let heartbeat_fun =
    if with_heartbeat then
      fun () -> marshal ofd (Response Wait)
    else
      ignore
  in
  marshal_protocol ofd ;
  let exit_code = ref None in
  while !exit_code = None do
    let cmd = try unmarshal ifd with e -> handle_unmarshal_failure e ifd in
    debug "Read: %s\n%!" (string_of_message cmd) ;
    flush stderr ;
    match cmd with
    | Command (Print x) ->
        print_endline x ; flush stdout
    | Command (PrintStderr x) ->
        Printf.fprintf stderr "%s%!" x
    | Command (Debug x) ->
        debug "debug from server: %s\n%!" x
    | Command (Load x) -> (
        assert_filename_permitted permitted_filenames x ;
        try
          let fd = Unix.openfile x [Unix.O_RDONLY] 0 in
          marshal ofd (Response OK) ;
          let length = (Unix.stat x).Unix.st_size in
          marshal ofd (Blob (Chunk (Int32.of_int length))) ;
          let buffer = Bytes.make (1024 * 1024 * 10) '\000' in
          let left = ref length in
          while !left > 0 do
            let n = Unix.read fd buffer 0 (min (Bytes.length buffer) !left) in
            Xapi_stdext_unix.Unixext.really_write ofd
              (Bytes.unsafe_to_string buffer)
              0 n ;
            left := !left - n
          done ;
          marshal ofd (Blob End) ;
          Unix.close fd
        with
        | Unix.Unix_error (Unix.EPIPE, _, _) ->
            raise (ClientSideError (Printf.sprintf "Failed to upload file %s" x))
        | _ ->
            marshal ofd (Response Failed)
      )
    | Command (HttpConnect url) -> (
        let server, path = parse_url url in
        (* The releatively complex design here helps to buffer input/output
           	         when the underlying connection temporarily breaks, hence provides
           	         seemingly continous connection. *)
        let block = 65536 in
        let buf_local = Bytes.make block '\000' in
        let buf_local_end = ref 0 in
        let buf_local_start = ref 0 in
        let buf_remote = Bytes.make block '\000' in
        let buf_remote_end = ref 0 in
        let buf_remote_start = ref 0 in
        let final = ref false in
        let tc_save = ref None in
        let connection ic oc =
          let fd = Unix.descr_of_out_channel oc in
          Printf.fprintf oc "CONNECT %s HTTP/1.0\r\ncontent-length: 0\r\n\r\n"
            path ;
          flush oc ;
          let resultline = input_line ic in
          let _ = read_rest_of_headers ic in
          (* Get the result header immediately *)
          match http_response_code resultline with
          | 200 ->
              if !tc_save = None then (
                (* Remember the current terminal state so we can restore it *)
                let tc = Unix.tcgetattr Unix.stdin in
                (* Switch into a raw mode, passing through stuff like Control + C *)
                let tc' =
                  {
                    tc with
                    Unix.c_ignbrk= false
                  ; Unix.c_brkint= false
                  ; Unix.c_parmrk= false
                  ; Unix.c_istrip= false
                  ; Unix.c_inlcr= false
                  ; Unix.c_igncr= false
                  ; Unix.c_icrnl= false
                  ; Unix.c_ixon= false
                  ; Unix.c_opost= false
                  ; Unix.c_echo= false
                  ; Unix.c_echonl= false
                  ; Unix.c_icanon= false
                  ; Unix.c_isig= false
                  ; (* IEXTEN? *)
                    Unix.c_csize= 8
                  ; Unix.c_parenb= false
                  ; Unix.c_vmin= 0
                  ; Unix.c_vtime= 0
                  }
                in
                Unix.tcsetattr Unix.stdin Unix.TCSANOW tc' ;
                tc_save := Some tc
              ) ;
              let finished = ref false in
              let last_heartbeat = ref (Unix.time ()) in
              while not !finished do
                if !buf_local_start <> !buf_local_end then (
                  let b =
                    Unix.write Unix.stdout buf_local !buf_local_start
                      (!buf_local_end - !buf_local_start)
                  in
                  buf_local_start := !buf_local_start + b ;
                  if !buf_local_start = !buf_local_end then (
                    buf_local_start := 0 ;
                    buf_local_end := 0
                  )
                ) else if !buf_remote_start <> !buf_remote_end then (
                  let b =
                    Unix.write fd buf_remote !buf_remote_start
                      (!buf_remote_end - !buf_remote_start)
                  in
                  buf_remote_start := !buf_remote_start + b ;
                  if !buf_remote_start = !buf_remote_end then (
                    buf_remote_start := 0 ;
                    buf_remote_end := 0
                  )
                ) else if !final then
                  finished := true
                else
                  let r, _, _ =
                    Xapi_stdext_unix.Unixext.select [Unix.stdin; fd] [] []
                      heartbeat_interval
                  in
                  let now = Unix.time () in
                  if now -. !last_heartbeat >= heartbeat_interval then (
                    heartbeat_fun () ;
                    last_heartbeat := now
                  ) ;
                  if List.mem Unix.stdin r then (
                    let b =
                      Unix.read Unix.stdin buf_remote !buf_remote_end
                        (block - !buf_remote_end)
                    in
                    let i = ref !buf_remote_end in
                    while
                      !i < !buf_remote_end + b
                      && Char.code (Bytes.get buf_remote !i) <> 0x1d
                    do
                      incr i
                    done ;
                    if !i < !buf_remote_end + b then final := true ;
                    buf_remote_end := !i
                  ) ;
                  if List.mem fd r then
                    let b =
                      Unix.read fd buf_local !buf_local_end
                        (block - !buf_local_end)
                    in
                    buf_local_end := !buf_local_end + b
              done ;
              marshal ofd (Response OK)
          | 404 ->
              Printf.fprintf stderr
                "Server replied with HTTP 404: the console is not available\n" ;
              marshal ofd (Response Failed)
          | _ ->
              Printf.fprintf stderr "Server said: %s" resultline ;
              marshal ofd (Response Failed)
        in
        let delay = ref 0.1 in
        let rec keep_connection () =
          try
            with_open_tcp server @@ fun (ic, oc) ->
            delay := 0.1 ;
            connection ic oc
          with
          | Unix.Unix_error (_, _, _)
            when !delay <= long_connection_retry_timeout ->
              Unix.sleepf !delay ;
              delay := !delay *. 2. ;
              keep_connection ()
          | e ->
              prerr_endline (Printexc.to_string e) ;
              marshal ofd (Response Failed)
        in
        keep_connection () ;
        match !tc_save with
        | Some tc ->
            Unix.tcsetattr Unix.stdin Unix.TCSANOW tc ;
            print_endline "\r"
        | None ->
            ()
      )
    | Command (HttpPut (filename, url)) -> (
        assert_filename_permitted permitted_filenames filename ;
        try
          let rec doit url =
            let server, path = parse_url url in
            if not (Sys.file_exists filename) then
              raise
                (ClientSideError
                   (Printf.sprintf "file '%s' does not exist" filename)
                ) ;
            (* If we can tell the file size then supply a content-length header--
               this will make the progress bar work. If we can't tell the file size
               (e.g. because it's a pipe) then we provide no header and rely on EOF
               to signal the upload is complete. *)
            let content_length =
              let stats = Unix.LargeFile.stat filename in
              if stats.Unix.LargeFile.st_kind = Unix.S_REG then
                Printf.sprintf "\r\nContent-length: %Ld"
                  stats.Unix.LargeFile.st_size
              else
                ""
            in
            let file_ch = open_in_bin filename in
            with_open_tcp server @@ fun (ic, oc) ->
            debug "PUTting to path [%s]\n%!" path ;
            Printf.fprintf oc "PUT %s HTTP/1.0%s\r\n\r\n" path content_length ;
            flush oc ;
            let resultline = input_line ic in
            let headers = read_rest_of_headers ic in
            (* Get the result header immediately *)
            match http_response_code resultline with
            | 200 ->
                Pervasiveext.finally
                  (fun () ->
                    copy_with_heartbeat file_ch oc heartbeat_fun ;
                    marshal ofd (Response OK)
                  )
                  (fun () -> try close_in file_ch with _ -> ())
            | 302 ->
                let newloc = List.assoc "location" headers in
                (try close_in ic with _ -> ()) ;
                (* Unixfd.with_connection requires both channels to be closed *)
                close_in_noerr ic ;
                close_out_noerr oc ;
                (* recursive call here, had to close channels on our own *)
                doit newloc
            | _ ->
                failwith "Unhandled response code"
          in
          doit url
        with
        | ClientSideError msg ->
            marshal ofd (Response Failed) ;
            Printf.fprintf stderr "Operation failed. Error: %s\n" msg ;
            exit_code := Some 1
        | e ->
            debug "HttpPut failure: %s\n%!" (Printexc.to_string e) ;
            (* Assume the server will figure out what's wrong and tell us over
               the normal communication channel *)
            marshal ofd (Response Failed)
      )
    | Command (HttpGet (filename, url)) -> (
      try
        let rec doit url =
          let server, path = parse_url url in
          debug "Opening connection to server '%s' path '%s'\n%!" server path ;
          with_open_tcp server @@ fun (ic, oc) ->
          Printf.fprintf oc "GET %s HTTP/1.0\r\n\r\n" path ;
          flush oc ;
          (* Get the result header immediately *)
          let resultline = input_line ic in
          debug "Got %s\n%!" resultline ;
          match http_response_code resultline with
          | 200 ->
              let file_ch =
                if filename = "" then
                  Unix.out_channel_of_descr (Unix.dup Unix.stdout)
                else (
                  assert_filename_permitted ~permit_cwd:true permitted_filenames
                    filename ;
                  try
                    open_out_gen
                      [Open_wronly; Open_creat; Open_excl]
                      0o600 filename
                  with e -> raise (ClientSideError (Printexc.to_string e))
                )
              in
              while input_line ic <> "\r" do
                ()
              done ;
              Pervasiveext.finally
                (fun () ->
                  copy_with_heartbeat ic file_ch heartbeat_fun ;
                  marshal ofd (Response OK)
                )
                (fun () -> try close_out file_ch with _ -> ())
          | 302 ->
              let headers = read_rest_of_headers ic in
              let newloc = List.assoc "location" headers in
              (* see above about Unixfd.with_connection *)
              close_in_noerr ic ; close_out_noerr oc ; doit newloc
          | _ ->
              failwith "Unhandled response code"
        in
        doit url
      with
      | ClientSideError msg ->
          marshal ofd (Response Failed) ;
          Printf.fprintf stderr "Operation failed. Error: %s\n" msg ;
          exit_code := Some 1
      | e -> (
        match e with
        | Filename_not_permitted _ ->
            raise e
        | _ ->
            debug "HttpGet failure: %s\n%!" (Printexc.to_string e) ;
            marshal ofd (Response Failed)
      )
    )
    | Command Prompt ->
        let data = input_line stdin in
        marshal ofd (Blob (Chunk (Int32.of_int (String.length data)))) ;
        Unix.write_substring ofd data 0 (String.length data) |> ignore ;
        marshal ofd (Blob End)
    | Command (Error (code, params)) ->
        error "Error code: %s\n" code ;
        error "Error parameters: %s\n" (String.concat ", " params)
    | Command (Exit c) ->
        exit_code := Some c
    | x ->
        raise (Unexpected_msg x)
  done ;
  match !exit_code with Some c -> c | _ -> assert false

let main () =
  let _ =
    try
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
      Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1)) ;
      let _xe, args =
        match Array.to_list Sys.argv with h :: t -> (h, t) | _ -> assert false
      in
      if List.mem "-version" args then (
        Printf.printf "ThinCLI protocol: %d.%d\n" major minor ;
        exit 0
      ) ;
      let args, traceparent = parse_args args in
      (* All the named args are taken as permitted filename to be uploaded *)
      let permitted_filenames = get_permit_filenames args in
      if args = [] then
        raise Usage
      else
        with_open_channels @@ fun (ic, oc) ->
        Printf.fprintf oc "POST /cli HTTP/1.0\r\n" ;
        let args =
          args @ ["username=" ^ !xapiuname; "password=" ^ !xapipword]
        in
        let args = String.concat "\n" args in
        Printf.fprintf oc "User-agent: xe-cli/Unix/%d.%d\r\n" major minor ;
        Printf.fprintf oc "originator: cli\r\n" ;
        Option.iter (Printf.fprintf oc "traceparent: %s\r\n") traceparent ;
        Printf.fprintf oc "content-length: %d\r\n\r\n" (String.length args) ;
        Printf.fprintf oc "%s" args ;
        flush_all () ;
        let in_fd = Unix.descr_of_in_channel ic
        and out_fd = Unix.descr_of_out_channel oc in
        exit_status := main_loop in_fd out_fd permitted_filenames
    with
    | Usage ->
        exit_status := 0 ;
        usage ()
    | Not_a_cli_server ->
        error "Failed to contact a running management agent.\n" ;
        error "Try specifying a server name and port.\n" ;
        usage ()
    | Protocol_version_mismatch x ->
        error "Protocol version mismatch: %s.\n" x ;
        error "Try specifying a server name and port on the command-line.\n" ;
        usage ()
    | Not_found ->
        error "Host '%s' not found.\n" !xapiserver
    | Unix.Unix_error (err, fn, arg) ->
        error "Error: %s (calling %s %s)\n" (Unix.error_message err) fn arg
    | Connect_failure ->
        error
          "Unable to contact server. Please check server and port settings.\n"
    | Stunnel.Stunnel_binary_missing ->
        error
          "Please install the stunnel package or define the XE_STUNNEL \
           environment variable to point to the binary.\n"
    | End_of_file ->
        error "Lost connection to the server.\n"
    | Unexpected_msg m ->
        error "Unexpected message from server: %s" (string_of_message m)
    | Server_internal_error ->
        error "Server internal error.\n"
    | Filename_not_permitted e ->
        error "File not permitted: %s.\n" e
    | ClientSideError e ->
        error "Client Side error: %s.\n" e
    | e ->
        error "Unhandled exception\n%s\n" (Printexc.to_string e)
  in
  List.iter
    (fun (x, ic, oc) ->
      close_out_noerr oc ;
      close_in_noerr ic ;
      if Sys.file_exists x.Stunnel.logfile then (
        if !exit_status <> 0 then (
          debug "\nStunnel diagnosis:\n\n" ;
          try Stunnel.diagnose_failure x
          with e -> debug "%s\n" (Printexc.to_string e)
        ) ;
        try Unix.unlink x.Stunnel.logfile with _ -> ()
      ) ;
      Stunnel.disconnect ~wait:false ~force:true x
    )
    !stunnel_processes ;
  ( match (!debug_file, !debug_channel) with
  | Some f, Some ch -> (
      close_out ch ;
      if !exit_status <> 0 then (
        output_string stderr "\nDebug info:\n\n" ;
        output_string stderr (Xapi_stdext_unix.Unixext.string_of_file f)
      ) ;
      try Unix.unlink f with _ -> ()
    )
  | _ ->
      ()
  ) ;
  exit !exit_status

let _ = main ()
