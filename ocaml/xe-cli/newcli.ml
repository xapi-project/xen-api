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
let get_xapiport ssl =
  match !xapiport with
    None -> if ssl then 443 else 80
  | Some p -> p

let xeusessl = ref true
let allow_ssl_legacy = ref false
let ciphersuites = ref None
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
  let printer s = match !debug_channel with
    | Some c -> output_string c s
    | None -> () in
  Printf.kprintf printer fmt

(* usage message *)
exception Usage

let usage () =
  error "Usage: %s <cmd> [-s server] [-p port] ([-u username] [-pw password] or [-pwf <password file>]) <other arguments>\n" Sys.argv.(0);
  error "\nA full list of commands can be obtained by running \n\t%s help -s <server> -p <port>\n" Sys.argv.(0)

let is_localhost ip = ip = "127.0.0.1"

(* HTTP level bits and pieces *)

exception Http_parse_failure
let hdrs = ["content-length"; "cookie"; "connection"; "transfer-encoding"; "authorization"; "location"]

let end_of_string s from =
  String.sub s from ((String.length s)-from)

let strip_cr r =
  if String.length r=0 then raise Http_parse_failure;
  let last_char = String.sub r ((String.length r)-1) 1 in
  if last_char <> "\r" then raise Http_parse_failure;
  String.sub r 0 ((String.length r)-1)

let rec read_rest_of_headers ic =
  try
    let r = input_line ic in
    let r = strip_cr r in
    if r="" then [] else
      begin
        debug "read '%s'\n" r;
        let hdr = List.find (fun s -> String.startswith (s^": ") (String.lowercase_ascii r)) hdrs in
        let value = end_of_string r (String.length hdr + 2) in
        (hdr,value)::read_rest_of_headers ic
      end
  with
  | Not_found -> read_rest_of_headers ic
  | _ -> []

let parse_url url =
  if String.startswith "https://" url
  then
    let stripped = end_of_string url (String.length "https://") in
    let host, rest =
      let l =  String.split '/' stripped in
      List.hd l, List.tl l in
    (host,"/" ^ (String.concat "/" rest))
  else
    (!xapiserver,url)


(* Read the password file *)
let read_pwf () =
  try
    let ic = open_in !xapipasswordfile in
    try
      xapiuname := (input_line ic);
      xapipword := (input_line ic)
    with End_of_file ->
      error "Error: password file format: expecting username on the first line, password on the second line\n";
      exit 1
  with _ ->
    error "Error opening password file '%s'\n" !xapipasswordfile;
    exit 1


let parse_port (x: string) =
  try
    let p = int_of_string x in
    if p < 0 || p > 65535 then failwith "illegal";
    p
  with _ ->
    error "Port number must be an integer (0-65535)\n";
    raise Usage

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

  let set_keyword (k,v) =
    try
      (match k with
       | "server" -> xapiserver := v
       | "port" -> xapiport := Some (parse_port v)
       | "username" -> xapiuname := v
       | "password" -> xapipword := v
       | "passwordfile" -> xapipasswordfile := v
       | "nossl"   -> xeusessl := not(bool_of_string v)
       | "allow-ssl-legacy" -> allow_ssl_legacy := (bool_of_string v)
       | "ciphersuites" -> ciphersuites := Some v
       | "debug" -> xedebug := (try bool_of_string v with _ -> false)
       | "debugonfail" -> xedebugonfail := (try bool_of_string v with _ -> false)
       | _ -> raise Not_found);
      true
    with Not_found -> false in

  let parse_opt args =
    match args with
    | "-s" :: server :: xs -> Some ("server", server, xs)
    | "-p" :: port :: xs -> Some("port", port, xs)
    | "-u" :: uname :: xs -> Some("username", uname, xs)
    | "-pw" :: pw :: xs -> Some("password", pw, xs)
    | "-pwf" :: pwf :: xs -> Some("passwordfile", pwf, xs)
    | "--nossl" :: xs -> Some("nossl", "true", xs)
    | "--allow-ssl-legacy" :: xs -> Some("allow-ssl-legacy", "true", xs)
    | "--ciphersuites" :: c :: xs -> Some("ciphersuites", c, xs)
    | "--debug" :: xs -> Some("debug", "true", xs)
    | "--debug-on-fail" :: xs -> Some("debugonfail", "true", xs)
    | "-h" :: h :: xs -> Some("server", h, xs)
    | _ -> None in

  let parse_eql arg =
    try
      let eq = String.index arg '=' in
      let k = String.sub arg 0 eq in
      let v = String.sub arg (eq+1) (String.length arg - (eq+1)) in
      Some (k,v)
    with _ -> None in

  let rec process_args = function
    | [] -> []
    | args ->
      match parse_opt args with
      | Some(k, v, rest) ->
        if set_keyword(k, v) then process_args rest else process_eql args
      | None ->
        process_eql args
  and process_eql = function
    | [] -> []
    | arg :: args ->
      match parse_eql arg with
      | Some(k, v) when set_keyword(k,v) -> process_args args
      | _ -> arg :: process_args args in

  fun args ->
    let rcs = Options.read_rc() in
    let rcs_rest =
      List.map (fun (k,v) -> k^"="^v)
        (List.filter (fun (k, v) -> not (set_keyword (k,v))) rcs) in
    let extras =
      let extra_args = try Sys.getenv "XE_EXTRA_ARGS" with Not_found -> "" in
      let l = ref [] and pos = ref 0 and i = ref 0 in
      while !pos < String.length extra_args do
        if extra_args.[!pos] = ',' then (incr pos; i := !pos)
        else
        if !i >= String.length extra_args
        || extra_args.[!i] = ',' && extra_args.[!i-1] <> '\\' then
          (let seg = String.sub extra_args !pos (!i - !pos) in
           l := String.filter_chars seg ((<>) '\\') :: !l;
           incr i; pos := !i)
        else incr i
      done;
      List.rev !l  in
    let extras_rest = process_args extras in
    let help = ref false in
    let args' = List.filter (fun s -> s<>"-help" && s <> "--help") args in
    if List.length args' < List.length args then help := true;
    let args_rest = process_args args in
    if !help then raise Usage;
    let () =
      if !xapipasswordfile <> "" then read_pwf ();
      if !xedebug then debug_channel := Some stderr;
      if !xedebugonfail then begin
        let tmpfile, tmpch = Filename.open_temp_file "xe_debug" "tmp" in
        debug_file := Some tmpfile;
        debug_channel := Some tmpch
      end in
    args_rest @ extras_rest @ rcs_rest @ !reserve_args

let open_tcp_ssl server =
  let port = get_xapiport true in
  debug "Connecting via%s stunnel to [%s] port [%d]%s\n%!"
    (if !allow_ssl_legacy then " legacy-mode" else "") server port
    (match !ciphersuites with None -> "" | Some c -> " with ciphersuites "^c);
  Stunnel.set_legacy_protocol_and_ciphersuites_allowed !allow_ssl_legacy;
  (match !ciphersuites with
   | None -> ()
   | Some c -> (* Use only the specified ones, none of Stunnel's built-in defaults. *)
     Stunnel.set_good_ciphersuites c;
     Stunnel.set_legacy_ciphersuites ""
  );
  (* We don't bother closing fds since this requires our close_and_exec wrapper *)
  let x = Stunnel.connect ~use_fork_exec_helper:false
      ~write_to_log:(fun x -> debug "stunnel: %s\n%!" x)
      ~extended_diagnosis:(!debug_file <> None) server port in
  stunnel_processes := x :: !stunnel_processes;
  Unix.in_channel_of_descr x.Stunnel.fd, Unix.out_channel_of_descr x.Stunnel.fd

let open_tcp server =
  if !xeusessl && not(is_localhost server) then (* never use SSL on-host *)
    open_tcp_ssl server
  else (
    let host = Unix.gethostbyname server in
    let addr = host.Unix.h_addr_list.(0) in
    Unix.open_connection (Unix.ADDR_INET (addr,get_xapiport false))
  )

let open_channels () =
  if is_localhost !xapiserver then (
    try
      Unix.open_connection (Unix.ADDR_UNIX (Filename.concat "/var/lib/xcp" "xapi"))
    with _ ->
      open_tcp !xapiserver
  ) else
    open_tcp !xapiserver

let http_response_code x = match String.split ' ' x with
  | _ :: code :: _ -> int_of_string code
  | _ -> failwith "Bad response from HTTP server"

let copy_with_heartbeat ?(block=65536) in_ch out_ch heartbeat_fun =
  let buf = Bytes.make block '\000' in
  let last_heartbeat = ref (Unix.time()) in
  let finish = ref false in
  while not !finish do
    let bytes = input in_ch buf 0 block in
    if bytes <> 0 then
      output out_ch buf 0 bytes
    else begin
      flush out_ch;
      finish := true
    end;
    let now = Unix.time () in
    if now -. !last_heartbeat >= heartbeat_interval then begin
      heartbeat_fun ();
      last_heartbeat := now
    end
  done

exception Http_failure
exception Connect_failure
exception Protocol_version_mismatch of string
exception ClientSideError of string
exception Stunnel_exit of int * Unix.process_status
exception Unexpected_msg of message
exception Server_internal_error

let handle_unmarshal_failure ex ifd = match ex with
  | Unmarshal_failure (e, s) ->
    let s = s ^ Xapi_stdext_unix.Unixext.try_read_string ifd in
    debug "Read: %s\n" s;
    if String.length s >= 4 && String.uppercase_ascii (String.sub s 0 4) = "HTTP"
    then raise Server_internal_error
    else raise e
  | e -> raise e

let main_loop ifd ofd =
  (* Intially exchange version information *)
  let major', minor' =
    try unmarshal_protocol ifd with
    | Unmarshal_failure (_, "") -> raise Connect_failure
    | e -> handle_unmarshal_failure e ifd in
  let msg = Printf.sprintf "Server has protocol version %d.%d. Client has %d.%d" major' minor' major minor in
  debug "%s\n%!" msg;
  if major' <> major
  then raise (Protocol_version_mismatch msg);
  let with_heartbeat =
    major' * 10 + minor' >= int_of_float (heartbeat_version *. 10.) in
  let heartbeat_fun =
    if with_heartbeat then (fun () -> marshal ofd (Response Wait))
    else ignore in
  marshal_protocol ofd;

  let exit_code = ref None in
  while !exit_code = None do
    (* Wait for input asynchronously so that we can check the status
       of Stunnel every now and then, for better debug/dignosis.
    *)
    while (match Unix.select [ifd] [] [] 5.0 with
        | _ :: _, _, _ -> false
        | _ -> true) do ()
    done;
    let cmd =
      try unmarshal ifd
      with e -> handle_unmarshal_failure e ifd in
    debug "Read: %s\n%!" (string_of_message cmd); flush stderr;
    match cmd with
    | Command (Print x) -> print_endline x; flush stdout
    | Command (PrintStderr x) -> Printf.fprintf stderr "%s%!" x
    | Command (Debug x) -> debug "debug from server: %s\n%!" x
    | Command (Load x) ->
      begin
        try
          let fd = Unix.openfile x [ Unix.O_RDONLY ] 0 in
          marshal ofd (Response OK);
          let length = (Unix.stat x).Unix.st_size in
          marshal ofd (Blob (Chunk (Int32.of_int length)));
          let buffer = Bytes.make (1024 * 1024 * 10) '\000' in
          let left = ref length in
          while !left > 0 do
            let n = Unix.read fd buffer 0 (min (Bytes.length buffer) !left) in
            Xapi_stdext_unix.Unixext.really_write ofd (Bytes.unsafe_to_string buffer) 0 n;
            left := !left - n
          done;
          marshal ofd (Blob End);
          Unix.close fd
        with
        | e -> marshal ofd (Response Failed)
      end
    | Command (HttpConnect(url)) ->
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
        Printf.fprintf oc "CONNECT %s HTTP/1.0\r\ncontent-length: 0\r\n\r\n" path;
        flush oc;
        let resultline = input_line ic in
        let _ = read_rest_of_headers ic in
        (* Get the result header immediately *)
        begin match http_response_code resultline with
          | 200 ->
            if !tc_save = None then begin
              (* Remember the current terminal state so we can restore it *)
              let tc = Unix.tcgetattr Unix.stdin in
              (* Switch into a raw mode, passing through stuff like Control + C *)
              let tc' = {
                tc with
                Unix.c_ignbrk = false;
                Unix.c_brkint = false;
                Unix.c_parmrk = false;
                Unix.c_istrip = false;
                Unix.c_inlcr = false;
                Unix.c_igncr = false;
                Unix.c_icrnl = false;
                Unix.c_ixon = false;
                Unix.c_opost = false;
                Unix.c_echo = false;
                Unix.c_echonl = false;
                Unix.c_icanon = false;
                Unix.c_isig = false;
                (* IEXTEN? *)
                Unix.c_csize = 8;
                Unix.c_parenb = false;
                Unix.c_vmin = 0;
                Unix.c_vtime = 0;
              } in
              Unix.tcsetattr Unix.stdin Unix.TCSANOW tc';
              tc_save := Some tc
            end;
            let finished = ref false in
            let last_heartbeat = ref (Unix.time ()) in
            while not !finished do
              if !buf_local_start <> !buf_local_end then begin
                let b = Unix.write Unix.stdout buf_local
                    !buf_local_start (!buf_local_end - !buf_local_start)
                in
                buf_local_start := !buf_local_start + b;
                if !buf_local_start = !buf_local_end then
                  (buf_local_start := 0; buf_local_end := 0)
              end
              else if !buf_remote_start <> !buf_remote_end then begin
                let b = Unix.write fd buf_remote !buf_remote_start
                    (!buf_remote_end - !buf_remote_start) in
                buf_remote_start := !buf_remote_start + b;
                if !buf_remote_start = !buf_remote_end then
                  (buf_remote_start := 0; buf_remote_end := 0)
              end
              else if !final then finished := true
              else begin
                let r, _, _ =
                  Unix.select [Unix.stdin; fd] [] [] heartbeat_interval in
                let now = Unix.time () in
                if now  -. !last_heartbeat >= heartbeat_interval then begin
                  heartbeat_fun ();
                  last_heartbeat := now
                end;
                if List.mem Unix.stdin r then begin
                  let b = Unix.read Unix.stdin buf_remote
                      !buf_remote_end (block - !buf_remote_end) in
                  let i = ref !buf_remote_end in
                  while !i < !buf_remote_end + b && Char.code (Bytes.get buf_remote !i) <> 0x1d do incr i; done;
                  if !i < !buf_remote_end + b then final := true;
                  buf_remote_end := !i
                end;
                if List.mem fd r then begin
                  let b = Unix.read fd buf_local
                      !buf_local_end (block - !buf_local_end) in
                  buf_local_end := !buf_local_end + b
                end
              end
            done;
            marshal ofd (Response OK)
          | 404 ->
            Printf.fprintf stderr "Server replied with HTTP 404: the console is not available\n";
            marshal ofd (Response Failed)
          | x ->
            Printf.fprintf stderr "Server said: %s" resultline;
            marshal ofd (Response Failed)
        end in
      let delay = ref 0.1 in
      let rec keep_connection () =
        try
          let ic, oc = open_tcp server in
          delay := 0.1;
          Pervasiveext.finally
            (fun () -> connection ic oc)
            (fun () -> try close_in ic with _ -> ())
        with
        | Unix.Unix_error (_, _, _)
          when !delay <= long_connection_retry_timeout ->
          ignore (Unix.select [] [] [] !delay);
          delay := !delay *. 2.;
          keep_connection ()
        | e ->
          prerr_endline (Printexc.to_string e);
          marshal ofd (Response Failed) in
      keep_connection ();
      (match !tc_save with
       | Some tc ->
         Unix.tcsetattr Unix.stdin Unix.TCSANOW tc;
         print_endline "\r"
       | None -> ())
    | Command (HttpPut(filename, url)) ->
      begin
        try
          let rec doit url =
            let (server,path) = parse_url url in
            if not (Sys.file_exists filename) then
              raise (ClientSideError (Printf.sprintf "file '%s' does not exist" filename));
            (* If we can tell the file size then supply a content-length header--
               this will make the progress bar work. If we can't tell the file size
               (e.g. because it's a pipe) then we provide no header and rely on EOF
               to signal the upload is complete. *)
            let content_length =
              let stats = Unix.LargeFile.stat filename in
              if stats.Unix.LargeFile.st_kind = Unix.S_REG
              then Printf.sprintf "\r\nContent-length: %Ld" stats.Unix.LargeFile.st_size
              else "" in
            let file_ch = open_in_bin filename in
            let ic, oc = open_tcp server in
            debug "PUTting to path [%s]\n%!" path;
            Printf.fprintf oc "PUT %s HTTP/1.0%s\r\n\r\n" path content_length;
            flush oc;
            let resultline = input_line ic in
            let headers = read_rest_of_headers ic in
            (* Get the result header immediately *)
            match http_response_code resultline with
            | 200 ->
              Pervasiveext.finally
                (fun () ->
                   copy_with_heartbeat file_ch oc heartbeat_fun;
                   marshal ofd (Response OK))
                (fun () ->
                   (try close_in file_ch with _ -> ()))
            | 302 ->
              let newloc = List.assoc "location" headers in
              (try close_in ic with _ -> ()); (* Nb. Unix.close_connection only requires the in_channel *)
              doit newloc
            | _ -> failwith "Unhandled response code"
          in
          doit url
        with
        | ClientSideError msg ->
          marshal ofd (Response Failed);
          Printf.fprintf stderr "Operation failed. Error: %s\n" msg;
          exit_code := Some 1
        | e ->
          debug "HttpPut failure: %s\n%!" (Printexc.to_string e);
          (* Assume the server will figure out what's wrong and tell us over
             the normal communication channel *)
          marshal ofd (Response Failed)
      end
    | Command (HttpGet(filename, url)) ->
      begin
        try
          let rec doit url =
            let (server,path) = parse_url url in
            debug "Opening connection to server '%s' path '%s'\n%!" server path;
            let ic, oc = open_tcp server in
            Printf.fprintf oc "GET %s HTTP/1.0\r\n\r\n" path;
            flush oc;
            (* Get the result header immediately *)
            let resultline = input_line ic in
            debug "Got %s\n%!" resultline;
            match http_response_code resultline with
            | 200 ->
              let file_ch =
                if filename = "" then
                  Unix.out_channel_of_descr (Unix.dup Unix.stdout)
                else
                  try open_out_gen [Open_wronly; Open_creat; Open_excl] 0o600 filename
                  with e -> raise (ClientSideError (Printexc.to_string e))
              in
              while input_line ic <> "\r" do () done;
              Pervasiveext.finally
                (fun ()  ->
                   copy_with_heartbeat ic file_ch heartbeat_fun;
                   marshal ofd (Response OK))
                (fun () ->
                   (try close_in ic with _ -> ());
                   (try close_out file_ch with _ -> ()))
            | 302 ->
              let headers = read_rest_of_headers ic in
              let newloc = List.assoc "location" headers in
              (try close_in ic with _ -> ()); (* Nb. Unix.close_connection only requires the in_channel *)
              doit newloc
            | _ -> failwith "Unhandled response code"
          in
          doit url
        with
        | ClientSideError msg ->
          marshal ofd (Response Failed);
          Printf.fprintf stderr "Operation failed. Error: %s\n" msg;
          exit_code := Some 1
        | e ->
          debug "HttpGet failure: %s\n%!" (Printexc.to_string e);
          marshal ofd (Response Failed)
      end
    | Command Prompt ->
      let data = input_line stdin in
      marshal ofd (Blob (Chunk (Int32.of_int (String.length data))));
      Unix.write_substring ofd data 0 (String.length data) |> ignore;
      marshal ofd (Blob End)
    | Command (Error(code, params)) ->
      error "Error code: %s\n" code;
      error "Error parameters: %s\n" (String.concat ", " params)
    | Command (Exit c) ->
      exit_code := Some c
    | x ->
      raise (Unexpected_msg x)
  done;
  match !exit_code with Some c -> c | _ -> assert false

let main () =
  let exit_status = ref 1 in
  let _ =  try
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> exit 1));
      let xe, args =
        match Array.to_list Sys.argv with
        | h :: t -> h, t
        | _ -> assert false in
      if List.mem "-version" args then begin
        Printf.printf "ThinCLI protocol: %d.%d\n" major minor;
        exit 0
      end;

      let args = parse_args args in

      if List.length args < 1 then raise Usage else
        begin
          let ic, oc = open_channels () in
          Printf.fprintf oc "POST /cli HTTP/1.0\r\n";
          let args = args @ [("username="^ !xapiuname);("password="^ !xapipword)] in
          let args = String.concat "\n" args in
          Printf.fprintf oc "User-agent: xe-cli/Unix/%d.%d\r\n" major minor;
          Printf.fprintf oc "content-length: %d\r\n\r\n" (String.length args);
          Printf.fprintf oc "%s" args;
          flush_all ();

          let in_fd = Unix.descr_of_in_channel ic
          and out_fd = Unix.descr_of_out_channel oc in
          exit_status := main_loop in_fd out_fd
        end
    with
    | Usage ->
      exit_status := 0;
      usage ();
    | Not_a_cli_server ->
      error "Failed to contact a running management agent.\n";
      error "Try specifying a server name and port.\n";
      usage();
    | Protocol_version_mismatch x ->
      error "Protocol version mismatch: %s.\n" x;
      error "Try specifying a server name and port on the command-line.\n";
      usage();
    | Not_found ->
      error "Host '%s' not found.\n" !xapiserver;
    | Unix.Unix_error(err,fn,arg) ->
      error "Error: %s (calling %s %s)\n" (Unix.error_message err) fn arg
    | Connect_failure ->
      error "Unable to contact server. Please check server and port settings.\n"
    | Stunnel.Stunnel_binary_missing ->
      error "Please install the stunnel package or define the XE_STUNNEL environment variable to point to the binary.\n"
    | End_of_file ->
      error "Lost connection to the server.\n"
    | Unexpected_msg m ->
      error "Unexpected message from server: %s" (string_of_message m)
    | Server_internal_error ->
      error "Server internal error.\n"
    | Stunnel_exit (i, e) ->
      error "Stunnel process %d %s.\n" i
        (match e with
         | Unix.WEXITED c -> "existed with exit code " ^ string_of_int c
         | Unix.WSIGNALED c -> "killed by signal " ^ (Xapi_stdext_unix.Unixext.string_of_signal c)
         | Unix.WSTOPPED c -> "stopped by signal " ^ string_of_int c)
    | e ->
      error "Unhandled exception\n%s\n" (Printexc.to_string e) in
  List.iter (fun p ->
      if Sys.file_exists p.Stunnel.logfile then
        begin
          if !exit_status <> 0 then
            (debug "\nStunnel diagnosis:\n\n";
             try Stunnel.diagnose_failure p
             with e -> debug "%s\n" (Printexc.to_string e));
          try Unix.unlink p.Stunnel.logfile with _ -> ()
        end;
      Stunnel.disconnect ~wait:false ~force:true p) !stunnel_processes;
  begin match !debug_file, !debug_channel with
    | Some f, Some ch -> begin
        close_out ch;
        if !exit_status <> 0 then begin
          output_string stderr "\nDebug info:\n\n";
          output_string stderr (Xapi_stdext_unix.Unixext.string_of_file f)
        end;
        try Unix.unlink f with _ -> ()
      end
    | _ -> ()
  end;
  exit !exit_status

let _ = main ()
