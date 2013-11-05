(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Common
open Cmdliner
open Lwt

module Impl = Vhd.Make(Vhd_lwt)
open Impl
open Vhd
open Vhd_lwt

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let get common filename key =
  try
    let filename = require "filename" filename in
    let key = require "key" key in
    let t =
      Vhd_IO.openfile ~path:common.path filename false >>= fun t ->
      let result = Vhd.Field.get t key in
      Vhd_IO.close t >>= fun () ->
      return result in
    match Lwt_main.run t with
    | Some v ->
      Printf.printf "%s\n" v;
      `Ok ()
    | None -> raise Not_found
  with
    | Failure x ->
      `Error(true, x)
    | Not_found ->
      `Error(true, Printf.sprintf "Unknown key. Known keys are: %s" (String.concat ", " Vhd.Field.list))

let info common filename =
  try
    let filename = require "filename" filename in
    let t =
      Vhd_IO.openfile ~path:common.path filename false >>= fun t ->
      let all = List.map (fun f ->
        match Vhd.Field.get t f with
        | Some v -> [ f; v ]
        | None -> assert false
      ) Vhd.Field.list in
      print_table ["field"; "value"] all;
      return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    `Error(true, x)

let create common filename size parent =
  try
    begin let filename = require "filename" filename in
    match parent, size with
    | None, None -> failwith "Please supply either a size or a parent"
    | None, Some size ->
      let size = parse_size size in
      let t =
        Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
        Vhd_IO.close vhd in
      Lwt_main.run t
    | Some parent, None ->
      let t =
        Vhd_IO.openfile ~path:common.path parent false >>= fun parent ->
        Vhd_IO.create_difference ~filename ~parent () >>= fun vhd ->
        Vhd_IO.close parent >>= fun () ->
        Vhd_IO.close vhd >>= fun () ->
        return () in
      Lwt_main.run t
    | Some parent, Some size ->
      failwith "Overriding the size in a child node not currently implemented"
    end;
     `Ok ()
  with Failure x ->
    `Error(true, x)

let check common filename =
  try
    let filename = require "filename" filename in
    let t =
      Vhd_IO.openfile ~path:common.path filename false >>= fun vhd ->
      Vhd.check_overlapping_blocks vhd;
      return () in
    Lwt_main.run t;
    `Ok ()
  with Failure x ->
    `Error(true, x)

module P = Progress_bar(Int64)

let console_progress_bar total_work =
  let p = P.create 80 0L total_work in
  fun work_done ->
    let progress_updated = P.update p work_done in
    if progress_updated then P.print_bar p;
    if work_done = total_work then Printf.printf "\n%!"

let no_progress_bar _ _ = ()

let stream_human common _ s _ ?(progress = no_progress_bar) () =
  (* How much space will we need for the sector numbers? *)
  let sectors = Int64.(shift_right (add s.size.total 511L) sector_shift) in
  let decimal_digits = int_of_float (ceil (log10 (Int64.to_float sectors))) in
  Printf.printf "# stream summary:\n";
  Printf.printf "# size of the final artifact: %Ld\n" s.size.total;
  Printf.printf "# size of metadata blocks:    %Ld\n" s.size.metadata;
  Printf.printf "# size of empty space:        %Ld\n" s.size.empty;
  Printf.printf "# size of referenced blocks:  %Ld\n" s.size.copy;
  Printf.printf "# offset : contents\n";
  fold_left (fun sector x ->
    Printf.printf "%s: %s\n"
      (padto ' ' decimal_digits (Int64.to_string sector))
      (Element.to_string x);
    return (Int64.add sector (Element.len x))
  ) 0L s.elements >>= fun _ ->
  Printf.printf "# end of stream\n";
  return None

let stream_nbd common c s prezeroed ?(progress = no_progress_bar) () =
  let c = { Nbd_lwt_client.read = c.Channels.really_read; write = c.Channels.really_write } in

  Nbd_lwt_client.negotiate c >>= fun (server, size, flags) ->
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->
  expand_copy s >>= fun s ->

  fold_left (fun (sector, work_done) x ->
    ( match x with
      | Element.Sectors data ->
        Nbd_lwt_client.write server data (Int64.mul sector 512L) >>= fun () ->
        return Int64.(of_int (Cstruct.len data))
      | Element.Empty n -> (* must be prezeroed *)
        assert prezeroed;
        return 0L
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Element.to_string x))) ) >>= fun work ->
    let sector = Int64.add sector (Element.len x) in
    let work_done = Int64.add work_done work in
    p work_done;
    return (sector, work_done)
  ) (0L, 0L) s.elements >>= fun _ ->
  p total_work;

  return (Some total_work)

let stream_chunked common c s prezeroed ?(progress = no_progress_bar) () =
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->
  expand_copy s >>= fun s ->

  let header = Cstruct.create Chunked.sizeof in
  fold_left (fun(sector, work_done) x ->
    ( match x with
      | Element.Sectors data ->
        let t = { Chunked.offset = Int64.(mul sector 512L); data } in
        Chunked.marshal header t;
        c.Channels.really_write header >>= fun () ->
        c.Channels.really_write data >>= fun () ->
        return Int64.(of_int (Cstruct.len data))
      | Element.Empty n -> (* must be prezeroed *)
        assert prezeroed;
        return 0L
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Element.to_string x))) ) >>= fun work ->
    let sector = Int64.add sector (Element.len x) in
    let work_done = Int64.add work_done work in
    p work_done;
    return (sector, work_done)
  ) (0L, 0L) s.elements >>= fun _ ->
  p total_work;

  (* Send the end-of-stream marker *)
  Chunked.marshal header { Chunked.offset = 0L; data = Cstruct.create 0 };
  c.Channels.really_write header >>= fun () ->

  return (Some total_work)

let stream_raw common c s prezeroed ?(progress = no_progress_bar) () =
  (* Work to do is: non-zero data to write + empty sectors if the
     target is not prezeroed *)
  let total_work = Int64.(add (add s.size.metadata s.size.copy) (if prezeroed then 0L else s.size.empty)) in
  let p = progress total_work in

  ( if not prezeroed then expand_empty s else return s ) >>= fun s ->
  expand_copy s >>= fun s ->

  fold_left (fun work_done x ->
    (match x with
      | Element.Sectors data ->
        c.Channels.really_write data >>= fun () ->
        return Int64.(of_int (Cstruct.len data))
      | Element.Empty n -> (* must be prezeroed *)
        c.Channels.skip (Int64.(mul n 512L)) >>= fun () ->
        assert prezeroed;
        return 0L
      | _ -> fail (Failure (Printf.sprintf "unexpected stream element: %s" (Element.to_string x))) ) >>= fun work ->
    let work_done = Int64.add work_done work in
    p work_done;
    return work_done
  ) 0L s.elements >>= fun _ ->
  p total_work;

  return (Some total_work)

type protocol = Nbd | Chunked | Human | NoProtocol
let protocol_of_string = function
  | "nbd" -> Nbd | "chunked" -> Chunked | "human" -> Human | "none" -> NoProtocol
  | x -> failwith (Printf.sprintf "Unsupported protocol: %s" x)
let string_of_protocol = function
  | Nbd -> "nbd" | Chunked -> "chunked" | Human -> "human" | NoProtocol -> "none"

type endpoint =
  | Stdout
  | Null
  | File_descr of Lwt_unix.file_descr
  | Sockaddr of Lwt_unix.sockaddr
  | File of string
  | Http of Uri.t
  | Https of Uri.t

let endpoint_of_string = function
  | "stdout:" -> return Stdout
  | "null:" -> return Null
  | uri ->
    let uri' = Uri.of_string uri in
    begin match Uri.scheme uri' with
    | Some "fd" ->
      return (File_descr (Uri.path uri' |> int_of_string |> file_descr_of_int |> Lwt_unix.of_unix_file_descr))
    | Some "tcp" ->
      let host = match Uri.host uri' with None -> failwith "Please supply a host in the URI" | Some host -> host in
      let port = match Uri.port uri' with None -> failwith "Please supply a port in the URI" | Some port -> port in
      Lwt_unix.gethostbyname host >>= fun host_entry ->
      return (Sockaddr(Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), port)))
    | Some "unix" ->
      return (Sockaddr(Lwt_unix.ADDR_UNIX(Uri.path uri')))
    | Some "file" ->
      return (File(Uri.path uri'))
    | Some "http" ->
      return (Http uri')
    | Some "https" ->
      return (Https uri')
    | Some x ->
      fail (Failure (Printf.sprintf "Unknown URI scheme: %s" x))
    | None ->
      fail (Failure (Printf.sprintf "Failed to parse URI: %s" uri))
    end

let socket sockaddr =
  let family = match sockaddr with
  | Lwt_unix.ADDR_INET(_, _) -> Unix.PF_INET
  | Lwt_unix.ADDR_UNIX _ -> Unix.PF_UNIX in
  Lwt_unix.socket family Unix.SOCK_STREAM 0

let colon = Re_str.regexp_string ":"

let make_stream common source relative_to source_format destination_format =
  match source_format, destination_format with
  | "hybrid", "raw" ->
    (* expect source to be block_device:vhd *)
    begin match Re_str.bounded_split colon source 2 with
    | [ raw; vhd ] ->
      Vhd_IO.openfile ~path:common.path vhd false >>= fun t ->
      Vhd_lwt.Fd.openfile raw false >>= fun raw ->
      ( match relative_to with None -> return None | Some f -> Vhd_IO.openfile ~path:common.path f false >>= fun t -> return (Some t) ) >>= fun from ->
      Vhd_input.hybrid ?from raw t
    | _ ->
      fail (Failure (Printf.sprintf "Failed to parse hybrid source: %s (expected raw_disk|vhd_disk)" source))
    end
  | "vhd", "vhd" ->
    Vhd_IO.openfile ~path:common.path source false >>= fun t ->
    ( match relative_to with None -> return None | Some f -> Vhd_IO.openfile ~path:common.path f false >>= fun t -> return (Some t) ) >>= fun from ->
    Vhd_input.vhd ?from t
  | "vhd", "raw" ->
    Vhd_IO.openfile ~path:common.path source false >>= fun t ->
    ( match relative_to with None -> return None | Some f -> Vhd_IO.openfile ~path:common.path f false >>= fun t -> return (Some t) ) >>= fun from ->
    Vhd_input.raw ?from t
  | "raw", "vhd" ->
    Raw_IO.openfile source false >>= fun t ->
    Raw_input.vhd t
  | "raw", "raw" ->
    Raw_IO.openfile source false >>= fun t ->
    Raw_input.raw t
  | _, _ -> assert false

let write_stream common s destination source_protocol destination_protocol prezeroed progress = 
  endpoint_of_string destination >>= fun endpoint ->
  let use_ssl = match endpoint with Https _ -> true | _ -> false in
  ( match endpoint with
    | File path ->
      Lwt_unix.openfile path [ Unix.O_RDWR ] 0o0 >>= fun fd ->
      Channels.of_seekable_fd fd >>= fun c ->
      return (c, [ NoProtocol; Human ])
    | Null ->
      Lwt_unix.openfile "/dev/null" [ Unix.O_RDWR ] 0o0 >>= fun fd ->
      Channels.of_raw_fd fd >>= fun c ->
      return (c, [ NoProtocol; Human ])
    | Stdout ->
      Channels.of_raw_fd Lwt_unix.stdout >>= fun c ->
      return (c, [ NoProtocol; Human ])
    | File_descr fd ->
      Channels.of_raw_fd fd >>= fun c ->
      return (c, [ Nbd; NoProtocol; Chunked; Human ])
    | Sockaddr sockaddr ->
      let sock = socket sockaddr in
      Lwt_unix.connect sock sockaddr >>= fun () ->
      Channels.of_raw_fd sock >>= fun c ->
      return (c, [ Nbd; NoProtocol; Chunked; Human ])
    | Https uri'
    | Http uri' ->
      (* TODO: https is not currently implemented *)
      let port = match Uri.port uri' with None -> (if use_ssl then 443 else 80) | Some port -> port in
      let host = match Uri.host uri' with None -> failwith "Please supply a host in the URI" | Some host -> host in
      Lwt_unix.gethostbyname host >>= fun host_entry ->
      let sockaddr = Lwt_unix.ADDR_INET(host_entry.Lwt_unix.h_addr_list.(0), port) in
      let sock = socket sockaddr in
      Lwt_unix.connect sock sockaddr >>= fun () ->

      let open Cohttp in
      ( if use_ssl then Channels.of_ssl_fd sock else Channels.of_raw_fd sock ) >>= fun c ->
  
      let module Request = Request.Make(Cohttp_unbuffered_io) in
      let module Response = Response.Make(Cohttp_unbuffered_io) in
      let headers = Header.init () in
      let k, v = Cookie.Cookie_hdr.serialize [ "chunked", "true" ] in
      let headers = Header.add headers k v in
      let headers = match Uri.userinfo uri' with
        | None -> headers
        | Some x ->
          begin match Re_str.bounded_split_delim (Re_str.regexp_string ":") x 2 with
          | [ user; pass ] ->
            let b = Cohttp.Auth.(to_string (Basic (user, pass))) in
            Header.add headers "authorization" b
          | _ ->
            Printf.fprintf stderr "I don't know how to handle authentication for this URI.\n Try scheme://user:password@host/path\n";
            exit 1
          end in
      let request = Cohttp.Request.make ~meth:`PUT ~version:`HTTP_1_1 ~headers uri' in
      Request.write (fun t _ -> return ()) request c >>= fun () ->
      Response.read (Cohttp_unbuffered_io.make_input c) >>= fun r ->
      begin match r with
      | None -> fail (Failure "Unable to parse HTTP response from server")
      | Some x ->
        let code = Code.code_of_status (Cohttp.Response.status x) in
        if Code.is_success code then begin
          let advertises_nbd =
            let headers = Header.to_list (Cohttp.Response.headers x) in
            let headers = List.map (fun (x, y) -> String.lowercase x, String.lowercase y) headers in
            let te = "transfer-encoding" in
            List.mem_assoc te headers && (List.assoc te headers = "nbd") in
          if advertises_nbd
          then return(c, [ Nbd ])
          else return(c, [ Chunked; NoProtocol ])
        end else fail (Failure (Code.reason_phrase_of_code code))
      end
    ) >>= fun (c, possible_protocols) ->
    let destination_protocol = match destination_protocol with
      | Some x -> x
      | None ->
        let t = List.hd possible_protocols in
        Printf.fprintf stderr "Using protocol: %s\n%!" (string_of_protocol t);
        t in
    if not(List.mem destination_protocol possible_protocols)
    then fail(Failure(Printf.sprintf "this destination only supports protocols: [ %s ]" (String.concat "; " (List.map string_of_protocol possible_protocols))))
    else
      let start = Unix.gettimeofday () in
      (match destination_protocol with
          | Nbd -> stream_nbd
          | Human -> stream_human
          | Chunked -> stream_chunked
          | NoProtocol -> stream_raw) common c s prezeroed ~progress () >>= fun p ->
      c.Channels.close () >>= fun () ->
      match p with
      | Some p ->
        let time = Unix.gettimeofday () -. start in
        let physical_rate = Int64.(to_float p /. time) in
        if common.Common.verb then begin
          let add_unit x =
            let kib = 1024. in
            let mib = kib *. 1024. in
            let gib = mib *. 1024. in
            let tib = gib *. 1024. in
            if x /. tib > 1. then Printf.sprintf "%.1f TiB" (x /. tib)
            else if x /. gib > 1. then Printf.sprintf "%.1f GiB" (x /. gib)
            else if x /. mib > 1. then Printf.sprintf "%.1f MiB" (x /. mib)
            else if x /. kib > 1. then Printf.sprintf "%.1f KiB" (x /. kib)
            else Printf.sprintf "%.1f B" x in

          Printf.printf "Time taken: %s\n" (hms (int_of_float time));
          Printf.printf "Physical data rate: %s/sec\n" (add_unit physical_rate);
          let speedup = Int64.(to_float s.size.total /. (to_float p)) in
          Printf.printf "Speedup: %.1f\n" speedup;
          Printf.printf "Virtual data rate: %s/sec\n" (add_unit (physical_rate *. speedup));
        end;
        return ()
      | None -> return ()


let stream_t common source relative_to source_format destination_format destination source_protocol destination_protocol prezeroed ?(progress = no_progress_bar) () =
  make_stream common source relative_to source_format destination_format >>= fun s ->
  write_stream common s destination source_protocol destination_protocol prezeroed progress

let stream common (source: string) (relative_to: string option) (source_format: string) (destination_format: string) (destination: string) (source_protocol: string option) (destination_protocol: string option) prezeroed progress =
  try
    File.use_unbuffered := common.Common.unbuffered;

    let source_protocol = require "source-protocol" source_protocol in

    let supported_formats = [ "raw"; "vhd"; "hybrid" ] in
    let destination_protocol = match destination_protocol with
      | None -> None
      | Some x -> Some (protocol_of_string x) in
    if not (List.mem source_format supported_formats)
    then failwith (Printf.sprintf "%s is not a supported format" source_format);
    if not (List.mem destination_format supported_formats)
    then failwith (Printf.sprintf "%s is not a supported format" destination_format);

    let progress_bar = if progress then console_progress_bar else no_progress_bar in

    let thread = stream_t common source relative_to source_format destination_format destination source_protocol destination_protocol prezeroed ~progress:progress_bar () in
    Lwt_main.run thread;
    `Ok ()
  with Failure x ->
    `Error(true, x)

let serve_nbd_to_raw common size c dest =
  let flags = [] in
  let open Nbd in
  let buf = Cstruct.create Negotiate.sizeof in
  Negotiate.marshal buf { Negotiate.size; flags };
  c.Channels.really_write buf >>= fun () ->

  let twomib = 2 * 1024 * 1024 in
  let block = Memory.alloc twomib in
  let inblocks fn request =
    let rec loop offset remaining =
      let n = min twomib remaining in
      let subblock = Cstruct.sub block 0 n in
      fn offset subblock >>= fun () ->
      let remaining = remaining - n in
      let offset = Int64.(add offset (of_int n)) in
      if remaining > 0 then loop offset remaining else return () in
    loop request.Request.from (Int32.to_int request.Request.len) in

  let req = Cstruct.create Request.sizeof in
  let rep = Cstruct.create Reply.sizeof in
  let rec serve_requests () =
    c.Channels.really_read req >>= fun () ->
    match Request.unmarshal req with
    | Result.Error e -> fail e
    | Result.Ok request ->
      if common.Common.debug
      then Printf.fprintf stderr "%s\n%!" (Request.to_string request);
      begin match request.Request.ty with
      | Command.Write ->
        inblocks (fun offset subblock ->
          c.Channels.really_read subblock >>= fun () ->
          Fd.really_write dest offset subblock
        ) request >>= fun () ->
        Reply.marshal rep { Reply.error = 0l; handle = request.Request.handle };
        c.Channels.really_write rep
      | Command.Read ->
        Reply.marshal rep { Reply.error = 0l; handle = request.Request.handle };
        c.Channels.really_write rep >>= fun () ->
        inblocks (fun offset subblock ->
          Fd.really_read_into dest offset subblock >>= fun subblock ->
          c.Channels.really_write subblock
        ) request
      | _ ->
        Reply.marshal rep { Reply.error = 1l; handle = request.Request.handle };
        c.Channels.really_write rep
      end >>= fun () ->
      serve_requests () in
  serve_requests ()

let serve_chunked_to_raw common c dest =
  let header = Cstruct.create Chunked.sizeof in
  let twomib = 2 * 1024 * 1024 in
  let buffer = Memory.alloc twomib in
  let rec loop () =
    c.Channels.really_read header >>= fun () ->
    if Chunked.is_last_chunk header then begin
      Printf.fprintf stderr "Received last chunk.\n%!";
      return ()
    end else begin
      let rec block offset remaining =
        let this = Int32.(to_int (min (of_int twomib) remaining)) in
        let buf = if this < twomib then Cstruct.sub buffer 0 this else buffer in
        c.Channels.really_read buf >>= fun () ->
        Fd.really_write dest offset buf >>= fun () ->
        let offset = Int64.(add offset (of_int this)) in
        let remaining = Int32.(sub remaining (of_int this)) in
        if remaining > 0l
        then block offset remaining
        else return () in
      block (Chunked.get_offset header) (Chunked.get_len header) >>= fun () ->
      loop ()
    end in
  loop ()

let serve_raw_to_raw common size c dest =
  let twomib = 2 * 1024 * 1024 in
  let buffer = Memory.alloc twomib in
  let rec loop offset remaining =
    let this = Int64.(to_int (min remaining (of_int (Cstruct.len buffer)))) in
    let block = Cstruct.sub buffer 0 this in
    c.Channels.really_read block >>= fun () ->
    Fd.really_write dest offset block >>= fun () ->
    let offset = Int64.(add offset (of_int this)) in
    let remaining = Int64.(sub remaining (of_int this)) in
    if remaining > 0L
    then loop offset remaining
    else return () in
  loop 0L size

let serve common source source_fd source_protocol destination destination_format size =
  try
    File.use_unbuffered := common.Common.unbuffered;

    let source_protocol = protocol_of_string (require "source-protocol" source_protocol) in

    let supported_formats = [ "raw" ] in
    if not (List.mem destination_format supported_formats)
    then failwith (Printf.sprintf "%s is not a supported format" destination_format);
    let supported_protocols = [ NoProtocol; Chunked; Nbd ] in
    if not (List.mem source_protocol supported_protocols)
    then failwith (Printf.sprintf "%s is not a supported source protocol" (string_of_protocol source_protocol));

    let thread =
      endpoint_of_string destination >>= fun destination_endpoint ->
      ( match source_fd with
        | None -> endpoint_of_string source
        | Some fd -> return (File_descr (Lwt_unix.of_unix_file_descr (file_descr_of_int fd))) ) >>= fun source_endpoint ->
      ( match source_endpoint with
        | File_descr fd ->
          Channels.of_raw_fd fd >>= fun c ->
          return c
        | Sockaddr s ->
          let sock = socket s in
          Lwt_unix.bind sock s;
          Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
          Lwt_unix.listen sock 1;
          Lwt_unix.accept sock >>= fun (fd, _) ->
          Channels.of_raw_fd fd >>= fun c ->
          return c
        | _ -> failwith (Printf.sprintf "Not implemented: serving from source %s" source) ) >>= fun source_sock ->
      ( match destination_endpoint with
        | File path ->
          let size = File.get_file_size path in
          Fd.openfile path false >>= fun fd ->
          return (fd, size)
        | _ -> failwith (Printf.sprintf "Not implemented: writing to destination %s" destination) ) >>= fun (destination_fd, default_size) ->
      let fn = match source_protocol, size with
        | NoProtocol, Some size -> serve_raw_to_raw common size
        | NoProtocol, None      -> serve_raw_to_raw common default_size
        | Nbd, Some size        -> serve_nbd_to_raw     common size
        | Nbd, None             -> serve_nbd_to_raw     common default_size
        | Chunked, _            -> serve_chunked_to_raw common
        | _, _ -> assert false in
      fn source_sock destination_fd >>= fun () ->
      (try Fd.fsync destination_fd; return () with _ -> fail (Failure "fsync failed")) in
    Lwt_main.run thread;
    `Ok ()
  with Failure x ->
  `Error(true, x)
