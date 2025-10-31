(*
 * Copyright (C) 2011 Citrix Systems Inc.
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
(** Advertise services
*)

module D = Debug.Make (struct let name = "xapi_services" end)

open D

let finally = Xapi_stdext_pervasives.Pervasiveext.finally

module Unixext = Xapi_stdext_unix.Unixext
open Constants

type driver_list = Storage_interface.query_result list [@@deriving rpcty]

let list_sm_drivers ~__context =
  let all =
    List.map
      (fun x -> Smint.query_result_of_sr_driver_info (Sm.info_of_driver x))
      (Sm.supported_drivers ())
  in
  Storage_interface.rpc_of driver_list all

let respond req rpc s =
  let txt = Jsonrpc.to_string rpc in
  Http_svr.headers s (Http.http_200_ok ~version:"1.0" ~keep_alive:false ()) ;
  req.Http.Request.close <- true ;
  Unixext.really_write s txt 0 (String.length txt)

let list_drivers req s =
  respond req
    (System_domains.rpc_of_services (System_domains.list_services ()))
    s

let fix_cookie = function
  | [] ->
      []
  | cookie ->
      let str_cookie =
        String.concat "; "
          (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) cookie)
      in
      let bounded_split_delim re s n =
        let rec extract_comps_inner start acc m =
          let get_fin () =
            List.rev (String.sub s start (String.length s - start) :: acc)
          in
          if m = 1 then
            get_fin ()
          else
            try
              let first_end, all_end =
                Re.Group.offset (Re.exec ~pos:start re s) 0
              in
              extract_comps_inner all_end
                (String.sub s start (first_end - start) :: acc)
                (m - 1)
            with Not_found -> get_fin ()
        in
        extract_comps_inner 0 [] n
      in
      let comps =
        bounded_split_delim (Re.compile (Re.Emacs.re "[;,][ \t]*")) str_cookie 0
      in
      (* We don't handle $Path, $Domain, $Port, $Version (or $anything $else) *)
      let cookies = List.filter (fun s -> s.[0] <> '$') comps in
      let split_pair nvp =
        match String.split_on_char '=' nvp with
        | [] ->
            ("", "")
        | [n] ->
            (n, "")
        | n :: v :: _ ->
            (n, v)
      in
      List.map split_pair cookies

(* Transmits [req] and [s] to the service listening on [path] *)
let hand_over_connection req s path =
  try
    debug "hand_over_connection %s %s to %s"
      (Http.string_of_method_t req.Http.Request.m)
      req.Http.Request.path path ;
    let control_fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    finally
      (fun () ->
        let req = Http.Request.{req with cookie= fix_cookie req.cookie} in
        Unix.connect control_fd (Unix.ADDR_UNIX path) ;
        let msg = req |> Http.Request.rpc_of_t |> Jsonrpc.to_string in
        let len = String.length msg in
        let written = Unixext.send_fd_substring control_fd msg 0 len [] s in
        if written <> len then (
          error "Failed to transfer fd to %s" path ;
          Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
          req.Http.Request.close <- true ;
          None
        ) else
          let response = Http_client.response_of_fd control_fd in
          match response with
          | Some res ->
              res.Http.Response.task
          | None ->
              None
      )
      (fun () -> Unix.close control_fd)
  with e ->
    error "Failed to transfer fd to %s: %s" path (Printexc.to_string e) ;
    Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
    req.Http.Request.close <- true ;
    None

let http_proxy_to req from addr =
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  finally
    (fun () ->
      let () =
        try Unix.connect s addr
        with e ->
          error "Failed to proxy HTTP request to: %s"
            ( match addr with
            | Unix.ADDR_UNIX path ->
                "UNIX:" ^ path
            | Unix.ADDR_INET (ip, port) ->
                "IP:" ^ Unix.string_of_inet_addr ip ^ ":" ^ string_of_int port
            ) ;
          Http_svr.headers from (Http.http_404_missing ~version:"1.0" ()) ;
          raise e
      in
      Http_proxy.one req from s
    )
    (fun () -> Unix.close s)

let http_proxy_to_plugin req from name =
  let path = Filename.concat "/var/lib/xcp" (Printf.sprintf "plugin/%s" name) in
  if not (Sys.file_exists path) then (
    req.Http.Request.close <- true ;
    error "There is no Unix domain socket %s for plugin %s" path name ;
    Http_svr.headers from (Http.http_404_missing ~version:"1.0" ())
  ) else
    http_proxy_to req from (Unix.ADDR_UNIX path)

let post_handler (req : Http.Request.t) s _ =
  Xapi_http.with_context ~dummy:true "Querying services" req s (fun __context ->
      match String.split_on_char '/' req.Http.Request.path with
      | "" :: services :: "xenops" :: _ when services = _services ->
          (* over the network we still use XMLRPC *)
          let request = Http_svr.read_body req s in
          let response =
            if !Xcp_client.use_switch then
              let req = Xmlrpc.call_of_string request in
              let res =
                Xcp_client.switch_rpc
                  !Xapi_globs.default_xenopsd
                  Jsonrpc.string_of_call Jsonrpc.response_of_string req
              in
              Xmlrpc.string_of_response res
            else
              Xcp_client.http_rpc
                (fun x -> x)
                (fun x -> x)
                ~srcstr:"remote" ~dststr:"xenops" Xenops_interface.default_uri
                request
          in
          Http_svr.response_str req ~hdrs:[] s response
      | "" :: services :: "plugin" :: name :: _ when services = _services ->
          http_proxy_to_plugin req s name
      | [""; services; "SM"] when services = _services ->
          Storage_mux.Local_domain_socket.xmlrpc_handler
            Storage_mux.Server.process req s ()
      | _ ->
          Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
          req.Http.Request.close <- true
  )

let put_handler (req : Http.Request.t) s _ =
  Xapi_http.with_context ~dummy:true "Querying services" req s (fun __context ->
      match String.split_on_char '/' req.Http.Request.path with
      | "" :: services :: "xenops" :: _ when services = _services ->
          ignore
            (hand_over_connection req s
               (Filename.concat "/var/lib/xcp" "xenopsd.forwarded")
            )
      | "" :: services :: "plugin" :: name :: _ when services = _services ->
          http_proxy_to_plugin req s name
      | [""; services; "SM"; "data"; sr; vdi] when services = _services ->
          let vdi, _ =
            Storage_utils.find_vdi ~__context
              (Storage_interface.Sr.of_string sr)
              (Storage_interface.Vdi.of_string vdi)
          in
          ignore (Import_raw_vdi.import (Some vdi) req s ())
      | [""; services; "SM"; "nbd"; sr; vdi; dp] when services = _services ->
          Storage_migrate.nbd_handler req s sr vdi dp
      | [""; services; "SM"; "nbd"; vm; sr; vdi; dp] when services = _services
        ->
          Storage_migrate.nbd_handler req s ~vm sr vdi dp
      | [""; services; "SM"; "nbdproxy"; vm; sr; vdi; dp]
      | [""; services; "SM"; "nbdproxy"; "import"; vm; sr; vdi; dp]
        when services = _services ->
          Storage_migrate.import_nbd_proxy req s vm sr vdi dp
      | _ ->
          Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
          req.Http.Request.close <- true
  )

let get_handler (req : Http.Request.t) s _ =
  Xapi_http.with_context ~dummy:true "Querying services" req s (fun __context ->
      debug "uri = %s" req.Http.Request.path ;
      match String.split_on_char '/' req.Http.Request.path with
      | "" :: services :: "xenops" :: _ when services = _services ->
          ignore
            (hand_over_connection req s
               (Filename.concat "/var/lib/xcp" "xenopsd.forwarded")
            )
      | "" :: services :: "plugin" :: name :: _ when services = _services ->
          http_proxy_to_plugin req s name
      | [""; services; "driver"] when services = _services ->
          list_drivers req s
      | [""; services; "SM"; driver] when services = _services -> (
        try
          respond req
            (Storage_interface.(rpc_of query_result)
               (Smint.query_result_of_sr_driver_info (Sm.info_of_driver driver))
            )
            s
        with _ ->
          Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
          req.Http.Request.close <- true
      )
      | [""; services; "SM"] when services = _services ->
          let rpc = list_sm_drivers ~__context in
          respond req rpc s
      | [""; services] when services = _services ->
          let q =
            {
              Storage_interface.driver= "mux"
            ; name= "storage multiplexor"
            ; description= "forwards calls to other plugins"
            ; vendor= "XCP"
            ; copyright= "see the source code"
            ; version= "2.0"
            ; required_api_version= "2.0"
            ; features= List.map (fun x -> path [_services; x]) [_SM]
            ; configuration= []
            ; required_cluster_stack= []
            ; smapi_version= SMAPIv2
            }
          in
          respond req (Storage_interface.(rpc_of query_result) q) s
      | _ ->
          Http_svr.headers s (Http.http_404_missing ~version:"1.0" ()) ;
          req.Http.Request.close <- true
  )
