open Xen_api
open Lwt

let is_ssl ?(allow_file=true) uri =
  match Uri.scheme uri with
  | Some "https" ->
    true
  | Some "http" ->
    false
  | Some "file" when allow_file ->
    false
  | x ->
    failwith (Printf.sprintf "Unsupported URI scheme: %s" (match x with None -> "None" | Some x -> x))

let sockaddr_of_uri uri =
  let use_ssl = is_ssl ~allow_file:false uri in
  let port =
    match Uri.port uri with
    | None when use_ssl ->
      443
    | None ->
      80
    | Some port ->
      port
  in
  let host =
    match Uri.host uri with
    | None ->
      failwith "Please supply a host in the URI"
    | Some host ->
      Scanf.ksscanf host (fun _ _ -> host) "[%s@]" Fun.id
  in
  Lwt.catch (fun () ->
    Lwt_unix.getaddrinfo host (string_of_int port) [] >|= function
      | [] ->
        raise Not_found
      | addrinfo :: _ ->
        ( addrinfo.Unix.ai_addr, use_ssl) )
    (fun _ ->  fail (Failed_to_resolve_hostname host))
