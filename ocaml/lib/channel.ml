open S

let ( |> ) a b = b a

type t = Unix.file_descr

type protocol =
  | TCP_proxy of string * int             (** IP, port *)
  | V4V_proxy of int * int                (** domid, port *)
  | Unix_sendmsg of int * string * string (** domid, path, token *)
[@@deriving rpc]

type protocols = protocol list [@@deriving rpc]

let _proxy = "/usr/local/bin/proxy"

let int_of_file_descr (fd: Unix.file_descr) : int = Obj.magic fd

let export fd =
  let ic = Unix.open_process_in (Printf.sprintf "%s -proxy %d" _proxy (int_of_file_descr fd)) in
  let json = input_line ic in
  close_in ic;
  protocols_of_rpc (Jsonrpc.of_string json)

let rpc_of_t fd =
  (* Advertise the fd's availability over a list of protocols *)
  rpc_of_protocols (export fd)

let t_of_rpc x =
  let protocols = protocols_of_rpc x in
  (* Choose the best transport mechanism from the list of options *)
  try
    match List.find (function TCP_proxy(_, _)-> true | _ -> false) protocols with
    | TCP_proxy(ip, port) ->
      let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      begin
        try
          Unix.connect s (Unix.ADDR_INET(Unix.inet_addr_of_string ip, port));
          s
        with e ->
          Unix.close s;
          raise (Internal_error (Printexc.to_string e))
      end
    | _ -> assert false
  with Not_found ->
    raise Channel_setup_failed

let example = Unix.stdout
