open Rresult

let list l k =
  if not(List.mem_assoc k l)
  then Error (Printf.sprintf "missing %s key" k)
  else Ok (List.assoc k l)

let int x = try Ok (int_of_string x) with _ -> Error ("not an int: " ^ x)
let int32 x = try Ok (Int32.of_string x) with _ -> Error ("not an int32: " ^ x)
let _int64 x = try Ok (Int64.of_string x) with _ -> Error ("not an int64: " ^ x)

module Protocol = struct
  type t = Vt100
  let to_string _ = "vt100"
end

module Output = struct
  type t = Pty
  let to_string _ = "pty"
end

module RingInfo = struct
  type t = {
      ref: int32;
      event_channel: int;
    }

  let to_string t =
    Printf.sprintf "{ ref = %ld; event_channel = %d }"
                   t.ref t.event_channel

  let _ring_ref = "ring-ref"
  let _port = "port"

  let keys = [
      _ring_ref;
      _port;
    ]

  let to_assoc_list t = [
      _ring_ref, Int32.to_string t.ref;
      _port, string_of_int t.event_channel;
    ]

  let of_assoc_list l =
    list l _ring_ref >>= fun x -> int32 x
    >>= fun ref -> list l _port >>= fun x -> int x
    >>= fun event_channel -> Ok { ref; event_channel }
end

module State = struct
  type t = Initialising | InitWait | Initialised | Connected | Closing | Closed
  let table = [
      1, Initialising;
      2, InitWait;
      3, Initialised;
      4, Connected;
      5, Closing;
      6, Closed
    ]
  let table' = List.map (fun (x, y) -> y, x) table
  let to_string t = string_of_int (List.assoc t table' )
  let of_string t = try Some (List.assoc (int_of_string t) table) with _ -> None

  let of_int x =
    if List.mem_assoc x table
    then Ok (List.assoc x table)
    else Error (Printf.sprintf "unknown device state: %d" x)

  let _state = "state"
  let keys = [ _state ]
  let of_assoc_list l =
    list l _state >>= fun x ->
    int x >>= fun x ->
    of_int x
  let to_assoc_list t = [
      _state, string_of_int (List.assoc t table')
    ]
end

module Connection = struct
  type t = {
      virtual_device: int;
      backend_path: string;
      backend_domid: int;
      frontend_path: string;
      frontend_domid: int;
      protocol: Protocol.t;
      name: string option; (* protocol extension to name consoles *)
    }

  let make ?(protocol = Protocol.Vt100) ?(backend_domid = 0) ?name
           ~frontend_domid ~virtual_device () =
    let backend_path =
      Printf.sprintf "/local/domain/%d/backend/console/%d/%d"
                     backend_domid frontend_domid virtual_device
    in
    let frontend_path =
      Printf.sprintf "/local/domain/%d/device/console/%d"
                     frontend_domid virtual_device
    in
    { virtual_device; backend_domid; backend_path;
      frontend_domid; frontend_path; protocol; name }

  let to_assoc_list t =
    let backend =
      List.map (fun (owner, key, value) ->
          owner, Printf.sprintf "%s/%s" t.backend_path key, value
        ) [`Backend, "frontend", t.frontend_path;
           `Backend, "frontend-id", string_of_int t.frontend_domid;
           `Backend, "online", "1";
           `Backend, "state", State.to_string State.Initialising;
           `Backend, "protocol", Protocol.to_string t.protocol;
          ] in
    let frontend =
      List.map (fun (owner, key, value) ->
          owner, Printf.sprintf "%s/%s" t.frontend_path key, value
        ) ([`Frontend, "backend", t.backend_path;
            `Frontend, "backend-id", string_of_int t.backend_domid;
            `Frontend, "state", State.to_string State.Initialising;
            `Frontend, "protocol", Protocol.to_string t.protocol;
            `Backend, "limit", "1048576";
            (* `Backend, "type", "xenconsoled"; *)
            `Backend, "output", "pty";
            `Backend, "tty", "";
           ] @ (match t.name with
                | None -> []
                | Some n -> [ `Frontend, "name", n ]))
    in
    let all = [
        `Backend, t.backend_path, "";
        `Frontend, t.frontend_path, "";
      ] @ backend @ frontend in
    let frontend_acl =
      Xs_protocol.ACL.({owner = t.frontend_domid;
                        other = NONE; acl = [ t.backend_domid, READ ]})
    in
    let backend_acl =
      Xs_protocol.ACL.({owner = t.backend_domid;
                        other = NONE; acl = [ t.frontend_domid, READ ]})
    in
    List.map (fun (owner, key, value) ->
        match owner with
        | `Frontend -> frontend_acl, (key, value)
        | `Backend -> backend_acl, (key, value)
      ) all
end
