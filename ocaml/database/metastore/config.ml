(** Types *)
module F = Config_field

let rpc_equal typ_of a b =
  (* Rpc is built out of standard types, so stdlib equality works on it  *)
  Rpcmarshal.marshal typ_of a = Rpcmarshal.marshal typ_of b

module StringMap = Map.Make (String)

(** stable representation for serializing/deserializing between XAPI version
   upgrades.
  Uses semantic versioning: can add more fields in a compatible way, e.g. new
  optional fields, or new fields with default values.
  If incompatible changes are made then {!module:V1} must be kept as is
  and a {!module:V2} introduced (with conversion functions from {!module:V1}).
  However this should be avoided.
*)
module V1 = struct
  module Live = struct
    type t = {
        advertise_client_urls: F.uri list
      ; initial_advertise_peer_urls: F.uri list
    }
    [@@deriving rpcty]
  end

  module Local = struct
    type listen_cert = {cert_file: F.path; key_file: F.path} [@@deriving rpcty]

    type tls_listen = AutoTLS | Certs of listen_cert [@@deriving rpcty]

    type tls_auth = {
        trusted_ca_file: F.path
      ; cert_allowed_hostname: string option
      ; cert_allowed_cn: string option
      ; crl_file: F.path option
    }
    [@@deriving rpcty]

    type listen = (tls_listen * tls_auth option) option * F.uri list
    [@@deriving rpcty]

    type config = {
        name: string
      ; enable_v2: bool
      ; log_level: F.level option
      ; log_output: F.log_output option
    }
    [@@deriving rpcty]

    type t = {config: config; peer: listen; client: listen} [@@deriving rpcty]
  end
end

(* we will have more fields here when we support pools *)

type v1 = {live: V1.Live.t; local: V1.Local.t} [@@deriving rpcty]

type t = V1 of v1 [@@deriving rpcty]

(** key-value representation matching etcd.

  Nesting will be flattened, but can only contain record or primitive types
  Field names match [etcd] names but without the [ETCD_] prefix.
*)
type main = t

module Live = struct include V1.Live end

module Local = struct
  include V1.Local

  let equal = rpc_equal typ_of
end

(** Configuration building *)

(** Building *)
module Dict = struct
  module Local = struct
    open V1.Local

    let unpack_tls = function
      | AutoTLS ->
          (None, None, true)
      | Certs t ->
          (Some t.cert_file, Some t.key_file, false)

    let unpack_auth = function
      | None ->
          (false, None, None, None, None)
      | Some t ->
          ( true
          , Some t.trusted_ca_file
          , t.cert_allowed_cn
          , t.cert_allowed_hostname
          , t.crl_file
          )

    let unpack (tls, auth) = (unpack_tls tls, unpack_auth auth)

    let apply_uri_scheme f (tls_opt, uris) =
      let scheme = match tls_opt with None -> "http" | Some _ -> "https" in
      let tls_config = tls_opt |> Option.map unpack |> Option.map f in
      let uris =
        uris |> List.map @@ fun uri -> Uri.with_scheme uri (Some scheme)
      in
      (tls_config, uris)

    type listen_peer_tls = {
        peer_cert_file: F.path option
      ; peer_key_file: F.path option
      ; peer_auto_tls: bool
      ; peer_client_cert_auth: bool
      ; peer_trusted_ca_file: F.path option
      ; peer_cert_allowed_cn: string option
      ; peer_cert_allowed_hostname: string option
      ; peer_crl_file: F.path option
    }
    [@@deriving rpcty]

    let listen_peer_tls
        ( (peer_cert_file, peer_key_file, peer_auto_tls)
        , ( peer_client_cert_auth
          , peer_trusted_ca_file
          , peer_cert_allowed_cn
          , peer_cert_allowed_hostname
          , peer_crl_file
          )
        ) =
      {
        peer_cert_file
      ; peer_key_file
      ; peer_auto_tls
      ; peer_client_cert_auth
      ; peer_trusted_ca_file
      ; peer_cert_allowed_cn
      ; peer_cert_allowed_hostname
      ; peer_crl_file
      }

    type listen_peer = {
        listen_peer_urls: F.uri list
      ; listen_tls: listen_peer_tls option
    }
    [@@deriving rpcty]

    let listen_peer listen =
      let listen_tls, listen_peer_urls =
        apply_uri_scheme listen_peer_tls listen
      in
      {listen_peer_urls; listen_tls}

    type listen_client_tls = {
        cert_file: F.path option
      ; key_file: F.path option
      ; auto_tls: bool
      ; client_cert_auth: bool
      ; trusted_ca_file: F.path option
      ; client_cert_allowed_hostname: string option
      ; client_crl_file: F.path option
    }
    [@@deriving rpcty]

    let listen_client_tls
        ( (cert_file, key_file, auto_tls)
        , ( client_cert_auth
          , trusted_ca_file
          , _
          , client_cert_allowed_hostname
          , client_crl_file
          )
        ) =
      {
        cert_file
      ; key_file
      ; auto_tls
      ; client_cert_auth
      ; trusted_ca_file
      ; client_cert_allowed_hostname
      ; client_crl_file
      }

    type listen_client = {
        listen_client_urls: F.uri list
      ; listen_tls: listen_client_tls option
    }
    [@@deriving rpcty]

    let listen_client listen =
      let listen_tls, listen_client_urls =
        apply_uri_scheme listen_client_tls listen
      in
      {listen_client_urls; listen_tls}

    type t = {config: config; peer: listen_peer; client: listen_client}
    [@@deriving rpcty]

    let make (v1 : V1.Local.t) : t =
      {
        config= v1.config
      ; peer= listen_peer v1.peer
      ; client= listen_client v1.client
      }
  end

  type t = {live: Live.t; local: Local.t} [@@deriving rpcty]

  (** Conversion to Environment *)

  let to_env_key_char = function
    | '-' ->
        '_'
    | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c ->
        Char.uppercase_ascii c
    | c ->
        Fmt.invalid_arg "Unexpected character in config field name: %C" c

  let to_env_key k =
    try "ETCD_" ^ (k |> String.map to_env_key_char)
    with Invalid_argument msg -> invalid_arg (Printf.sprintf "%s: %s" msg k)

  let rec to_environment = function
    | Rpc.Int i64 ->
        Int64.to_string i64
    | Rpc.Int32 i32 ->
        Int32.to_string i32
    | Rpc.Bool b ->
        if b then "true" else "false"
    | Rpc.Float f ->
        Printf.sprintf "%.16g" f
    | Rpc.String s ->
        s
    | Rpc.DateTime s ->
        s
    | Rpc.Enum lst ->
        lst |> List.map to_environment |> String.concat ","
    | Rpc.Dict d ->
        d
        |> List.map (fun (k, v) -> String.concat "" [k; "="; to_environment v])
        |> String.concat ","
    | Rpc.Base64 b64 ->
        Base64.decode_exn b64
    | Rpc.Null ->
        ""

  let rec rpc_to_dict = function
    | Rpc.Dict d ->
        d
        |> List.to_seq
        |> Seq.flat_map (function
             | _, (Rpc.Dict _ as nested) ->
                 rpc_to_dict nested
             | k, v ->
                 Seq.return (to_env_key k, to_environment v)
             )
    | _ ->
        assert false

  let to_dict t =
    t |> Rpcmarshal.marshal typ_of |> rpc_to_dict |> StringMap.of_seq

  let make : main -> t = function
    | V1 t ->
        {live= t.live; local= Local.make t.local}
end

let name t =
  let t = t |> Dict.make in
  t.local.Dict.Local.config.name

let make live local = V1 {live; local}

(** Configuration pretty printing *)

let dump = Serialization.dump typ_of

(** Serialization *)

let serialize t = t |> Rpcmarshal.marshal typ_of |> Jsonrpc.to_string

let deserialize s =
  s
  |> Jsonrpc.of_string
  |> Rpcmarshal.unmarshal typ_of
  |> Rresult.R.open_error_msg

let to_dict t = t |> Dict.make |> Dict.to_dict

(* cannot use Idl_test_common directly: it works on full RPC calls, not RPC types. *)

let gen_test () =
  (* generate tests at depth 0 should deterministically give us a string
     representing the structure of the type, which can be used to compute a
     digest *)
  let schema =
    Rpc_genfake.genall 0 "schema" typ_of
    |> List.to_seq
    |> Seq.map @@ Rpcmarshal.marshal typ_of
    |> List.of_seq
  in
  let schema_digest = Rpc.Enum schema |> Rpc.to_string |> Digest.string in
  (schema_digest, Rpc_genfake.genall 4 __MODULE__ typ_of)
