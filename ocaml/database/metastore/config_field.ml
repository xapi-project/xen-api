type ('a, 'b) result = ('a, ([> `Msg of string] as 'b)) Result.t

let ( let+ ) = Result.bind

type positive = int64 [@@deriving rpcty]

let positive i64 =
  if Int64.compare i64 Int64.zero > 0 then
    Ok i64
  else
    Fmt.error_msg "Value must be > 0: %a" Fmt.int64 i64

type limit = int64 [@@deriving rpcty]

let positive_or_unlimited = function None -> Ok 0L | Some i64 -> positive i64

type milliseconds = int64 [@@deriving rpcty]

let milliseconds t =
  t |> Mtime.Span.to_ms |> Float.round |> Int64.of_float |> Result.ok

type uri = Uri.t

let typ_of_uri =
  Serialization.using ~aname:"uri" Uri.to_string Uri.of_string
    Rpc.Types.string.Rpc.Types.ty

let uri ~ip ~port =
  if port <= 0 then
    Fmt.error_msg "Port must be > 0: %d" port
  else
    Uri.make ~host:(Ipaddr.to_string ip) ~port () |> Result.ok

type path = Fpath.t

let typ_of_path =
  Serialization.using ~aname:"path" Fpath.to_string Fpath.v
    Rpc.Types.string.Rpc.Types.ty

type initial_cluster_state = New | Existing [@@deriving rpcty]

type log_output = Stdout | Stderr | Default [@@deriving rpcty]

(* list of log levels accepted by etcd *)
type level = CRITICAL | ERROR | WARNING | INFO | DEBUG [@@deriving rpcty]

module StringMap = Map.Make (String)

(** string map, serialized as [k1=v1,...,kN=vN] *)
type uri_map = Uri.t StringMap.t

let typ_of_map elt_to_string elt_of_string =
  let to_string t =
    t
    |> StringMap.to_seq
    |> Seq.map (fun (k, v) ->
           (* it is assumed that keys do not contain '=',
              which they cannot if they come from OCaml record field names *)
           assert (String.index_opt k '=' = None) ;
           String.concat "=" [k; elt_to_string v]
       )
    |> List.of_seq
    |> String.concat ","
  in
  let of_string s =
    s
    |> String.split_on_char ','
    |> List.to_seq
    |> Seq.filter_map (Astring.String.cut ~sep:"=")
    |> Seq.map (fun (k, v) -> (k, elt_of_string v))
    |> StringMap.of_seq
  in
  Serialization.using ~aname:"uri" to_string of_string
    Rpc.Types.string.Rpc.Types.ty

let typ_of_uri_map = typ_of_map Uri.to_string Uri.of_string
