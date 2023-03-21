(** Types used for [etcd] configuration fields.

    The serialization here matches what etcd expects.
 *)

(** a [result] type with errors having [`Msg of string] type *)
type ('a, 'b) result = ('a, ([> `Msg of string] as 'b)) Result.t

val ( let+ ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
(** [let+ ok = opt in ...] executes the body when [opt] is [Some ok] with [ok] in scope.

    This is the same as [Result.bind opt @@ fun ok -> ...]
 *)

(** an integer that is always > 0 *)
type positive = private int64

val typ_of_positive : positive Rpc.Types.typ
(** [typ_of_positive] serializes the same as an [int64] *)

val positive : int64 -> (positive, _) result
(** [positive i] is [i] when [i > 0] or an error otherwise.  *)

(** an integer that is always >= 0, where 0 means unlimited *)
type limit = private int64

val typ_of_limit : limit Rpc.Types.typ
(** [typ_of_limit] serializes the same as an [int64] *)

val positive_or_unlimited : int64 option -> (limit, _) result
(** [positive_or_unlimited limit] declares an etcd style limit.

      @param limit when [None] means unlimited (encoded as 0), otherwise [Some p], where [p > 0].

      [0] is not accepted, because it means unlimited, and explicitly raises an
  exception to avoid accidentally interpreting a "no values permitted" as
  "unlimited values are permitted".
   *)

(** milliseconds, always > 0 *)
type milliseconds = private int64

val typ_of_milliseconds : milliseconds Rpc.Types.typ
(** [typ_of_milliseconds] serializes the same as an [int64] *)

val milliseconds : Mtime.Span.t -> (milliseconds, _) result
(** [milliseconds seconds] is [seconds] converted to [milliseconds] and
      rounded to nearest integer.

      @return [Error (`Msg reason`)] if the value would be [<= 0]
  *)

(** local or peer Uri *)
type uri = Uri.t

val typ_of_uri : uri Rpc.Types.typ
(** [typ_of_uri] serializes by converting to/from strings *)

val uri : ip:Ipaddr.t -> port:int -> (Uri.t, _) result
(** [uri ~ip ~port] builds an URL for the given [ip:port].
  Whether to use https or not will be decided later when a TLS configuration is
  present.
 *)

(** [filesystem path] *)
type path = Fpath.t

val typ_of_path : path Rpc.Types.typ
(** [typ_of_path] serializes as string *)

(** whether this is the first node or joining existing cluster *)
type initial_cluster_state = New | Existing

val typ_of_initial_cluster_state : initial_cluster_state Rpc.Types.typ
(** [typ_of_initial_cluster_state] serializes as [new] and [existing] *)

(** etcd log destination *)
type log_output = Stdout | Stderr | Default

val typ_of_log_output : log_output Rpc.Types.typ
(** [typ_of_log_output] serializes as [stdout], [stderr], and [default] *)

(** logging level *)
type level = CRITICAL | ERROR | WARNING | INFO | DEBUG

val typ_of_level : level Rpc.Types.typ
(** [typ_of_level] serializes the same as the variant names *)

(** string map, serialized as [k1=v1,...,kN=vN] *)
type uri_map = Uri.t Map.Make(String).t

val typ_of_uri_map : uri_map Rpc.Types.typ
(** [typ_of_uri_map] serializes as [k1=v1, ..., kN=vN] *)
