type dev = { device : string; offset : int64; }
type stripety = { chunk_size : int64; dests : dev array; }
type mapty = Linear of dev | Striped of stripety
type mapping = { start : int64; len : int64; map : mapty; }
type status = {
  exists : bool;
  suspended : bool;
  live_table : bool;
  inactive_table : bool;
  open_count : int32;
  event_nr : int32;
  major : int32;
  minor : int32;
  read_only : bool;
  targets : (int64 * int64 * string * string) list;
}
val convert_mapty : mapty -> string * string
val create : string -> mapping array -> unit
val remove : string -> unit
val table : string -> status
val mknods : string -> unit
val mknod : string -> int -> int -> int -> unit
