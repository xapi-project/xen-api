type dev = {
  device : string;
  offset : int64;
}

type stripety = {
  chunk_size : int64;
  dests : dev array;
}

type mapty = 
    | Linear of dev (* Device, offset *)
    | Striped of stripety

type mapping = {
  start : int64;
  len : int64;
  map : mapty;
}

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
  targets : (int64 * int64 * string * string) list
}

external _create : string -> (int64 * int64 * string * string) array -> unit = "camldm_create"
external _table : string -> status = "camldm_table"
external _mknods : string -> unit = "camldm_mknods"
external _remove : string -> unit = "camldm_remove"
external _mknod : string -> int -> int -> int -> unit = "camldm_mknod"

(* Helper to convert from our type to the string*string 
 * type expected by libdevmapper *)
let convert_mapty m =
  let array_concat sep a = String.concat sep (Array.to_list a) in
  match m with
    | Linear dev -> 
	"linear",Printf.sprintf "%s %Ld" dev.device dev.offset
    | Striped st ->
	"striped",
	Printf.sprintf "%d %Ld %s" (Array.length st.dests) st.chunk_size 
	  (array_concat " " 
	      (Array.map (fun dev -> 
		Printf.sprintf "%s %Ld" dev.device dev.offset) st.dests))

let create dev map =
  let newmap = Array.map (fun m ->
    let (ty,params) = convert_mapty m.map in
    (m.start, m.len, ty, params)) map in
  _create dev newmap

let remove = _remove
let table = _table
let mknods = _mknods
let mknod = _mknod
 
