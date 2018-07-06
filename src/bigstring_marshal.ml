open! Import
open Bigstring

(* Marshalling to/from bigstrings *)

external unsafe_marshal_blit
  : 'a -> pos : int -> len : int -> t -> Marshal.extern_flags list -> int
  = "bigstring_marshal_blit_stub"

let marshal_blit ?(flags = []) v ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"marshal" bstr ~pos ~len;
  unsafe_marshal_blit v ~pos ~len bstr flags
;;

external marshal : 'a -> Marshal.extern_flags list -> t
  = "bigstring_marshal_stub"

let marshal ?(flags = []) x = marshal x flags

external unsafe_marshal_data_size
  : pos : int -> t -> int = "bigstring_marshal_data_size_stub"

let marshal_data_size ?(pos = 0) bstr =
  if pos < 0 || pos > length bstr - Marshal.header_size
  then invalid_arg "Bigstring.marshal_data_size"
  else unsafe_marshal_data_size ~pos bstr

external unsafe_unmarshal
  : pos : int -> len : int -> t -> 'a = "bigstring_unmarshal_stub"

let unmarshal_next ?pos bstr =
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 then invalid_arg "Bigstring.unmarshal: pos < 0"
      else pos
  in
  let len = length bstr in
  let len_header = len - Marshal.header_size in
  if pos > len_header
  then invalid_arg "Bigstring.unmarshal: pos > len - header"
  else
    let data_len = unsafe_marshal_data_size ~pos bstr in
    let block_len = Marshal.header_size + data_len in
    let next_pos = pos + block_len in
    if next_pos > len
    then invalid_arg "Bigstring.unmarshal: pos + block_len > len"
    else
      let v = unsafe_unmarshal ~pos ~len:block_len bstr in
      v, next_pos
;;

let unmarshal ?pos bstr = fst (unmarshal_next ?pos bstr)

let skip ?pos bstr =
  let pos =
    match pos with
    | None -> 0
    | Some pos ->
      if pos < 0 then invalid_arg "Bigstring.skip: pos < 0"
      else pos
  in
  let len = length bstr in
  let len_header = len - Marshal.header_size in
  if pos > len_header
  then invalid_arg "Bigstring.skip: pos > len - header"
  else
    let data_len = unsafe_marshal_data_size ~pos bstr in
    let block_len = Marshal.header_size + data_len in
    let next_pos = pos + block_len in
    if next_pos > len
    then invalid_arg "Bigstring.skip: pos + block_len > len"
    else next_pos
;;
