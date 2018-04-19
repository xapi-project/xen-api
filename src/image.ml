let get_device_numbers path =
  let rdev = (Unix.LargeFile.stat path).Unix.LargeFile.st_rdev in
  let major = rdev / 256 and minor = rdev mod 256 in
  (major, minor)

let is_nbd_device path =
  let nbd_device_num = 43 in
  let (major, _) = get_device_numbers path in
  major = nbd_device_num

module Opt = struct
  let default d = function
    | None -> d
    | Some x -> x
end

type t = [
  | `Vhd of string
  | `Raw of string
  | `Nbd of string * string
]

let to_string = function
  | `Vhd x -> "vhd:" ^ x
  | `Raw x -> "raw:" ^ x
  | `Nbd (x,y) -> Printf.sprintf "nbd:(%s,%s)" x y

type nbd_connect_info =
  { path : string
  ; exportname : string
  } [@@deriving rpc]

let get_nbd_device path =
  let nbd_device_prefix = "/dev/nbd" in
  if Astring.String.is_prefix ~affix:nbd_device_prefix path && is_nbd_device path then begin
    let nbd_number =
      String.sub path (String.length nbd_device_prefix) (String.length path - String.length nbd_device_prefix)
    in
    let { path; exportname } =
      let persistent_nbd_info_dir = "/var/run/nonpersistent/nbd" in
      let filename = persistent_nbd_info_dir ^ "/" ^ nbd_number in
      Xapi_stdext_unix.Unixext.string_of_file filename
      |> Jsonrpc.of_string
      |> nbd_connect_info_of_rpc
    in
    Some (`Nbd (path, exportname))
  end else None

let of_device path =
  try
    match Tapctl.of_device (Tapctl.create ()) path with
    | _, _, (Some ("vhd", vhd)) -> Some (`Vhd vhd)
    | _, _, (Some ("aio", vhd)) -> Some (`Raw vhd)
    | _, _, _ -> raise Not_found
  with
  | Tapctl.Not_blktap ->
    get_nbd_device path
  | Tapctl.Not_a_device ->
    None
  | _ ->
    None
