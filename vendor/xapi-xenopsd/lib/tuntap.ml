(*
 * Copyright (c) Citrix
 *)

external _tap_open : string -> Unix.file_descr = "stub_tap_open"

let finally = Xenops_utils.finally

let tap_open ifname =
  try
    _tap_open ifname
  with
    Failure msg -> raise (Xenops_interface.Internal_error msg)

let with_tap ifname ~fn =
  let fd = tap_open ifname in
  finally
    (fun () -> fn fd)
    (fun () -> Unix.close fd)

