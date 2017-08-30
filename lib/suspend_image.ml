(*
 * Copyright (C) 2006-2014 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module M = struct
  type ('a, 'b) t = [ `Ok of 'a | `Error of 'b ]
  let (>>=) m f = match m with | `Ok x -> f x | `Error x -> `Error x
  let return x = `Ok x
  let fold f l a = List.fold_left (fun c b -> c >>= f b) (return a) l
end

let (|>) x f = f x

open M

let wrap f =
  try
    return (f ())
  with e ->
    `Error e

module Xenops_record = struct
  open Sexplib
  open Sexplib.Std
  open Sexplib.Conv
  open Xenops_interface

  type t = {
    time: string;
    word_size: int;
    (* All additional fields below should use the sexp_option extension *)
    vm_str: string sexp_option;
    xs_subtree: (string * string) list sexp_option;
  } [@@deriving sexp]

  let make ?vm_str ?xs_subtree () =
    let time = Stdext.Date.(to_string (of_float (Unix.time ()))) in
    let word_size = Sys.word_size in
    { word_size; time; vm_str; xs_subtree }

  let to_string t = wrap (fun () -> t |> sexp_of_t |> Sexp.to_string)
  let of_string s = wrap (fun () -> s |> Sexp.of_string |> t_of_sexp)
end


type format = Structured | Legacy

type header_type =
  | Xenops
  | Libxc
  | Libxl
  | Libxc_legacy
  | Qemu_trad
  | Qemu_xen
  | Demu
  | End_of_image

exception Invalid_header_type

let header_type_of_int64 = function
  | 0x000fL -> `Ok Xenops
  | 0x00f0L -> `Ok Libxc
  | 0x00f1L -> `Ok Libxl
  | 0x00f2L -> `Ok Libxc_legacy
  | 0x0f00L -> `Ok Qemu_trad
  | 0x0f01L -> `Ok Qemu_xen
  | 0x0f10L -> `Ok Demu
  | 0xffffL -> `Ok End_of_image
  | _ -> `Error Invalid_header_type

let int64_of_header_type = function
  | Xenops       -> 0x000fL
  | Libxc        -> 0x00f0L
  | Libxl        -> 0x00f1L
  | Libxc_legacy -> 0x00f2L
  | Qemu_trad    -> 0x0f00L
  | Qemu_xen     -> 0x0f01L
  | Demu         -> 0x0f10L
  | End_of_image -> 0xffffL

type header = header_type * int64 (* length *)

let string_of_header h =
  let s = Printf.sprintf in
  match h with
  | Xenops, len     -> s "Xenops header (record length=%Ld)" len
  | Libxc, _        -> s "Libxc memory image"
  | Libxl, _        -> s "Libxl save record"
  | Libxc_legacy, _ -> s "Legacy Libxc (< Xen 4.5) record (w/ QEMU record)"
  | Qemu_trad, len  -> s "Qemu (traditional) save record (record length=%Ld)" len
  | Qemu_xen, len   -> s "Qemu (Xen) save record (record length=%Ld)" len
  | Demu, len       -> s "vGPU save record (record length=%Ld)" len
  | End_of_image, _ -> s "Suspend image footer"

let read_int64 fd = wrap (fun () -> Io.read_int64 ~endianness:`little fd)
let write_int64 fd x = wrap (fun () -> Io.write_int64 ~endianness:`little fd x)

let save_signature = "XenSavedDomv2-\n"
let legacy_save_signature = "XenSavedDomain\n"
let legacy_qemu_save_signature = "QemuDeviceModelRecord\n"
let qemu_save_signature_legacy_libxc = "DeviceModelRecord0002"

let write_save_signature fd = Io.write fd save_signature
let read_save_signature fd =
  match Io.read fd (String.length save_signature) with
  | x when x = save_signature -> `Ok Structured
  | x when x = legacy_save_signature -> `Ok Legacy
  | x -> `Error (Printf.sprintf "Not a valid signature: \"%s\"" x)

let read_legacy_qemu_header fd =
  try
    match Io.read fd (String.length legacy_qemu_save_signature) with
    | x when x = legacy_qemu_save_signature ->
      `Ok (Int64.of_int (Io.read_int ~endianness:`big fd))
    | x -> `Error "Read invalid legacy qemu save signature"
  with e ->
    `Error ("Failed to read signature: " ^ (Printexc.to_string e))

let write_qemu_header_for_legacy_libxc fd size =
  wrap (fun () -> Io.write fd qemu_save_signature_legacy_libxc) >>= fun () ->
  wrap (fun () -> Io.write_int ~endianness:`little fd (Io.int_of_int64_exn size))

let read_header fd =
  read_int64 fd >>= fun x ->
  header_type_of_int64 x >>= fun hdr ->
  read_int64 fd >>= fun len ->
  return (hdr, len)

let write_header fd (hdr_type, len) =
  write_int64 fd (int64_of_header_type hdr_type) >>= fun () ->
  write_int64 fd len

let check_conversion_script () =
  let open Unix in
  try return (access !Resources.legacy_conv_tool [X_OK])
  with _ -> `Error (Failure (Printf.sprintf "Executable not found: %s" !Resources.legacy_conv_tool))

type 'a thread_status = Running | Thread_failure of exn | Success of 'a

let with_conversion_script task name hvm fd f =
  let module D = Debug.Make(struct let name = "suspend_image_conversion" end) in
  let open D in
  let open Stdext in
  let open Pervasiveext in
  let open Threadext in
  check_conversion_script () >>= fun () ->
  let (pipe_r, pipe_w) = Unix.pipe () in
  let fd_uuid = Uuidm.(to_string (create `V4))
  and pipe_w_uuid = Uuidm.(to_string (create `V4)) in
  let conv_script = !Resources.legacy_conv_tool
  and args =
    [ "--in"; fd_uuid; "--out"; pipe_w_uuid;
      "--width"; "32"; "--skip-qemu";
      "--guest-type"; if hvm then "hvm" else "pv";
      "--syslog"; "--verbose";
    ]
  in
  let (m, c) = Mutex.create (), Condition.create () in
  let spawn_thread_and_close_fd name fd' f =
    let status = ref Running in
    let thread =
      Thread.create (fun () ->
          try
            let result =
              finally (fun () -> f ()) (fun () -> Unix.close fd')
            in
            Mutex.execute m (fun () ->
                status := Success result;
                Condition.signal c
              )
          with e ->
            Mutex.execute m (fun () ->
                status := Thread_failure e;
                Condition.signal c
              )
        ) ()
    in
    (thread, status)
  in
  let (conv_th, conv_st) =
    spawn_thread_and_close_fd "convert-legacy-stream" pipe_w (fun () ->
        debug "Executing %s with args [ %s ]"
          conv_script (String.concat "; " args);
        Cancellable_subprocess.run task
          [ fd_uuid, fd; pipe_w_uuid, pipe_w; ] conv_script args
      )
  and (f_th, f_st) =
    spawn_thread_and_close_fd name pipe_r (fun () ->
        f pipe_r
      )
  in
  debug "Spawned threads for conversion script and %s" name;
  let rec handle_threads () = match (!conv_st, !f_st) with
    | Thread_failure e, _ ->
      begin match e with
        | Forkhelpers.Spawn_internal_error(_,_,status) ->
          begin match status with
            | Unix.WEXITED n ->
              `Error (Failure (Printf.sprintf "Conversion script exited with code %d" n))
            | Unix.WSIGNALED n ->
              `Error (Failure (Printf.sprintf "Conversion script exited with signal %s" (Unixext.string_of_signal n)))
            | Unix.WSTOPPED n ->
              `Error (Failure (Printf.sprintf "Conversion script stopped with signal %s" (Unixext.string_of_signal n)))
          end
        | _ ->
          `Error (Failure (Printf.sprintf "Conversion script thread caught exception: %s"
                             (Printexc.to_string e)))
      end
    | _, Thread_failure e ->
      `Error (Failure (Printf.sprintf "Thread executing %s caught exception: %s"
                         name (Printexc.to_string e)))
    | Running, _ | _, Running ->
      Condition.wait c m;
      handle_threads ()
    | Success _, Success res ->
      debug "Waiting for conversion script thread to join";
      Thread.join conv_th;
      debug "Waiting for xenguest thread to join";
      Thread.join f_th;
      `Ok res
  in
  Mutex.execute m handle_threads
