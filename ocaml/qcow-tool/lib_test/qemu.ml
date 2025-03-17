(*
 * Copyright (C) 2016 Unikernel Systems
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

(* Wrappers for qemu-img, qemu-nbd to allow us to compare the contents of
   ocaml-qcow images and qemu-produced images. *)
open Utils

module Img = struct
  let create file size =
    ignore_output @@ run "qemu-img" [ "create"; "-f"; "qcow2"; "-o"; "lazy_refcounts=on"; file; Int64.to_string size ];
    (* workaround for https://github.com/mirage/mirage-block-unix/issues/59 *)
    Lwt_main.run begin
      let open Lwt.Infix in
      Lwt_unix.LargeFile.stat file
      >>= fun stat ->
      let bytes = stat.Lwt_unix.LargeFile.st_size in
      let remainder = Int64.rem bytes 512L in
      let padding_required = if remainder = 0L then 0L else Int64.sub 512L remainder in
      Lwt_unix.openfile file [ Lwt_unix.O_WRONLY; Lwt_unix.O_APPEND ] 0o0
      >>= fun fd ->
      let buf = Cstruct.create (Int64.to_int padding_required) in
      Cstruct.memset buf 0;
      Lwt_cstruct.complete (Lwt_cstruct.write fd) buf
      >>= fun () ->
      Lwt_unix.close fd
    end

  let check file =
    ignore_output @@ run "qemu-img" [ "check"; file ]

  type info = {
    virtual_size: int64;
    filename: string;
    cluster_size: int;
    actual_size: int;
    compat: string;
    lazy_refcounts: bool option;
    refcount_bits: int option;
    corrupt: bool option;
    dirty_flag: bool;
  }

  let info file =
    let lines, _ = run "qemu-img" [ "info"; "--output"; "json"; file ] in
    let json = Ezjsonm.(get_dict @@ from_string @@ String.concat "\n" lines) in
    let find name json =
      if List.mem_assoc name json
      then List.assoc name json
      else failwith (Printf.sprintf "Failed to find '%s' in %s" name (String.concat "\n" lines)) in
    let virtual_size = Ezjsonm.get_int64 @@ find "virtual-size" json in
    let filename = Ezjsonm.get_string @@ find "filename" json in
    let cluster_size = Ezjsonm.get_int @@ find "cluster-size" json in
    let format = Ezjsonm.get_string @@ find "format" json in
    if format <> "qcow2" then failwith (Printf.sprintf "Expected qcow2 format, got %s" format);
    let actual_size = Ezjsonm.get_int @@ find "actual-size" json in
    let specific = Ezjsonm.get_dict @@ find "format-specific" json in
    let ty = Ezjsonm.get_string @@ find "type" specific in
    if ty <> "qcow2" then failwith (Printf.sprintf "Expected qcow2 type, got %s" ty);
    let data = Ezjsonm.get_dict @@ find "data" specific in
    let compat = Ezjsonm.get_string @@ find "compat" data in
    let lazy_refcounts = try Some (Ezjsonm.get_bool @@ find "lazy-refcounts" data) with _ -> None in
    let refcount_bits = try Some (Ezjsonm.get_int @@ find "refcount-bits" data) with _ -> None in
    let corrupt = try Some (Ezjsonm.get_bool @@ find "corrupt" data) with _ -> None in
    let dirty_flag = Ezjsonm.get_bool @@ find "dirty-flag" json in
    { virtual_size; filename; cluster_size; actual_size; compat;
      lazy_refcounts; refcount_bits; corrupt; dirty_flag }
end
