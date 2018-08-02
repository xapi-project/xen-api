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

open Suspend_image

let (|>) a b = b a

let opt_debug = ref true
let msg ~prefix s  = Printf.printf "%s: %s\n%!" prefix s
let debug fmt = Printf.ksprintf (fun s -> if !opt_debug then msg ~prefix:"debug" s) fmt
let error fmt = Printf.ksprintf (msg ~prefix:"error") fmt

let verify_libxc_v2_record fd =
  let fd_uuid = Uuidm.(to_string (create `V4)) in
  let path = !Resources.verify_libxc_v2 in
  let args = ["--in"; fd_uuid; "--syslog"] in
  begin
    try Unix.(access path [X_OK])
    with _ -> failwith (Printf.sprintf "Executable not found: %s" path)
  end;
  let pid =
    Forkhelpers.safe_close_and_exec
      None (Some Unix.stdout) (Some Unix.stderr) [fd_uuid, fd]
      path args
  in
  match Forkhelpers.waitpid pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> failwith "Failed to verify Libxc v2 record"

let parse_layout fd =
  debug "Reading save signature...";
  match read_save_signature fd with
  | `Error e ->
    error "Error reading save signature: %s" e;
    failwith e
  | `Ok Legacy -> []
  | `Ok Structured ->
    let open Suspend_image.M in
    let rec aux acc =
      debug "Reading header...";
      read_header fd >>= fun h ->
      debug "Read header <%s>" (string_of_header h);
      debug "Dummy-processing record...";
      begin match h with
        | Xenops, len ->
          Io.read fd (Io.int_of_int64_exn len) |> ignore;
          aux (h::acc)
        | Libxc, _ ->
          verify_libxc_v2_record fd;
          aux (h::acc)
        | Qemu_trad, len ->
          Io.read fd (Io.int_of_int64_exn len) |> ignore;
          aux (h::acc)
        | Varstored, len ->
          Io.read fd (Io.int_of_int64_exn len) |> ignore;
          aux (h :: acc)
        | End_of_image, _ -> return (h::acc)
        | Libxl, _ -> failwith "Unsupported: libxl"
        | Libxc_legacy, _ -> failwith "Unsupported: libxc-legacy"
        | Demu, _ -> failwith "Unsupported: demu"
        | Qemu_xen, _ -> failwith "Unsupported: qemu-xen"
      end
    in
    match aux [] with
    | `Ok hs -> List.rev hs
    | `Error e ->
      failwith (Printf.sprintf "Error parsing image: %s" (Printexc.to_string e))

let print_layout headers =
  let module S = String in
  let default_width = 10 in
  let max_header_word_length =
    List.map (fun h -> string_of_header h |> S.length) headers
    |> List.fold_left max default_width
  in
  let left_pad = "| " and right_pad = " |" in
  let col_width = max_header_word_length + (S.length left_pad) + (S.length right_pad) in
  Printf.printf "+%s+\n" (S.make (col_width - 2) '-');
  let rec inner = function
    | [] -> ()
    | h::hs ->
      let h_str = string_of_header h in
      let filled_space =
        List.map S.length [left_pad; h_str; right_pad]
        |> List.fold_left (+) 0
      in
      let padding = S.make (col_width - filled_space) ' ' in
      Printf.printf "%s%s%s%s\n" left_pad h_str padding right_pad;
      Printf.printf "+%s+\n" (S.make (col_width - 2) '-');
      inner hs
  in
  inner headers

module D = Debug.Make(struct let name = "suspend-image-viewer" end)

let print_image path =
  Stdext.Unixext.with_file path [Unix.O_RDONLY] 0o400 (fun fd ->
      print_layout (parse_layout fd)
    )

(* Command line interface *)
let () =
  let resources = Resources.make_resources ~essentials:Resources.essentials
      ~nonessentials:Resources.nonessentials in
  let doc = "Print the layout of a suspend image" in
  let path = ref "" in
  let options = [
    "path", Arg.Set_string path, (fun () -> !path), "Path to the suspend image device"
  ] in
  match Xcp_service.configure2 ~name:"suspend-image-viewer"
          ~version:Version.version ~resources ~doc  ~options () with
  | `Ok () ->
    print_image !path
  | `Error m ->
    error "%s" m;
    exit 1
