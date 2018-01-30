(*
 * Copyright (C) Citrix Systems Inc.
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

open Common

let make_temp_volume () =
  let path = Filename.temp_file Sys.argv.(0) "volume" in
  ignore_string (Common.run "dd" [ "if=/dev/zero"; "of=" ^ path; "seek=1024"; "bs=1M"; "count=1"]);
  finally
    (fun () ->
      ignore_string (Common.run "losetup" [ "-f"; path ]);
      (* /dev/loop0: [fd00]:1973802 (/tmp/SR.createc04251volume) *)
      let line = Common.run "losetup" [ "-j"; path ] in
      try
        let i = String.index line ' ' in
        String.sub line 0 (i - 1)
      with e ->
        error "Failed to parse output of losetup -j: [%s]" line;
        ignore_string (Common.run "losetup" [ "-d"; path ]);
        failwith (Printf.sprintf "Failed to parse output of losetup -j: [%s]" line)
    ) (fun () -> rm_f path)

let remove_temp_volume volume =
  ignore_string (Common.run "losetup" [ "-d"; volume ])

let vgcreate vg_name = function
  | [] -> failwith "I need at least 1 physical device to create a volume group"
  | d :: ds as devices ->
    List.iter
      (fun dev ->
        (* First destroy anything already on the device *)
        ignore_string (run "dd" [ "if=/dev/zero"; "of=" ^ dev; "bs=512"; "count=4" ]);
        ignore_string (run "pvcreate" [ "--metadatasize"; "10M"; dev ])
      ) devices;

    (* Create the VG on the first device *)
    ignore_string (run "vgcreate" [ vg_name; d ]);
    List.iter (fun dev -> ignore_string (run "vgextend" [ vg_name; dev ])) ds;
    ignore_string (run "vgchange" [ "-an"; vg_name ])

let lvcreate vg_name lv_name bytes =
  let size_mb = Int64.to_string (Int64.div (Int64.add 1048575L bytes) (1048576L)) in
  ignore_string (Common.run "lvcreate" [ "-L"; size_mb; "-n"; lv_name; vg_name; "-Z"; "n" ])

let lvremove vg_name lv_name =
  ignore_string(Common.run "lvremove" [ "-f"; Printf.sprintf "%s/%s" vg_name lv_name])

type lv = {
  name: string;
  size: int64;
}

let newline = Re_str.regexp_string "\n"
let whitespace = Re_str.regexp "[\n\r\t ]+"
let comma = Re_str.regexp_string ","

let to_lines output = List.filter (fun x -> x <> "") (Re_str.split_delim newline output)

let lvs vg_name =
  Common.run "lvs" [ "-o"; "lv_name,lv_size"; "--units"; "b"; "--noheadings"; vg_name ]
  |> to_lines
  |> List.map
    (fun line ->
      match List.filter (fun x -> x <> "") (Re_str.split_delim whitespace line) with
      | [ x; y ] ->
        let size = Int64.of_string (String.sub y 0 (String.length y - 1)) in
        { name = x; size }
      | _ ->
        debug "Couldn't parse the LV name/ size: [%s]" line;
        failwith (Printf.sprintf "Couldn't parse the LV name/ size: [%s]" line)
    )

let device vg_name lv_name = Printf.sprintf "/dev/%s/%s" vg_name lv_name

let lvresize vg_name lv_name size =
  let size_mb = Int64.div (Int64.add 1048575L size) (1048576L) in
  (* Check it's not already the correct size *)
  let out = Common.run "lvdisplay" [ vg_name ^ "/" ^ lv_name; "-C"; "-o"; "size"; "--noheadings"; "--units"; "m"] in
  (* Returns something of the form: "   40.00M\n" *)
  let cur_mb =
    try
      String.index out '.'
      |> String.sub out 0               (* ignore the decimals *)
      |> Re_str.split_delim whitespace
      |> List.filter (fun x -> x <> "") (* get rid of whitespace *)
      |> List.hd                        (* there should be only one thing ... *)
      |> Int64.of_string                (* ... and it should be a number *)
    with e ->
      error "Couldn't parse the lvdisplay output: [%s]" out;
      raise e in
  let size_mb_rounded = Int64.mul 4L (Int64.div (Int64.add size_mb 3L) 4L) in
  if cur_mb <> size_mb_rounded then begin
    debug "lvresize: current size is %Ld MiB <> requested size %Ld MiB (rounded from %Ld); resizing" cur_mb size_mb_rounded size_mb;
    ignore_string(Common.run ~stdin:"y\n" "lvresize" [ vg_name ^ "/" ^ lv_name; "-L"; Int64.to_string size_mb ])
  end

let vgs () =
  Common.run "vgs" [ "-o"; "name"; "--noheadings" ]
  |> to_lines
  |> List.map
    (fun line ->
      List.hd (List.filter (fun x -> x <> "") (Re_str.split_delim whitespace line))
    )

let dash = Re_str.regexp_string "-"
let path_of vg lv =
  let lv' = Re_str.split_delim dash lv in
  "/dev/mapper/" ^ vg ^ "-" ^ (String.concat "--" lv')

let volume_of_lv sr lv = {
  Xapi_storage.V.Types.key = lv.name;
  name = lv.name;
  description = "";
  read_write = true;
  uri = ["block://" ^ (path_of sr lv.name) ];
  virtual_size = lv.size;
}
