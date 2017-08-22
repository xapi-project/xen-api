(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
(*
 * RRD Unix module
 * This module provides Unix tools for dealing with RRDs
 *)
(**
 * @group Performance Monitoring
*)

let finally fct clean_f =
  let result =
    try
      fct ()
    with
      exn ->
      clean_f ();
      raise exn
  in
  clean_f ();
  result

let temp_file_in_dir otherfile =
  let base_dir = Filename.dirname otherfile in
  let rec keep_trying () =
    try
      let uuid = Uuidm.to_string (Uuidm.create `V4) in
      let newfile = base_dir ^ "/" ^ uuid in
      Unix.close (Unix.openfile newfile [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_EXCL] 0o600);
      newfile
    with
      Unix.Unix_error (Unix.EEXIST, _, _)  -> keep_trying ()
  in
  keep_trying ()

let unlink_safe file =
  try Unix.unlink file with (* Unix.Unix_error (Unix.ENOENT, _ , _)*) _ -> ()

let atomic_write_to_file fname perms f =
  let tmp = temp_file_in_dir fname in
  Unix.chmod tmp perms;
  finally
    (fun () ->
       let fd = Unix.openfile tmp [Unix.O_WRONLY; Unix.O_CREAT] perms (* ignored since the file exists *) in
       let result = finally
           (fun () -> f fd)
           (fun () -> Unix.close fd) in
       Unix.rename tmp fname; (* Nb this only happens if an exception wasn't raised in the application of f *)
       result)
    (fun () -> unlink_safe tmp)

let fd_blocks_fold block_size f start fd =
  let block = Bytes.create block_size in
  let rec fold acc =
    let n = Unix.read fd block 0 block_size in
    (* Consider making the interface explicitly use Substrings *)
    let s = if n = block_size then block else String.sub block 0 n in
    if n = 0 then acc else fold (f acc s) in
  fold start

let buffer_of_fd fd =
  fd_blocks_fold 1024 (fun b s -> Buffer.add_string b s; b) (Buffer.create 1024) fd

let with_file file mode perms f =
  let fd = Unix.openfile file mode perms in
  let r =
    try f fd
    with exn -> Unix.close fd; raise exn
  in
  Unix.close fd;
  r

let buffer_of_file file_path = with_file file_path [ Unix.O_RDONLY ] 0 buffer_of_fd

let string_of_file file_path = Buffer.contents (buffer_of_file file_path)








let of_file filename =
  let body = string_of_file filename in
  let input = Xmlm.make_input (`String (0,body)) in
  Rrd.from_xml input

let with_out_channel_output fd f =
  let oc = Unix.out_channel_of_descr fd in
  finally
    (fun () ->
      let output = Xmlm.make_output (`Channel oc) in
      f output)
    (fun () -> flush oc)

let xml_to_fd rrd fd =
  with_out_channel_output fd (Rrd.xml_to_output rrd)

let json_to_fd rrd fd =
  let string = Rrd.json_to_string rrd in
  let really_write n =
    let written = ref 0 in
    while !written < n
    do
      let wr = Unix.write fd string (!written) (n - !written) in
      written := wr + !written
    done
  in
  really_write (String.length string)

let to_fd ?(json=false) rrd fd = (if json then json_to_fd else xml_to_fd) rrd fd

let to_file ?(json=false) rrd filename =
  atomic_write_to_file filename 0o644 (to_fd ~json rrd)

(** WARNING WARNING: Do not call the following function from within xapi! *)
let text_export rrd _grouping =
  let open Rrd in
  for rra_i=0 to Array.length rrd.rrd_rras - 1 do
    let rra = rrd.rrd_rras.(rra_i) in
    let start = rrd.last_updated -. (Int64.to_float (Int64.mul (Int64.of_int (rra.rra_pdp_cnt * rra.rra_row_cnt)) rrd.timestep)) in
    Printf.printf "start=%f\n" start;
    let rra_timestep = (Int64.mul rrd.timestep (Int64.of_int rra.rra_pdp_cnt)) in

    (* Get the last and first times of the CDPs to be returned *)
    let (last_cdp_time,_age) = get_times rrd.last_updated rra_timestep in

    let time i = Int64.sub (last_cdp_time) (Int64.mul (Int64.of_int i) rra_timestep) in

    for j=0 to Array.length rrd.rrd_dss - 1 do
      Printf.printf "Doing ds: %s\n" rrd.rrd_dss.(j).ds_name;
      let oc = open_out (Printf.sprintf "rrd_data_%s_%s_%Ld.dat" rrd.rrd_dss.(j).ds_name (cf_type_to_string rra.rra_cf) (Int64.mul (Int64.of_int (rra.rra_pdp_cnt* rra.rra_row_cnt)) rrd.timestep)) in
      let rec do_data i accum =
        if (time i < Int64.of_float start) || (i >= rra.rra_row_cnt) then (List.rev accum) else
          do_data (i+1) ((time i, Fring.peek rra.rra_data.(j)  i)::accum)
      in
      let data = do_data 0 [] in
      List.iter (fun (t,d) -> if not (Utils.isnan d) then Printf.fprintf oc "%Ld %f\n" t d) data;
      close_out oc
    done
  done




