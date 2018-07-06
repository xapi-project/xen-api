(*
 * Copyright (C) 2011-2013 Citrix Inc
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
open OUnit
open Lwt

open Vhd_format.Patterns
module Impl = Vhd_format.F.From_file(Vhd_format_lwt.IO)
open Impl
open Vhd_format.F
open Vhd_format_lwt.IO
open Vhd_format_lwt.Patterns_lwt

let cstruct_to_string c = String.escaped (Cstruct.to_string c)

let create () =
  let _ = Create_vhd.disk in
  ()

let diff () =
  let _ = Diff_vhd.disk in
  ()

let tmp_file_dir = "/tmp"
let disk_name_stem = tmp_file_dir ^ "/parse_test."
let disk_suffix = ".vhd"

let make_new_filename =
  let counter = ref 0 in
  fun () ->
    let this = !counter in
    incr counter;
    disk_name_stem ^ (string_of_int this) ^ disk_suffix

let fill_sector_with pattern =
  let b = Io_page.(to_cstruct (get 1)) in
  let b = Cstruct.sub b 0 512 in
  for i = 0 to 511 do
    Cstruct.set_char b i (pattern.[i mod (String.length pattern)])
  done;
  b

(* Create a dynamic disk, check headers *)
let check_empty_disk size =
  let filename = make_new_filename () in
  Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
  Vhd_IO.openchain filename false >>= fun vhd' ->
  assert_equal ~printer:Header.to_string ~cmp:Header.equal vhd.Vhd.header vhd'.Vhd.header;
  assert_equal ~printer:Footer.to_string vhd.Vhd.footer vhd'.Vhd.footer;
  assert_equal ~printer:BAT.to_string ~cmp:BAT.equal vhd.Vhd.bat vhd'.Vhd.bat;
  Vhd_IO.close vhd' >>= fun () ->
  Vhd_IO.close vhd

(* Create a disk, resize it, check headers *)
let check_resize size =
  let newsize = max 0L (Int64.pred size) in
  let filename = make_new_filename () in
  Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
  let vhd = Vhd.resize vhd newsize in
  Vhd_IO.close vhd >>= fun () ->
  Vhd_IO.openchain filename false >>= fun vhd' ->
  assert_equal ~printer:Int64.to_string newsize vhd.Vhd.footer.Footer.current_size;
  Vhd_IO.close vhd'

(* Create a snapshot, check headers *)
let check_empty_snapshot size =
  let filename = make_new_filename () in
  Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
  let filename = make_new_filename () in
  Vhd_IO.create_difference ~filename ~parent:vhd () >>= fun vhd' ->
  Vhd_IO.openchain filename false >>= fun vhd'' ->
  assert_equal ~printer:Header.to_string ~cmp:Header.equal vhd'.Vhd.header vhd''.Vhd.header;
  assert_equal ~printer:Footer.to_string vhd'.Vhd.footer vhd''.Vhd.footer;
  assert_equal ~printer:BAT.to_string ~cmp:BAT.equal vhd'.Vhd.bat vhd''.Vhd.bat;
  Vhd_IO.close vhd'' >>= fun () ->
  Vhd_IO.close vhd' >>= fun () ->
  Vhd_IO.close vhd

(* Check changing the parent works *)
let check_reparent () =
  let all_ones = fill_sector_with "1" in
  let all_twos = fill_sector_with "2" in
  let p1 = make_new_filename () in
  let size = Int64.mul 1024L 1024L in
  Vhd_IO.create_dynamic ~filename:p1 ~size () >>= fun vhd ->
  (* write '1' into block 0 *)
  Vhd_IO.write vhd 0L [ all_ones ] >>= fun () ->
  Vhd_IO.close vhd >>= fun () ->
  let p2 = make_new_filename () in
  Vhd_IO.create_dynamic ~filename:p2 ~size () >>= fun vhd ->
  (* write '2' into block 0 *)
  Vhd_IO.write vhd 0L [ all_twos ] >>= fun () ->
  Vhd_IO.close vhd >>= fun () ->
  let l = make_new_filename () in
  Vhd_IO.openchain p1 false >>= fun vhd ->
  Vhd_IO.create_difference ~filename:l ~parent:vhd () >>= fun vhd' ->
  (* Verify block 0 has '1' *)
  let sector = fill_sector_with "0" in
  Vhd_IO.read_sector vhd' 0L sector >>= fun _ ->
  assert_equal ~printer:cstruct_to_string ~cmp:cstruct_equal all_ones sector;
  Vhd_IO.close vhd' >>= fun () ->
  Vhd_IO.close vhd >>= fun () ->
  (* Flip the parent locator *)
  Vhd_IO.openfile l true >>= fun vhd' ->
  let header = Header.set_parent vhd'.Vhd.header p2 in
  let vhd' = { vhd' with Vhd.header } in
  Vhd_IO.close vhd' >>= fun () ->
  Vhd_IO.openchain l false >>= fun vhd ->
  (* Verify block 0 has '2' *)
  let sector = fill_sector_with "0" in
  Vhd_IO.read_sector vhd 0L sector >>= fun _ ->
  assert_equal ~printer:cstruct_to_string ~cmp:cstruct_equal all_twos sector;
  Vhd_IO.close vhd

(* Check ../ works in parent locator *)
let check_parent_parent_dir () =
  let filename = make_new_filename () in
  Vhd_IO.create_dynamic ~filename ~size:0L () >>= fun vhd ->
  let leaf_path = Filename.(concat (concat tmp_file_dir "leaves") "leaf.vhd") in
  let leaf_dir = Filename.dirname leaf_path in
  (try Unix.mkdir leaf_dir 0o0755 with _ -> ());
  Vhd_IO.create_difference ~filename:leaf_path ~parent:vhd ~relative_path:true () >>= fun vhd' ->
  (* Make sure we can open the leaf *)
  Vhd_IO.openchain leaf_path false >>= fun vhd'' ->
  Vhd_IO.close vhd'' >>= fun () ->
  Vhd_IO.close vhd' >>= fun () ->
  Vhd_IO.close vhd

(* Check we respect RO-ness *)
let check_readonly () =
  let filename = make_new_filename () in
  Vhd_IO.create_dynamic ~filename ~size:0L () >>= fun vhd ->
  Vhd_IO.close vhd >>= fun () ->
  Unix.chmod filename 0o444;
  Vhd_IO.openchain filename false >>= fun vhd ->
  Vhd_IO.close vhd

let absolute_sector_of vhd { block; sector } =
  if vhd.Vhd.header.Header.max_table_entries = 0
  then None
  else
    let block = match block with
    | First -> 0
    | Last -> vhd.Vhd.header.Header.max_table_entries - 1 in
    let sectors_per_block = 1 lsl vhd.Vhd.header.Header.block_size_sectors_shift in
    let relative_sector = match sector with
    | First -> 0
    | Last -> sectors_per_block - 1 in
    Some (Int64.(add(mul (of_int block) (of_int sectors_per_block)) (of_int relative_sector)))

let cstruct_to_string c = String.escaped (Cstruct.to_string c)

type state = {
  to_close: fd Vhd.t list;
  to_unlink: string list;
  child: fd Vhd.t option;
  contents: (int64 * Cstruct.t) list;
}

let initial = {
  to_close = [];
  to_unlink = [];
  child = None;
  contents = [];
}

let sectors = Hashtbl.create 16
let sector_lookup message =
  if Hashtbl.mem sectors message
  then Hashtbl.find sectors message
  else
    let data = fill_sector_with message in
    Hashtbl.replace sectors message data;
    data

let execute state = function
  | Create size ->
    let filename = make_new_filename () in
    Vhd_IO.create_dynamic ~filename ~size () >>= fun vhd ->
    return {
      to_close = vhd :: state.to_close;
      to_unlink = filename :: state.to_unlink;
      child = Some vhd;
      contents = [];
    }
  | Snapshot ->
    let vhd = match state.child with
    | Some vhd -> vhd
    | None -> failwith "no vhd open" in
    let filename = make_new_filename () in
    Vhd_IO.create_difference ~filename ~parent:vhd () >>= fun vhd' ->
    return {
      to_close = vhd' :: state.to_close;
      to_unlink = filename :: state.to_unlink;
      child = Some vhd';
      contents = state.contents;
    }
  | Write (position, message) ->
    let data = sector_lookup message in
    let vhd = match state.child with
    | Some vhd -> vhd
    | None -> failwith "no vhd open" in
    begin match absolute_sector_of vhd position with
      | Some sector ->
        Vhd_IO.write vhd sector [ data ] >>= fun () ->
        (* Overwrite means we forget any previous contents *)
        let contents = List.filter (fun (x, _) -> x <> sector) state.contents in
        return { state with contents = (sector, data) :: contents }
      | None ->
        return state
    end

let verify state = match state.child with
  | None -> return ()
  | Some t -> verify t state.contents

module In = From_input(Input)
open In

let stream_vhd filename =
  Vhd_format_lwt.IO.openfile filename false >>= fun fd ->
  let rec loop = function
    | End -> return ()
    | Cons (hd, tl) ->
(*      begin match hd with
      | Fragment.Header x ->
        Printf.printf "Header\n%!"
      | Fragment.Footer x ->
        Printf.printf "Footer\n%!"
      | Fragment.BAT x ->
        Printf.printf "BAT\n%!"
      | Fragment.Batmap x ->
        Printf.printf "batmap\n%!"
      | Fragment.Block (offset, buffer) ->
        Printf.printf "Block %Ld (len %d)\n%!" offset (Cstruct.len buffer)
	end;*)
      tl () >>= fun x ->
      loop x in
  openstream (Input.of_fd (Vhd_format_lwt.IO.to_file_descr fd)) >>= fun stream ->
  loop stream >>= fun () -> Vhd_format_lwt.IO.close fd

let stream_test state =
  Lwt_list.iter_s stream_vhd state.to_unlink

let cleanup state =
  List.iter Unix.unlink state.to_unlink;
  Lwt.return ()

let run program =
  let single_instruction state x =
    execute state x >>= fun state' ->
    verify state' >>= fun () ->
    return state' in
  Lwt_list.fold_left_s single_instruction initial program >>= fun final_state ->
  Lwt_list.iter_s Vhd_IO.close final_state.to_close >>= fun () ->
  Lwt.catch
    (fun () -> stream_test final_state)
    (fun e -> Printf.fprintf stderr "Caught: %s\n%!" (Printexc.to_string e); fail e)
  >>= fun () ->
  cleanup final_state

let all_program_tests = List.map (fun p ->
  (string_of_program p) >:: (fun () -> Lwt_main.run (run p))
) programs

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test vhd parser";

  let check_empty_disk size =
    Printf.sprintf "check_empty_disk_%Ld" size
    >:: (fun () -> Lwt_main.run (check_empty_disk size)) in

  let check_resize size =
    Printf.sprintf "check_resize_%Ld" size
    >:: (fun () -> Lwt_main.run (check_resize size)) in

  let check_empty_snapshot size =
    Printf.sprintf "check_empty_snapshot_%Ld" size
    >:: (fun () -> Lwt_main.run (check_empty_snapshot size)) in

  (* Switch to the 'nobody' user so we can test file permissions *)
  let nobody = Unix.getpwnam "nobody" in
  begin
    try
      Unix.setuid nobody.Unix.pw_uid;
    with e ->
      Printf.fprintf stderr "WARNING: failed to setuid to a non-priviledged user, access control tests will pass spuriously (%s)\n%!" (Printexc.to_string e)
  end;
  let suite = "vhd" >:::
    [
      "create" >:: create;
      "check_parent_parent_dir" >:: (fun () -> Lwt_main.run (check_parent_parent_dir ()));
      "check_readonly" >:: (fun () -> Lwt_main.run (check_readonly ()));
      "check_reparent" >:: (fun () -> Lwt_main.run (check_reparent ()));
     ] @ (List.map check_empty_disk sizes)
       @ (List.map check_resize sizes)
       @ (List.map check_empty_snapshot sizes)
       @ all_program_tests in
  run_test_tt ~verbose:!verbose suite

