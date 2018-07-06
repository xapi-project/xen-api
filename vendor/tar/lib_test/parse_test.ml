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
open Tar_lwt_unix
open Lwt

exception Cstruct_differ

let cstruct_equal a b =
  let check_contents a b =
    try
      for i = 0 to Cstruct.len a - 1 do
        let a' = Cstruct.get_char a i in
        let b' = Cstruct.get_char b i in
        if a' <> b' then raise Cstruct_differ
      done;
      true
    with _ -> false in
  (Cstruct.len a = (Cstruct.len b)) && (check_contents a b)

let header () =
  (* check header marshalling and unmarshalling *)
  let h = Header.make ~file_mode:5 ~user_id:1001 ~group_id:1002 ~mod_time:55L ~link_name:"" "hello" 1234L in
  let txt = "hello\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0000000005\0000001751\0000001752\00000000002322\00000000000067\0000005534\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" in
  let c = Cstruct.create (String.length txt) in
  Cstruct.blit_from_string txt 0 c 0 (String.length txt);
  let c' = Cstruct.create Header.length in
  for i = 0 to Header.length - 1 do Cstruct.set_uint8 c' i 0 done;
  Header.marshal c' h;
  assert_equal ~printer:(fun x -> String.escaped (Cstruct.to_string x)) ~cmp:cstruct_equal c c';
  let printer = function
    | None -> "None"
    | Some x -> "Some " ^ (Header.to_detailed_string x) in
  assert_equal ~printer (Some h) (Header.unmarshal c');
  assert_equal ~printer:string_of_int 302 (Header.compute_zero_padding_length h)

let set_difference a b = List.filter (fun a -> not(List.mem a b)) a

let finally f g = try let results = f () in g (); results with e -> g (); raise e

let with_temp_file f =
  let tar_filename = Filename.temp_file "tar-test" ".tar" in
  finally (fun () -> f tar_filename) (fun () -> Unix.unlink tar_filename)

let rm_rf dir =
  let rec loop file_or_dir =
    Printf.fprintf stderr "rm %s\n%!" file_or_dir;
    match Unix.unlink file_or_dir with
    | () -> ()
    | exception Unix.Unix_error((Unix.EISDIR | Unix.EPERM), _, _) ->
      Array.iter (fun name -> loop (Filename.concat file_or_dir name)) (Sys.readdir file_or_dir);
      Unix.rmdir file_or_dir in
  loop dir

let with_temp_dir f =
  let dir = Filename.(concat (get_temp_dir_name ()) (Printf.sprintf "test.%d" (Unix.getpid()))) in
  Unix.mkdir dir 0o0755;
  finally (fun () -> f dir) (fun () -> rm_rf dir)

let with_tar ?files f =
  let files = match files with
    | None -> List.map (fun x -> "lib/" ^ x) (Array.to_list (Sys.readdir "lib"))
    | Some files -> files in
  with_temp_file
    (fun tar_filename ->
      let tar_filename = Filename.temp_file "tar-test" ".tar" in
      let cmdline = Printf.sprintf "tar -cf %s %s" tar_filename (String.concat " " files) in
      begin match Unix.system cmdline with
        | Unix.WEXITED 0 -> ()
        | Unix.WEXITED n -> failwith (Printf.sprintf "%s: exited with %d" cmdline n)
        | _ -> failwith (Printf.sprintf "%s: unknown error" cmdline)
      end;
      f tar_filename files
    )

let can_read_tar () =
  with_tar
    (fun tar_filename files ->
       let fd = Unix.openfile tar_filename [ Unix.O_RDONLY ] 0 in
       let files' = List.map (fun t -> t.Tar_unix.Header.file_name) (Tar_unix.Archive.list fd) in
       Unix.close fd;
       let missing = set_difference files files' in
       let missing' = set_difference files' files in
       assert_equal ~printer:(String.concat "; ") [] missing;
       assert_equal ~printer:(String.concat "; ") [] missing'
    )

let can_write_pax () =
  let open Tar_unix in
  with_temp_file
    (fun filename ->
      (* This userid is too large for a regular ustar header *)
      let user_id = 2116562692 in
      (* Write a file which would need a pax header *)
      let filename = "/tmp/foo.tar" in
      let fd = Unix.openfile filename [ Unix.O_CREAT; Unix.O_WRONLY ] 0o0644 in
      finally
        (fun () ->
          let hdr = Header.make ~user_id "test" 0L in
          write_block hdr (fun _ -> ()) fd;
          write_end fd;
        ) (fun () -> Unix.close fd);
      (* Read it back and verify the header was read *)
      let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
      finally
        (fun () ->
          match Archive.list fd with
          | [ one ] -> assert (one.Header.user_id = user_id)
          | xs ->
            Printf.fprintf stderr "Headers = [ %s ]\n%!" (String.concat "; " (List.map Header.to_detailed_string xs));
            assert false
        ) (fun () -> Unix.close fd);
    )

let can_list_longlink_tar () =
  let open Tar_unix in
  with_temp_dir
    (fun dir ->
      let fd = Unix.openfile "lib_test/long.tar" [ Unix.O_RDONLY ] 0o0 in
      finally
        (fun () ->
          let all = Archive.list fd in
          let filenames = List.map (fun h -> h.Tar_unix.Header.file_name) all in
          (* List.iteri (fun i x -> Printf.fprintf stderr "%d: %s\n%!" i x) filenames; *)
          let expected = [
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/";
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/BCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/";
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/BCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/CDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.txt";
          ] in
          OUnit.assert_equal ~printer:(String.concat ", ") expected filenames
        ) (fun () -> Unix.close fd);
    )

let expect_ok = function
  | Ok x -> x
  | Error _ -> failwith "expect_ok: got Error"

module Block4096 = struct
  include Block

  let get_info b =
    Block.get_info b
    >>= fun info ->
    let size_sectors = Int64.(div (add info.size_sectors 7L) 8L) in
    return { info with Mirage_block.sector_size = 4096; size_sectors }

  let read b ofs bufs =
    Block.get_info b
    >>= fun info ->
    let len = List.fold_left (+) 0 (List.map Cstruct.len bufs) in
    let requested_end = Int64.(add (mul ofs 4096L) (of_int len)) in
    let end_of_file = Int64.(mul info.size_sectors (of_int info.sector_size)) in
    let need_to_trim = max 0L (Int64.sub requested_end end_of_file) |> Int64.to_int in
    let need_to_keep = len - need_to_trim in
    let rec trimmed len = function
      | [] -> []
      | b :: bs ->
        let b' = Cstruct.len b in
        for i = 0 to b' do Cstruct.set_uint8 b 0 0 done;
        let to_drop = max 0 (len + b' - need_to_keep) in
        let to_keep = max 0 (b' - to_drop) in
        Cstruct.sub b 0 to_keep :: (trimmed (len + b') bs) in
    let trimmed =  (trimmed 0 bufs) in
    Block.read b (Int64.mul ofs 8L) trimmed
  let connect name = connect name
end

module type BLOCK = sig
  include Mirage_types_lwt.BLOCK
  val connect: string -> t Lwt.t
end

module B = struct
  include Block

  let connect name = connect name
end

module Test(B: BLOCK) = struct
  let can_read_through_BLOCK ?files () =
    with_tar ?files
      (fun tar_filename files ->
         let t =
           B.connect tar_filename >>= fun b ->
           let module KV_RO = Tar_mirage.Make_KV_RO(B) in
           KV_RO.connect b >>= fun k ->
           Lwt_list.iter_s
             (fun file ->
                KV_RO.size k file
                >>= fun r ->
                let size = expect_ok r in
                let stats = Unix.LargeFile.stat file in
                assert_equal ~printer:Int64.to_string stats.Unix.LargeFile.st_size size;
                let read_file key ofs len =
                  let fd = Unix.openfile key [ Unix.O_RDONLY ] 0 in
                  finally
                    (fun () ->
                       let (_: int) = Unix.lseek fd ofs Unix.SEEK_SET in
                       let buf = Bytes.make len '\000' in
                       let len' = Unix.read fd buf 0 len in
                       assert_equal ~printer:string_of_int len len';
                       Bytes.to_string buf
                    ) (fun () -> Unix.close fd) in
                let read_tar key ofs len =
                  KV_RO.read k key ofs len
                  >>= function
                  | Error _ -> failwith "KV_RO.read"
                  | Ok bufs -> return (String.concat "" (List.map Cstruct.to_string bufs)) in
                (* Read whole file *)
                let size = stats.Unix.LargeFile.st_size in
                let value = read_file file 0 (Int64.to_int size) in
                read_tar file 0L size
                >>= fun value' ->
                assert_equal ~printer:(fun x -> x) value value';
                if Int64.compare size 2L = 1 then begin
                  let value = read_file file 1 ((Int64.to_int size) - 2) in
                  read_tar file 1L (Int64.sub size 2L)
                  >>= fun value' ->
                  assert_equal ~printer:(fun x -> x) value value';
                  return ()
                end else return ()
             ) files in
         Lwt_main.run t
      )
    let check_not_padded () =
      ignore (Unix.openfile "empty" [ Unix.O_CREAT; Unix.O_TRUNC ] 0o0644);
      can_read_through_BLOCK ~files:["empty"] ()
end

module Sector512 = Test(B)
module Sector4096 = Test(Block4096)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test tar parser";
  let suite = "tar" >:::
              [
                "header" >:: header;
                "can_read_tar" >:: can_read_tar;
                "can_read_through_BLOCK/512" >:: Sector512.can_read_through_BLOCK;
                "not 4KiB padded" >:: Sector512.check_not_padded;
                "can_read_through_BLOCK/4096" >:: Sector4096.can_read_through_BLOCK;
                "can write pax headers" >:: can_write_pax;
                "can read @Longlink" >:: can_list_longlink_tar;
              ] in
  (* pwd = _build/default/lib_test *)
  Unix.chdir "../../..";
  run_test_tt ~verbose:!verbose suite
