(*
 * Copyright (c) Cloud Software Group, Inc.
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

open Helpers
open Tar_ext
open Bos

let ( // ) = Filename.concat

let gen_test_file_script = "test_data" // "gen_tar_ext_test_file.sh"

(* The test file generating script 'gen_tar_ext_test_file.sh' will create a tar
   file 'test_tar_ext_unpacked_exceeds_max_size.tar' of 3MB. Setting
   'max_size_limit' to 2MB will trigger the error 'Unpacked_exceeds_max_size_limit'.
*)
let max_size_limit = 2 * 1024 * 1024 |> Int64.of_int

let create_temp_dir () =
  let mktemp = Cmd.v "mktemp" in
  let mktemp' = Cmd.(mktemp % "-d") in
  let result = OS.Cmd.(run_out mktemp' |> to_string) in
  match result with
  | Ok path ->
      path
  | Error (`Msg s) ->
      Alcotest.fail
        (Printf.sprintf "Test tar_ext creating temp dir failure: %s" s)

let test_file_dir = create_temp_dir ()

let unpack_dir = test_file_dir // "output"

type test_case = {
    description: string
  ; test_file: string
  ; expected: (unit, unpack_error) result
}

let test_cases =
  [
    {
      description= "Test regular tar file"
    ; test_file= "test_tar_ext_regular.tar"
    ; expected= Ok ()
    }
  ; {
      description= "Test tar file with illegal path"
    ; test_file= "test_tar_ext_illegal_path.tar"
    ; expected= Error (Illegal_file_path "test_illegal_dir/../file1.txt")
    }
  ; {
      description= "Test tar file trying to escape the current dir"
    ; test_file= "current_dir" // "test_tar_ext_trying_to_escape.tar"
    ; expected=
        Error (Illegal_file_path "current_dir/../another_dir/escaped_file")
    }
  ; {
      description= "Test tar file with absolute path starting from '/'"
    ; test_file= "test_tar_ext_absolute_path.tar"
    ; expected= Error (Illegal_file_path "/usr/bin/ls")
    }
  ; {
      description= "Test tar file with unsupported file type"
    ; test_file= "test_tar_ext_unsupported_file_type.tar"
    ; expected= Error (Unsupported_file_type "Symbolic")
    }
  ; {
      description= "Test unpacked exceeds max size limit"
    ; test_file= "test_tar_ext_unpacked_exceeds_max_size.tar"
    ; expected= Error (Unpacked_exceeds_max_size_limit max_size_limit)
    }
  ; {
      description= "Test unpacked file size mismatch"
    ; test_file= "test_tar_ext_file_size_mismatch.tar"
    ; expected=
        Error
          (File_size_mismatch
             {
               path= unpack_dir // "file1"
             ; expected_size= 1_048_576L
             ; actual_size= 99_488L
             }
          )
    }
  ; {
      description= "Test file incomplete"
    ; test_file= "test_tar_ext_file_incomplete.tar"
    ; expected= Error File_incomplete
    }
  ; {
      description= "Test corrupted tar file"
    ; test_file= "test_tar_ext_corrupted_file.tar"
    ; expected= Error File_corrupted
    }
  ; {
      description= "Test file unpacking failure"
    ; test_file= "test_tar_ext_unpacking_failure.tar"
    ; expected= Error Unpacking_failure
    }
  ]

let prepare_env () =
  let bash = Cmd.v "bash" in
  let gen = Cmd.(bash % gen_test_file_script % test_file_dir) in
  let result = OS.Cmd.(run_out gen |> out_null |> success) in
  match result with
  | Ok () ->
      ()
  | Error (`Msg s) ->
      Alcotest.fail (Printf.sprintf "Test tar_ext preparing failure: %s" s)

let test {test_file; expected; _} () =
  let unpack () =
    Unixext.with_file (test_file_dir // test_file) [Unix.O_RDONLY] 0o644
      (fun ifd -> Tar_ext.unpack_tar_file ~dir:unpack_dir ~ifd ~max_size_limit
    )
  in
  let result = unpack () in
  match (expected, result) with
  | Ok (), Ok () ->
      let file1_content = Unixext.string_of_file (unpack_dir // "file1.txt") in
      let file2_content = Unixext.string_of_file (unpack_dir // "file2.txt") in
      Alcotest.(check string)
        "Unpacking file inconsistent" "This is file-1\n" file1_content ;
      Alcotest.(check string)
        "Unpacking file inconsistent" "This is file-2\n" file2_content
  | Error exp, Error acl ->
      Alcotest.(check string)
        "Unpacking Error inconsistent"
        (unpack_error_to_string exp)
        (unpack_error_to_string acl)
  | Ok (), Error acl ->
      Alcotest.fail
        (Printf.sprintf
           "Unpacking result inconsistent, expected: Ok, actual: %s"
           (unpack_error_to_string acl)
        )
  | Error exp, Ok () ->
      Alcotest.fail
        (Printf.sprintf
           "Unpacking result inconsistent, expected: %s, actual: Success"
           (unpack_error_to_string exp)
        )

let clean_env () = Unixext.rm_rec test_file_dir

let generate_tests case = (case.description, `Quick, test case)

let tests =
  (("prepare_env", `Quick, prepare_env) :: List.map generate_tests test_cases)
  @ [("clean_env", `Quick, clean_env)]

let () =
  Suite_init.harness_init () ;
  Alcotest.run "Test Tar_ext suite" [("Test_tar_ext", tests)]
