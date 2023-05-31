(*
 * Copyright (C) Cloud Software Group, Inc.
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

let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/root"

let dest = home ^ "/.config/systemd/user/xapi-envtest@.service"

let get_temp_file prefix suffix =
  (* /tmp visible to systemd may not match /tmp visible to test
     when running tests inside containers.
     Use a path in the current dir instead for testing.
  *)
  let name = Filename.temp_file ~temp_dir:(Sys.getcwd ()) prefix suffix in
  at_exit (fun () -> Xapi_stdext_unix.Unixext.unlink_safe name) ;
  name

let envout = get_temp_file "envout" "txt"

let unit =
  Printf.sprintf
    {|
  [Unit]
  Description=env test

  [Service]
  Type=simple
  ExecStart=/usr/bin/env
  StandardOutput=file:%s
|}
    envout

let init () =
  if Unix.geteuid () <> 0 then
    Fe_systemctl.set_test () ;
  let parent = Filename.dirname dest in
  Xapi_stdext_unix.Unixext.mkdir_rec parent 0o700 ;
  Xapi_stdext_unix.Unixext.write_string_to_file dest unit ;
  at_exit @@ fun () -> Xapi_stdext_unix.Unixext.unlink_safe dest

module StringMap = Map.Make (String)

let printable =
  Array.init 127 Char.chr
  |> Array.to_seq
  |> Seq.filter Astring.Char.Ascii.is_print
  |> String.of_seq

let test_systemctl () =
  init () ;
  let instance = Unix.gettimeofday () |> string_of_float in
  let service = "xapi-envtest@" ^ instance in
  let prefix = "XAPI_QUICKTEST" in
  let env =
    [
      (prefix ^ "FOO", "bar")
    ; (prefix ^ "FOO2", "bar2")
    ; (prefix ^ "PRINTABLE", printable)
    ]
    |> List.to_seq
    |> StringMap.of_seq
  in
  Fe_systemctl.set_properties ~env ~service () ;
  Fe_systemctl.start_templated ~template:"xapi-envtest" ~instance ;
  let (_ : Fe_systemctl.status) = Fe_systemctl.stop ~service in
  let actual_envs =
    envout
    |> Xapi_stdext_unix.Unixext.string_of_file
    |> String.split_on_char '\n'
    |> List.filter_map (Astring.String.cut ~sep:"=")
    |> List.filter (fun (k, _) -> String.starts_with ~prefix k)
  in
  if actual_envs = [] then
    Alcotest.failf "Environment cannot be empty" ;
  let cmp (k1, _) (k2, _) = String.compare k1 k2 in
  Alcotest.(check @@ slist (pair string string) cmp)
    "environment should match" (StringMap.bindings env) actual_envs

let tests () = [("systemctl", `Quick, test_systemctl)]
