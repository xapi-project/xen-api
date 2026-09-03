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

(** The main entry point of the quicktest executable *)

module Suite = struct
  type t = {
      name: string
    ; tags: Quicktest_args.tag list
    ; tests: (unit -> unit) Qt_filter.test_case list
  }

  let make ?(tags = []) name tests = {name; tags; tests}
end

let matches_tags (s : Suite.t) =
  List.for_all (fun t -> List.mem t s.tags) !Quicktest_args.with_tags
  && List.for_all
       (fun t -> not (List.mem t s.tags))
       !Quicktest_args.without_tags

let filter_by_tags =
  List.filter_map (fun (s : Suite.t) ->
      if matches_tags s then
        Some (s.name, s.tests)
      else
        None
  )

let qchecks =
  [("unixext", Unixext_test.tests); ("Timer", Test_timer.tests)]
  |> List.map @@ fun (name, test) ->
     (name, List.map QCheck_alcotest.(to_alcotest ~long:true) test)

let setup_tty () =
  let style_renderer =
    if !Quicktest_args.use_colour then
      (* use default style, auto-detect color support *)
      None
    else
      (* never use color *)
      Some `None
  in
  Fmt_tty.setup_std_outputs ?style_renderer ()

let wrap f =
  setup_tty () ;
  let open Quicktest_trace in
  Opentelemetry.Globals.service_name := "quicktest" ;
  TeeBackend.with_default_setup () @@ fun () ->
  Sys.catch_break true ;
  () |> Debug.with_thread_associated "quicktest" @@ fun () -> Qt_filter.wrap f

let () =
  Quicktest_args.parse () ;
  wrap (fun () ->
      let suites =
        let open Quicktest_args in
        [
          Suite.make "Quicktest_vm_calibrate_cleanup0"
            (Quicktest_vm_calibrate.tests_cleanup ())
        ; Suite.make "Quicktest_vm_calibrate" (Quicktest_vm_calibrate.tests ())
        ; Suite.make "Quicktest_vm_calibrate_cleanup1"
            (Quicktest_vm_calibrate.tests_cleanup ())
        ; Suite.make "Quicktest_vm_calibrate_cleanup00"
            (Quicktest_vm_calibrate.tests_cleanup ())
        ; Suite.make "Quicktest_vm_memory" (Quicktest_vm_memory.tests ())
        ; Suite.make "Quicktest_vm_calibrate_cleanup2"
            (Quicktest_vm_calibrate.tests_cleanup ())
        ; Suite.make "Quicktest_example" (Quicktest_example.tests ())
        ; Suite.make "Quicktest_message" (Quicktest_message.tests ())
        ; Suite.make "xenstore" (Quicktest_xenstore.tests ())
        ; Suite.make "cbt" ~tags:[Sr] (Quicktest_cbt.tests ())
        ; Suite.make "event" (Quicktest_event.tests ())
        ; Suite.make "import_raw_vdi" (Quicktest_import_raw_vdi.tests ())
        ; Suite.make "copy" ~tags:[Sr] (Quicktest_vdi_copy.tests ())
        ; Suite.make "SR tests" ~tags:[Sr] (Quicktest_sr.tests ())
        ; Suite.make "Quicktest_vdi" ~tags:[Sr] (Quicktest_vdi.tests ())
        ; Suite.make "Quicktest_async_calls" ~tags:[Sr]
            (Quicktest_async_calls.tests ())
        ; Suite.make "Quicktest_vm_import_export" ~tags:[Sr]
            (Quicktest_vm_import_export.tests ())
        ; Suite.make "Quicktest_vm_lifecycle" ~tags:[Sr]
            (Quicktest_vm_lifecycle.tests ())
        ; Suite.make "Quicktest_vm_snapshot" ~tags:[Sr]
            (Quicktest_vm_snapshot.tests ())
        ; Suite.make "Quicktest_vm_migration" ~tags:[Sr]
            (Quicktest_vm_migration.tests ())
        ; Suite.make "Quicktest_vdi_ops_data_integrity" ~tags:[Sr]
            (Quicktest_vdi_ops_data_integrity.tests ())
        ; Suite.make "Quicktest_max_vdi_size" ~tags:[Sr]
            (Quicktest_max_vdi_size.tests ())
        ; Suite.make "Quicktest_static_vdis" ~tags:[Sr]
            (Quicktest_static_vdis.tests ())
        ; Suite.make "Quicktest_date" (Quicktest_date.tests ())
        ; Suite.make "Quicktest_crypt_r" (Quicktest_crypt_r.tests ())
        ; Suite.make "Quicktest_rate_limit" (Quicktest_rate_limit.tests ())
        ]
        @ ( if not !using_unix_domain_socket then
              [Suite.make "http" Quicktest_http.tests]
            else
              []
          )
        @
        if not !skip_stress then
          List.map (fun (name, tests) -> Suite.make name tests) qchecks
        else
          []
      in
      let suites = suites |> filter_by_tags in
      (* Only list tests if asked, without running them *)
      if !Quicktest_args.list_tests then
        Printf.printf "%s\n"
          (Astring.String.concat ~sep:"," (List.map fst suites))
      else
        (* If -run-only parameter supplied, run specific suites from the list *)
        let suites =
          match !Quicktest_args.run_only with
          | Some tests ->
              List.filter_map
                (fun test_name ->
                  Option.map
                    (fun v -> (test_name, v))
                    (List.assoc_opt test_name suites)
                )
                (Astring.String.cuts ~sep:"," tests)
          | None ->
              suites
        in
        let argv = Quicktest_args.get_alcotest_args () in
        Alcotest.run ~and_exit:false ~argv "Quicktests" suites
  )
