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

let qchecks =
  [("unixext", Unixext_test.tests); ("Timer", Test_timer.tests)]
  |> List.map @@ fun (name, test) ->
     (name, List.map QCheck_alcotest.(to_alcotest ~long:true) test)

let () =
  Quicktest_args.parse () ;
  Qt_filter.wrap (fun () ->
      let suite =
        [
          ("Quicktest_example", Quicktest_example.tests ())
        ; ("Quicktest_message", Quicktest_message.tests ())
        ; ("xenstore", Quicktest_xenstore.tests ())
        ; ("cbt", Quicktest_cbt.tests ())
        ; ("event", Quicktest_event.tests ())
        ; ("import_raw_vdi", Quicktest_import_raw_vdi.tests ())
        ; ("copy", Quicktest_vdi_copy.tests ())
        ; ("SR tests", Quicktest_sr.tests ())
        ; ("Quicktest_vdi", Quicktest_vdi.tests ())
        ; ("Quicktest_async_calls", Quicktest_async_calls.tests ())
        ; ("Quicktest_vm_import_export", Quicktest_vm_import_export.tests ())
        ; ("Quicktest_vm_lifecycle", Quicktest_vm_lifecycle.tests ())
        ; ("Quicktest_vm_snapshot", Quicktest_vm_snapshot.tests ())
        ; ( "Quicktest_vdi_ops_data_integrity"
          , Quicktest_vdi_ops_data_integrity.tests ()
          )
        ; ("Quicktest_max_vdi_size", Quicktest_max_vdi_size.tests ())
        ; ("Quicktest_static_vdis", Quicktest_static_vdis.tests ())
        ; ("Quicktest_date", Quicktest_date.tests ())
        ; ("Quicktest_crypt_r", Quicktest_crypt_r.tests ())
        ; ("Quicktest_rate_limit", Quicktest_rate_limit.tests ())
        ]
        @ ( if not !Quicktest_args.using_unix_domain_socket then
              [("http", Quicktest_http.tests)]
            else
              []
          )
        @
        if not !Quicktest_args.skip_stress then
          qchecks
        else
          []
      in
      (* Only list tests if asked, without running them *)
      if !Quicktest_args.list_tests then
        Printf.printf "%s\n"
          (Astring.String.concat ~sep:"," (List.map fst suite))
      else
        (* If -run-only parameter supplied, run specific suites from the list *)
        let suite =
          match !Quicktest_args.run_only with
          | Some tests ->
              List.filter_map
                (fun test_name ->
                  Option.map
                    (fun v -> (test_name, v))
                    (List.assoc_opt test_name suite)
                )
                (Astring.String.cuts ~sep:"," tests)
          | None ->
              suite
        in
        let argv = Quicktest_args.get_alcotest_args () in
        Alcotest.run ~and_exit:false ~argv "Quicktests" suite
  )
