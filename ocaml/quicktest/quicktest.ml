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

let () =
  Quicktest_args.parse () ;
  Qt_filter.wrap (fun () ->
      let suite =
        [
          ("Quicktest_example", Quicktest_example.tests ())
        ; ("cbt", Quicktest_cbt.tests ())
        ; ("event", Quicktest_event.tests ())
        ; ("import_raw_vdi", Quicktest_import_raw_vdi.tests ())
        ; ("copy", Quicktest_vdi_copy.tests ())
        ; ("SR tests", Quicktest_sr.tests ())
        ; ("Quicktest_vdi", Quicktest_vdi.tests ())
        ; ("Quicktest_async_calls", Quicktest_async_calls.tests ())
        ; ("Quicktest_vm_import_export", Quicktest_vm_import_export.tests ())
        ; ("Quicktest_vm_lifecycle", Quicktest_vm_lifecycle.tests ())
        ; ( "Quicktest_vdi_ops_data_integrity"
          , Quicktest_vdi_ops_data_integrity.tests ()
          )
        ; ("Quicktest_max_vdi_size", Quicktest_max_vdi_size.tests ())
        ; ("Quicktest_static_vdis", Quicktest_static_vdis.tests ())
        ; ("Quicktest_date", Quicktest_date.tests ())
        ]
        @
        if not !Quicktest_args.using_unix_domain_socket then
          [("http", Quicktest_http.tests)]
        else
          []
      in
      let argv = Quicktest_args.get_alcotest_args () in
      Alcotest.run ~and_exit:false ~argv "Quicktests" suite
  )
