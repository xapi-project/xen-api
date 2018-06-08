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

let () =
  Quicktest_args.parse ();

  Stunnel.set_good_ciphersuites "!EXPORT:RSA+AES128-SHA256";
  let s = Quicktest_common.init_session !Quicktest_args.username !Quicktest_args.password in

  let suite =
    [ "Quicktest_example", Quicktest_example.tests s
    ; "cbt", Quicktest_cbt.tests s
    ; "event", Quicktest_event.tests s
    ; "import_raw_vdi", Quicktest_import_raw_vdi.tests s
    ; "copy", Quicktest_vdi_copy.tests s
    ; "SR tests", Quicktest_sr.tests s
    ; "Quicktest_vdi", Quicktest_vdi.tests s
    ; "Quicktest_iso_sr", Quicktest_iso_sr.tests s
    ; "Quicktest_async_calls", Quicktest_async_calls.tests s
    ; "Quicktest_vm_import_export", Quicktest_vm_import_export.tests s
    ; "Quicktest_vdi_ops_data_integrity", Quicktest_vdi_ops_data_integrity.tests s
    ]
    @ (if not !Quicktest_args.using_unix_domain_socket then
         ["http", Quicktest_http.tests]
       else [])
  in

  let argv = Quicktest_args.get_alcotest_args () in
  Alcotest.run ~argv "Quicktests" suite
