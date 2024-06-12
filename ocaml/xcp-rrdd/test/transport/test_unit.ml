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
open Test_common

let bracket setup test teardown () =
  let a = setup () in
  Fun.protect ~finally:(fun () -> teardown a) (fun () -> test a)

let test_file_io protocol =
  bracket
    (fun () ->
      let shared_file = make_shared_file () in
      let _, writer =
        Rrd_writer.FileWriter.create
          {Rrd_writer.path= shared_file; shared_page_count= 1}
          protocol
      in
      let reader = Rrd_reader.FileReader.create shared_file protocol in
      (writer, reader)
    )
    (fun (writer, reader) ->
      (* Check that writing then reading the shared file gives the expected
         timestamp and datasources. *)
      writer.Rrd_writer.write_payload test_payload ;
      let received_payload = reader.Rrd_reader.read_payload () in
      assert_payloads_equal test_payload received_payload
    )
    (fun (writer, reader) ->
      reader.Rrd_reader.cleanup () ;
      writer.Rrd_writer.cleanup ()
    )
    ()

let test_writer_cleanup protocol =
  let shared_file = make_shared_file () in
  let _, writer =
    Rrd_writer.FileWriter.create
      {Rrd_writer.path= shared_file; shared_page_count= 1}
      protocol
  in
  writer.Rrd_writer.write_payload test_payload ;
  writer.Rrd_writer.cleanup () ;
  Alcotest.(check bool)
    "Shared file was not cleaned up"
    (Sys.file_exists shared_file)
    false ;
  Alcotest.check_raises "write_payload should fail after cleanup"
    Rrd_io.Resource_closed (fun () ->
      writer.Rrd_writer.write_payload test_payload
  ) ;
  Alcotest.check_raises "Cleanup should fail after cleanup"
    Rrd_io.Resource_closed writer.Rrd_writer.cleanup

let test_reader_cleanup protocol =
  bracket
    (fun () ->
      let shared_file = make_shared_file () in
      let _, writer =
        Rrd_writer.FileWriter.create
          {Rrd_writer.path= shared_file; shared_page_count= 1}
          protocol
      in
      writer.Rrd_writer.write_payload test_payload ;
      (shared_file, writer)
    )
    (fun (shared_file, _writer) ->
      let reader = Rrd_reader.FileReader.create shared_file protocol in
      let (_ : Rrd_protocol.payload) = reader.Rrd_reader.read_payload () in
      reader.Rrd_reader.cleanup () ;
      Alcotest.check_raises "Read_payload should fail after cleanup"
        Rrd_io.Resource_closed (fun () ->
          let _ = reader.Rrd_reader.read_payload () in
          ()
      ) ;
      Alcotest.check_raises "Cleanup should fail after cleanup"
        Rrd_io.Resource_closed reader.Rrd_reader.cleanup
    )
    (fun (_, writer) -> writer.Rrd_writer.cleanup ())
    ()

let test_reader_state protocol =
  bracket
    (fun () ->
      let shared_file = make_shared_file () in
      let _, writer =
        Rrd_writer.FileWriter.create
          {Rrd_writer.path= shared_file; shared_page_count= 1}
          protocol
      in
      let reader = Rrd_reader.FileReader.create shared_file protocol in
      (writer, reader)
    )
    (fun (writer, reader) ->
      writer.Rrd_writer.write_payload test_payload ;
      let (_ : Rrd_protocol.payload) = reader.Rrd_reader.read_payload () in
      Alcotest.check_raises
        "read_payload should raise No_update if there has been no update"
        Rrd_protocol.No_update (fun () ->
          let _ = reader.Rrd_reader.read_payload () in
          ()
      ) ;
      (* After the timestamp has been updated, we should be able to read the
         payload again. *)
      let open Rrd_protocol in
      writer.Rrd_writer.write_payload
        {test_payload with timestamp= Int64.add test_payload.timestamp 5L} ;
      let (_ : Rrd_protocol.payload) = reader.Rrd_reader.read_payload () in
      ()
    )
    (fun (writer, reader) ->
      reader.Rrd_reader.cleanup () ;
      writer.Rrd_writer.cleanup ()
    )
    ()

let tests =
  Test_common.tests_for_all_protos
    [
      ("File I/O", test_file_io)
    ; ("Writer cleanup", test_writer_cleanup)
    ; ("Reader cleanup", test_reader_cleanup)
    ; ("Reader state", test_reader_state)
    ]

let () = Alcotest.run "Metrics transport" tests
