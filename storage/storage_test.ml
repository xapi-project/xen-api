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

open Storage_interface
open Storage_client

(* Principles: 1. we don't delete or manipulate VDIs we didn't create 2. we
   create VDIs with non-clashing names 3. we always clean up (as best we can)
   after every test. *)

(* We assume that no-one else has made VDIs with this name prefix: *)
let safe_prefix = Printf.sprintf "storage_test.%d" (Unix.getpid ())

let dbg = safe_prefix

let _vdi_create = "VDI_CREATE"

let _vdi_delete = "VDI_DELETE"

let _vdi_attach = "VDI_ATTACH"

let _vdi_detach = "VDI_DETACH"

let _vdi_activate = "VDI_ACTIVATE"

let _vdi_deactivate = "VDI_DEACTIVATE"

let _vdi_clone = "VDI_CLONE"

let _vdi_resize = "VDI_RESIZE"

(* Names which are likely to cause problems *)
let names =
  [
    (* start with an easy one *)
    "simple"
  ; ""
  ; "."
  ; ".."
  ; "/"
  ; "!"
  ; String.make 128 '0'
  ]

(* For each VDI we check that: 1. it shows up in a SR.scan 2. attach RO,
   activate, deactivate, detach works 3. attach RW, activate, deactivate, detach
   works *)

let vdi_exists sr vdi =
  let all = Client.SR.scan dbg sr in
  List.fold_left (fun acc vdi_info -> acc || vdi_info.vdi = vdi) false all

let create sr name_label =
  let vdi_info =
    {
      default_vdi_info with
      name_label= safe_prefix ^ "." ^ name_label
    ; virtual_size= 1000000000L
    }
  in
  let vdi = Client.VDI.create dbg sr vdi_info in
  assert (vdi_exists sr vdi.vdi) ;
  (* Check the disk has size >= the amount we requested *)
  assert (vdi.virtual_size >= vdi_info.virtual_size) ;
  vdi

let destroy sr vdi =
  Client.VDI.destroy dbg sr vdi.vdi ;
  assert (not (vdi_exists sr vdi.vdi))

let test_create_destroy sr n () = destroy sr (create sr n)

let attach_detach sr vdi read_write =
  let vm = Vm.of_string "0" in
  let _ = Client.VDI.attach dbg dbg sr vdi.vdi read_write in
  Client.VDI.detach dbg dbg sr vdi.vdi vm

let test_attach_detach sr n () =
  let vdi = create sr n in
  List.iter (attach_detach sr vdi) [true; false] ;
  destroy sr vdi

let attach_activate_deactivate_detach sr vdi read_write =
  let vm = Vm.of_string "0" in
  let _ = Client.VDI.attach dbg dbg sr vdi.vdi read_write in
  Client.VDI.activate dbg dbg sr vdi.vdi ;
  Client.VDI.deactivate dbg dbg sr vdi.vdi vm ;
  Client.VDI.detach dbg dbg sr vdi.vdi vm

let test_activate_deactivate sr n () =
  let vdi = create sr n in
  List.iter (attach_activate_deactivate_detach sr vdi) [true; false] ;
  destroy sr vdi

let test_clone sr n () =
  let vdi = create sr n in
  List.iter
    (fun _read_write ->
      (* Check whether the backend writes type=<something other than raw> *)
      let vdi = {vdi with sm_config= []} in
      let x = Client.VDI.clone dbg sr vdi in
      Client.VDI.destroy dbg sr x.vdi ;
      assert (List.mem_assoc "type" x.sm_config) ;
      assert (List.assoc "type" x.sm_config <> "raw"))
    [true; false] ;
  destroy sr vdi

let test_clone_attach sr n () =
  let vdi = create sr n in
  List.iter
    (fun read_write ->
      let vdis = Client.SR.scan dbg sr in
      let x = Client.VDI.clone dbg sr vdi in
      let vdis' = Client.SR.scan dbg sr in
      attach_activate_deactivate_detach sr x read_write ;
      Client.VDI.destroy dbg sr x.vdi ;
      assert (List.length vdis + 1 = List.length vdis'))
    [true; false] ;
  destroy sr vdi

let test_resize sr n () =
  let vdi = create sr n in
  let new_size_request = Int64.mul 2L vdi.virtual_size in
  let new_size_actual = Client.VDI.resize dbg sr vdi.vdi new_size_request in
  assert (new_size_actual >= new_size_request) ;
  destroy sr vdi

let vdi_create_destroy sr =
  ( "vdi_create_destroy"
  , List.map (fun n -> ("name " ^ n, `Quick, test_create_destroy sr n)) names )

let vdi_attach_detach sr =
  ( "vdi_attach_detach"
  , List.map (fun n -> ("name " ^ n, `Quick, test_attach_detach sr n)) names )

let vdi_activate_deactivate sr =
  ( "vdi_activate_deactivate"
  , List.map
      (fun n -> ("name " ^ n, `Quick, test_activate_deactivate sr n))
      names )

let vdi_clone sr =
  ("vdi_clone", List.map (fun n -> ("name " ^ n, `Quick, test_clone sr n)) names)

let vdi_clone_attach sr =
  ( "vdi_clone_attach"
  , List.map (fun n -> ("name " ^ n, `Quick, test_clone_attach sr n)) names )

let vdi_resize sr =
  ( "vdi_resize"
  , List.map (fun n -> ("name " ^ n, `Quick, test_resize sr n)) names )

open Cmdliner

let start verbose queue sr =
  match (queue, sr) with
  | Some queue, Some sr ->
      Storage_interface.queue_name := queue ;
      Xcp_client.use_switch := true ;
      let q = Client.Query.query dbg in
      let features =
        List.map
          (fun s ->
            try
              let i = String.index s '/' in
              Some
                ( String.sub s 0 i
                , Int64.of_string
                    (String.sub s (i + 1) (String.length s - i - 1)) )
            with _ -> Some (s, 1L))
          q.features
      in
      let features =
        List.fold_left
          (fun acc x -> match x with Some x -> x :: acc | None -> acc)
          [] features
      in
      let needs_capabilities caps suite =
        if
          List.fold_left
            (fun acc x -> acc && List.mem_assoc x features)
            true caps
        then
          [suite]
        else
          []
      in
      let suite =
        List.concat
          [
            needs_capabilities [_vdi_create; _vdi_delete] (vdi_create_destroy sr)
          ; needs_capabilities
              [_vdi_create; _vdi_delete; _vdi_attach; _vdi_detach]
              (vdi_attach_detach sr)
          ; needs_capabilities
              [
                _vdi_create
              ; _vdi_delete
              ; _vdi_attach
              ; _vdi_detach
              ; _vdi_activate
              ; _vdi_deactivate
              ]
              (vdi_activate_deactivate sr)
          ; needs_capabilities
              [_vdi_create; _vdi_delete; _vdi_clone]
              (vdi_clone sr)
          ; needs_capabilities
              [
                _vdi_create
              ; _vdi_delete
              ; _vdi_attach
              ; _vdi_detach
              ; _vdi_activate
              ; _vdi_deactivate
              ; _vdi_clone
              ]
              (vdi_clone_attach sr)
          ; needs_capabilities
              [_vdi_create; _vdi_delete; _vdi_resize]
              (vdi_resize sr)
          ]
      in
      Alcotest.run ~and_exit:false ~argv:[|Sys.argv.(0)|] "storage" suite
  | _, _ ->
      Printf.fprintf stderr "Please supply both a queue name and an SR\n%!" ;
      ()

let cmd =
  let doc = "Storage component test" in
  let man =
    [
      `S "DESCRIPTION"
    ; `P "Test a storage implementation via the SMAPI."
    ; `S "USAGE"
    ; `P "$(tname) <queue name> <sr name>"
    ; `P
        "-- test the service listening on <queue name> using the existing \
         attached SR <sr name>."
    ]
  in
  let verbose =
    let doc = "Print verbose output" in
    Arg.(value & flag & info ["verbose"; "v"] ~doc)
  in
  let queue =
    let doc = "The queue name where the storage implementation is listening." in
    Arg.(value & pos 0 (some string) None & info [] ~doc)
  in
  let sr_t =
    let parse s = Ok (Sr.of_string s) in
    let print fmt s = s |> Sr.string_of |> Arg.(conv_printer string) fmt in
    Arg.conv ~docv:"SR" (parse, print)
  in
  let sr =
    let doc = "The attached SR." in
    Arg.(value & pos 1 (some sr_t) None & info [] ~doc)
  in
  (Term.(const start $ verbose $ queue $ sr), Term.info "test" ~doc ~man)

let () = Term.exit @@ Term.eval ~catch:true cmd
