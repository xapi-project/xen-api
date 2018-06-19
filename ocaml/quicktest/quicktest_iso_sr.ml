
let iso_path = Quicktest_args.iso_path

(** ISO SR should be able to create VDIs *)
let packages_iso_test rpc session_id () =
  let host = List.hd (Client.Client.Host.get_all rpc session_id) in
  print_endline (Printf.sprintf "Will plug into host %s" (Client.Client.Host.get_name_label rpc session_id host));
  let sr = Client.Client.SR.introduce ~rpc ~session_id ~uuid:(Uuid.string_of_uuid (Uuid.make_uuid ()))
      ~name_label:"test tools SR" ~name_description:"" ~_type:"iso" ~content_type:"iso"
      ~shared:true ~sm_config:[] in
  Xapi_stdext_pervasives.Pervasiveext.finally
    (fun () ->
       let device_config = [ "location", !iso_path;
                             "legacy_mode", "true" ] in
       let pbd = Client.Client.PBD.create ~rpc ~session_id ~sR:sr ~host ~device_config ~other_config:[] in
       Xapi_stdext_pervasives.Pervasiveext.finally
         (fun () ->
            print_endline "Plugging PBD";
            Client.Client.PBD.plug rpc session_id pbd;
            Client.Client.SR.scan rpc session_id sr;
            let is_iso x = Astring.String.is_suffix ~affix:".iso" (String.lowercase_ascii x) in
            let files = List.filter is_iso (Array.to_list (Sys.readdir !iso_path)) in
            let vdis = Client.Client.SR.get_VDIs rpc session_id sr in
            print_endline (Printf.sprintf "SR.scan found %d files (directory has %d .isos)" (List.length vdis) (List.length files));
            if List.length files <> List.length vdis then begin
              Alcotest.fail (Printf.sprintf "%s has %d files; SR has %d VDIs" !iso_path (List.length files) (List.length vdis));
            end;
            let locations = List.map (fun vdi -> Client.Client.VDI.get_location rpc session_id vdi) vdis in
            (* Check each file has a VDI.location *)
            List.iter (fun file ->
                if not(List.mem file locations) then begin
                  Alcotest.fail (Printf.sprintf "ISO %s has no corresponding VDI" file);
                end) files;
            (* Check each VDI is read-only *)
            List.iter (fun vdi ->
                let vdir = Client.Client.VDI.get_record rpc session_id vdi in
                if not(vdir.API.vDI_read_only) then
                  Alcotest.failf "ISO VDI has read_only set to false (%s)" vdir.API.vDI_name_label;
                Printf.printf "ISO VDI %s looks ok\n" vdir.API.vDI_name_label;
              ) vdis;
         ) (fun () ->
             Client.Client.PBD.unplug rpc session_id pbd;
             Client.Client.PBD.destroy rpc session_id pbd)
    ) (fun () -> Client.Client.SR.forget ~rpc ~session_id ~sr)

let tests () =
  let open Qt_filter in
  [ ["packages_iso_test", `Slow, packages_iso_test] |> conn
  ]
  |> List.concat
