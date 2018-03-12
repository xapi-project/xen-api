open OUnit
open Pci

(* Helper functions *)
(*BISECT-IGNORE-BEGIN*)

let with_dump =
  with_access ~from_dump:(Filename.concat (Sys.getcwd ()) "dump.data")

let resident_pages () =
  let with_channel c f =
    try let r = f c in close_in c; r
    with exn -> close_in_noerr c; raise exn in
  let statm = with_channel (open_in "/proc/self/statm") input_line in
  Scanf.sscanf statm "%d %d %d %d %d %d %d" (fun _ res _ _ _ _ _ -> res)

(*BISECT-IGNORE-END*)
(* End helper functions *)

let smoke_test () =
  with_dump (fun a -> let (_: Pci_dev.t list) = get_devices a in ())

let test_with_access_cleanup () =
  (* Get overhead for calling the fuction and the measuremnt functions *)
  let _ = Gc.compact (); resident_pages () in
  for i = 1 to 6000 do with_dump ~cleanup:true (fun _ -> ()) done;
  let mem = Gc.compact (); resident_pages () in
  (* The incremental cost of calling with_access should be 0 *)
  for i = 1 to 1000 do with_dump ~cleanup:true (fun _ -> ()) done;
  let mem' = Gc.compact (); resident_pages () in
  assert_equal ~printer:(Printf.sprintf "VmRSS = %d pages") mem mem';
  (* Also check we don't leak when raising an exception *)
  for i = 1 to 1000 do
    try with_dump ~cleanup:true (fun _ -> failwith "") with Failure _ -> ()
  done;
  let mem'' = Gc.compact (); resident_pages () in
  assert_equal ~printer:(Printf.sprintf "VmRSS = %d pages") mem mem';
  (* Checking for a difference with cleanup=false as a negative test *)
  for i = 1 to 1000 do with_dump ~cleanup:false (fun _ -> ()) done;
  let mem''' = Gc.compact (); resident_pages () in
  assert_raises (OUnitTest.OUnit_failure "not equal") (fun () ->
      assert_equal mem'' mem''')

let test_lookup_functions () =
  (* Subset of `lspci -mnnv` on my system
     Class:  Bridge [0680]
     Vendor: Intel Corporation [8086]
     Device: 82371AB/EB/MB PIIX4 ACPI [7113]
     SVendor:        Red Hat, Inc [1af4]
     SDevice:        Qemu virtual machine [1100] *)
  let test_lookup = assert_equal ~printer:(fun x -> x) in
  let default v = match v with Some v -> v | None -> "" in
  with_dump (fun acc ->
      test_lookup "Bridge" @@ (lookup_class_name acc 0x0680 |> default);
      test_lookup "Intel Corporation" @@ (lookup_vendor_name acc 0x8086 |> default);
      test_lookup "82371AB/EB/MB PIIX4 ACPI" @@ (lookup_device_name acc 0x8086 0x7113 |> default);
      test_lookup "Red Hat, Inc" @@ (lookup_subsystem_vendor_name acc 0x1af4 |> default);
      test_lookup "Qemu virtual machine" @@ (lookup_subsystem_device_name acc 0x8086 0x7113 0x1af4 0x1100 |> default);
      test_lookup "VGA compatible controller" @@ (lookup_class_name acc 0x0300 |> default);
      test_lookup "VGA controller" @@ (lookup_progif_name acc 0x0300 0x00 |> default);
    )

let _ =
  let suite = "pci" >:::
              [
                "smoke_test" >:: smoke_test;
                "test_with_access_cleanup" >:: test_with_access_cleanup;
                "test_lookup_functions" >:: test_lookup_functions;
              ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite
