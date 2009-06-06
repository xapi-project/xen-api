(* Commandline utility to test PCI id parser *)

let _ = 
  let vendor = ref "" and device = ref "" in

  Arg.parse [ "-vendor", Arg.Set_string vendor, Printf.sprintf "Vendor id (default: %s)" !vendor;
	      "-device", Arg.Set_string device, Printf.sprintf "Device id (default: %s)" !device ]
    (fun x -> Printf.fprintf stderr "Ignoring unexpected argument: %s\n" x)
    "Test the PCI id parser";

  let v, d = Pciutil.parse !vendor !device in
  Printf.printf "vendor: %s\n" (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) v));
  Printf.printf "device: %s\n" (Opt.default "None" (Opt.map (fun x -> "Some " ^ x) d))
