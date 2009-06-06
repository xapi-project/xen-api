
open Debug
open Bootloader

let _ = 
  let disk = ref "" in
  Arg.parse [ 
    "-debug", Arg.Set debug_flag, "enable debug output"
  ]
    (fun x -> 
       if !disk = "" then disk := x else
       warn ("Ignoring unexpected extra argument: " ^ x))
    "Test code for pygrub wrapper";  

  let disk = !disk in
  if disk = "" then failwith "You must supply a disk name as an argument";

  extract_default_kernel disk
