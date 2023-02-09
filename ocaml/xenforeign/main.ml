module Xfm = Xenctrlext.Xenforeignmemory

let usage_msg = "foreign-mapper <domid>"

let domid = ref None

let anon_fun param =
  match !domid with None -> domid := Some (int_of_string param) | _ -> ()

let defer f = Fun.protect ~finally:f

let () =
  Arg.parse [] anon_fun usage_msg ;
  let the_domid = Option.get !domid in

  let handle = Xfm.acquire () in
  defer (fun () -> Xfm.release handle) @@ fun () ->
  let prot = {Xfm.read= true; write= true; exec= false} in
  let tpm_addr = 0x110000L in
  Xfm.with_mapping handle the_domid prot [tpm_addr]
    ~on_unmap_failure:(Fun.const ())
  @@ fun mapping ->
  let readable_region = Bigarray.Array1.(create Char C_layout 1024) in
  for i = 0 to 1023 do
    readable_region.{i} <- mapping.{i}
  done ;
  Hex.(hexdump (of_bigstring readable_region))
