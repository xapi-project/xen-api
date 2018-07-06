
let _ =
  if Array.length Sys.argv <> 2
  then failwith (Printf.sprintf "Usage: %s <cdrom device>" Sys.argv.(0));
  let device = Sys.argv.(1) in

  let a, b = Cdrom.query_cdrom_status device in
  Printf.printf "drive status = %s\n" (Cdrom.string_of_cdrom_drive_status a);
  Printf.printf "disk status = %s\n" (Cdrom.string_of_cdrom_disc_status b)
