module D=Debug.Make(struct let name="forkexecd" end)
include D

let log_path = "/var/log/fe.log"

let debug_log = ref []

let gettimestring () =
  let time = Unix.gettimeofday () in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ|" (1900 + tm.Unix.tm_year)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let reset () = debug_log := []

let debug (fmt : ('a, unit, string, unit) format4) = 
  Printf.kprintf (fun s -> debug_log := Printf.sprintf "%s|%d|%s\n" (gettimestring ()) (Unix.getpid ()) s :: !debug_log) fmt

let write_log () =
  List.iter (fun l -> warn "%s" l) (List.rev !debug_log);
  reset ()

