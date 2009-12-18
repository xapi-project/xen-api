let log_path = "/var/log/fe.log"

let dbuffer = ref (Buffer.create 1) 

let gettimestring () =
	let time = Unix.gettimeofday () in
	let tm = Unix.gmtime time in
        let msec = time -. (floor time) in
	Printf.sprintf "%d%.2d%.2dT%.2d:%.2d:%.2d.%.3dZ|" (1900 + tm.Unix.tm_year)
	        (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
	        tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
	        (int_of_float (1000.0 *. msec))

let reset () = dbuffer := Buffer.create 100

let debug (fmt : ('a, unit, string, unit) format4) = 
  Printf.kprintf (fun s -> ignore(Printf.bprintf !dbuffer "%s|%d|%s\n" (gettimestring ()) (Unix.getpid ()) s)) fmt

let write_log () =
  let logfile = Unix.openfile log_path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644 in
  Unixext.really_write_string logfile (Buffer.contents !dbuffer);
  Unix.close logfile

