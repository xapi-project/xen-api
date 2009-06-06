(* Simple generation count implementation *)
module D = Debug.Debugger(struct let name = "sql" end)
open D

type t = int64

let of_string str : t = Int64.of_string str
let to_string g = Int64.to_string g
let add_int a b = Int64.add a (Int64.of_int b)
let null_generation = -1L

(* the current generation (initialised on population) is stored here *)
let current_generation : t ref = ref 0L
let set_count g =
  Db_lock.with_lock
    (fun ()->
       debug "Set generation count to %Ld" g;
       current_generation := g)

let gen_count_file dbconn = dbconn.Parse_db_conf.path^".generation"

let read_generation() = Db_lock.with_lock (fun () -> !current_generation)

let create_with_contents dbconn contents =
  Db_lock.with_lock
    (fun () ->
       Unixext.write_string_to_file (gen_count_file dbconn) (Int64.to_string contents)
    )

(* Create a fresh generation count file *)
let create_fresh dbconn = create_with_contents dbconn 0L
let create_current dbconn = Db_lock.with_lock (fun ()->create_with_contents dbconn !current_generation)

(* Increment generation count *)
let increment() =
  Db_lock.with_lock (fun ()->current_generation := Int64.add !current_generation 1L)

(* Write out the cache's current generation count to the specified db connection *)
let write_out dbconn =
  Db_lock.with_lock
    (fun () ->
       try
	 let gencount_fname = gen_count_file dbconn in
	 debug "Writing generation count %Ld to file %s" !current_generation gencount_fname;
	 (* write new generation count atomically *)
	 Unixext.write_string_to_file gencount_fname (Int64.to_string !current_generation)
       with e ->
	 debug "Exception incrementing generation count: %s" (Printexc.to_string e);
	 log_backtrace();
	 raise e
    )

let write_out_specified_count dbconn count =
  Db_lock.with_lock
    (fun () ->
       try
	 let gencount_fname = gen_count_file dbconn in
	 debug "Writing specified generation count %Ld to file %s" count gencount_fname;
	 (* write new generation count atomically *)
	 Unixext.write_string_to_file gencount_fname (Int64.to_string count)
       with e ->
	 debug "Exception incrementing generation count: %s" (Printexc.to_string e);
	 log_backtrace();
	 raise e
    )

let read dbconn =
  let gencount_fname = gen_count_file dbconn in
  try Int64.of_string (Unixext.read_whole_file_to_string gencount_fname) with _ -> 0L
