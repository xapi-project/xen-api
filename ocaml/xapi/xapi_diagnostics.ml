module D = Debug.Make(struct let name="xapi_diagnostics" end)
open D

let gc_compact ~__context ~host =
  Gc.compact ()

let gc_stats ~__context ~host =
  let stat = Gc.stat () in
  ["minor_words",       string_of_float stat.Gc.minor_words;
   "promoted_words",    string_of_float stat.Gc.promoted_words;
   "major_words",       string_of_float stat.Gc.major_words;
   "minor_collections", string_of_int stat.Gc.minor_collections;
   "major_collections", string_of_int stat.Gc.major_collections;
   "heap_words",        string_of_int stat.Gc.heap_words;
   "heap_chunks",       string_of_int stat.Gc.heap_chunks;
   "live_words",        string_of_int stat.Gc.live_words;
   "live_blocks",       string_of_int stat.Gc.live_blocks;
   "free_words",        string_of_int stat.Gc.free_words;
   "free_blocks",       string_of_int stat.Gc.free_blocks;
   "largest_free",      string_of_int stat.Gc.largest_free;
   "fragments",         string_of_int stat.Gc.fragments;
   "compactions",       string_of_int stat.Gc.compactions;
   "top_heap_words",    string_of_int stat.Gc.top_heap_words;
  ]

let db_stats ~__context =
  (* Use Printf.sprintf to keep format *)
  let (n,avgtime,min,max) = Db_lock.report () in
  ["n",       Printf.sprintf "%d" n;
   "avgtime", Printf.sprintf "%f" avgtime;
   "min",     Printf.sprintf "%f" min;
   "max",     Printf.sprintf "%f" max;
  ]

let license_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "license_stats" ]))

let network_stats ~__context ~session ~host =
  raise Api_errors.(Server_error(not_implemented, [ "network_stats" ]))

