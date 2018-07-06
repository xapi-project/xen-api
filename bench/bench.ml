open Printf

let data =
  let l = ref [] in
  try
    while true do
      l := input_line stdin :: !l
    done;
    assert false
  with End_of_file -> String.concat "\n" (List.rev !l)

let yojson_data = Yojson.Safe.from_string data
let jsonwheel_data = Json_io.json_of_string data

let n = 10_000

let yojson_rd_loop () =
  for i = 1 to n do
    ignore (Yojson.Safe.from_string data)
  done

let yojson_wr_loop () =
  for i = 1 to n do
    ignore (Yojson.Safe.to_string yojson_data)
  done

let jsonwheel_rd_loop () =
  for i = 1 to n do
    ignore (Json_io.json_of_string data)
  done

let jsonwheel_wr_loop () =
  for i = 1 to n do
    ignore (Json_io.string_of_json ~compact:true jsonwheel_data)
  done

let time msg f =
  let t1 = Unix.gettimeofday () in
  f ();
  let t2 = Unix.gettimeofday () in
  printf "%s: %.3f\n%!" msg (t2 -. t1)

let () =
  time "rd yojson" yojson_rd_loop;
  time "rd json-wheel" jsonwheel_rd_loop;
  time "rd yojson" yojson_rd_loop;
  time "rd json-wheel" jsonwheel_rd_loop;

  time "wr yojson" yojson_wr_loop;
  time "wr json-wheel" jsonwheel_wr_loop;
  time "wr yojson" yojson_wr_loop;
  time "wr json-wheel" jsonwheel_wr_loop
  
