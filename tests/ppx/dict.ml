type key = string [@@deriving rpc]

type t = (key * float) list [@@deriving rpc]

let run () =
  let t = [ "foo", 3. ; "bar", 4. ] in
  let r = rpc_of_t t in
  Printf.printf "r = %s\n%!" (Rpc.to_string r);

  let t' = t_of_rpc r in
  Printf.printf "t = t' : %b\n%!" (t = t');
  Alcotest.(check (list (pair string Testable.float)))
    "dict same after marshal->unmarshal"
    t t'

let tests =
  [ "main test", `Quick, run ]
