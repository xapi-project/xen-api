type t = [ `foo | `bar of int * string ] [@@deriving rpc]

let run () =
  let t1 = `foo in
  let t2 = `bar (3, "bar") in

  let r1 = rpc_of_t t1 in
  let r2 = rpc_of_t t2 in

  Printf.printf "r1 = %s\nr2 = %s\n%!" (Rpc.to_string r1) (Rpc.to_string r2);

  let t1' = t_of_rpc r1 in
  let t2' = t_of_rpc r2 in

  Alcotest.check (Testable.from_rpc_of_t rpc_of_t) "t1 =t1'" t1 t1';
  Alcotest.check (Testable.from_rpc_of_t rpc_of_t) "t2 =t2'" t2 t2';

  let test3 = Rpc.String "FOO" in
  ignore(t_of_rpc test3);
  Printf.printf "Case insensitive test: OK\n!"

let tests =
  [ "test", `Quick, run ]
