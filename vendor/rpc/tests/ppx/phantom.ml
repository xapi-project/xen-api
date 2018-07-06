module P : sig
  type 'a t
  val rpc_of_t: ('a -> Rpc.t) -> 'a t -> Rpc.t
  val t_of_rpc: (Rpc.t -> 'a) -> Rpc.t -> 'a t
  val to_string: 'a t -> string
  val of_string: string -> 'a t
end = struct
  type 'a t = string [@@deriving rpc]
  let to_string x = x
  let of_string x = x
end

module Q = struct
  include P
  let rpc_of_t _ x = Rpc.rpc_of_string (to_string x)
  let t_of_rpc _ x = of_string (Rpc.string_of_rpc x)
end

type x = [`foo] Q.t [@@deriving rpc]
type y = [`bar] Q.t [@@deriving rpc]

let run () =
  let p : [`p] P.t = P.of_string "foo" in
  let q : [`q] P.t = P.of_string "foo" in
  let x : x = P.of_string "foo" in
  let y : y = P.of_string "foo" in

  let p_rpc = Q.rpc_of_t () p in
  let q_rpc = Q.rpc_of_t () q in
  let x_rpc = rpc_of_x x in
  let y_rpc = rpc_of_y y in

  let _ : [`p] P.t = Q.t_of_rpc () p_rpc in
  let _ : [`q] P.t = Q.t_of_rpc () q_rpc in
  let _ : x = x_of_rpc x_rpc in
  let _ : y = y_of_rpc y_rpc in

  Printf.printf "p=%s\n" (Xmlrpc.to_string p_rpc);
  Printf.printf "q=%s\n" (Xmlrpc.to_string q_rpc);
  Printf.printf "x=%s\n" (Xmlrpc.to_string x_rpc);
  Printf.printf "y=%s\n" (Xmlrpc.to_string y_rpc)

let tests =
  [ "test", `Quick, run ]
