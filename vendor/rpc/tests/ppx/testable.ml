
(** Creates a [testable] from the given pretty-printer using the polymorphic
    equality function *)
let from_to_string pp =
  Alcotest.testable (Fmt.of_to_string pp) (=)

(** Creates a [testable] using OCaml's polymorphic equality and [Rpc.t] ->
    [string] conversion for formatting *)
let from_rpc_of_t rpc_of =
  from_to_string (fun t -> (rpc_of t) |> Rpc.to_string)

(** Creates a [testable] using OCaml's polymorphic equality and [Rpc.t] ->
    [string] conversion for formatting *)
let from_rpcty ty = from_rpc_of_t (Rpcmarshal.marshal ty)

let generic () = from_to_string (fun _ -> "no pretty-printer")

let rpc = from_to_string Rpc.to_string

let unmarshal_err : Rpcmarshal.err Alcotest.testable = from_to_string (function `Msg x -> x)

(** Testable for the results returned by Rpcmarshal.unmarshal *)
let unmarshal_res ty = Alcotest.result (from_rpcty ty) unmarshal_err

(** float testable using machine epsilon for the precision *)
let float = Alcotest.float 2E-52
