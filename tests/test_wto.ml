open Graph

(*
  The non-reducible example from Bourdoncle paper
  Reference is in WeakTopological source
*)

(* Execution should print something like (i and i' can be switched) :
   (1 4 1' 4') 2' 3' 2 3 (6 5' 6' 5) *)

module Vertex = struct
  type t = string
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module G = Persistent.Digraph.Concrete (Vertex)
module Wto = WeakTopological.Make (G)

let rec print_element = function
  | WeakTopological.Vertex v -> print_string v
  | WeakTopological.Component (head, components) ->
    (* Printf is for the weak *)
    print_string "(";
    print_string head;
    print_string " ";
    print_components components;
    print_string ")"

and print_components components =
  WeakTopological.fold_left
    (fun () elem -> print_element elem; print_string " ")
    ()
    components

let edges = [
  "1", "4";
  "1", "2";
  "2", "3";
  "3", "6";
  "4", "1'";
  "5", "6";
  "6", "5'";
  "1'", "2'";
  "1'", "4'";
  "2'", "3'";
  "3'", "6'";
  "4'", "1";
  "5'", "6'";
  "6'", "5";
]

let g =
  List.fold_left
    (fun acc (v, w) -> G.add_edge acc v w)
    G.empty edges

let result = Wto.recursive_scc g "1"

let () =
  print_components result
