open Graph

(*
   Analysis with ChaoticIteration of the following program :
   X:=0;
   while X<40 do
     X:=X+1
   done

   The analyses uses the interval abstract domain, with widening, and
   the widening delay given as a parameter in the commande line (0 if
   no parameter is given).

   Executing this program should print:

   W = WTO, delay=40:
   1 -> [-∞; +∞]
   2 -> [0; +∞]
   3 -> [0; 39]
   4 -> [40; +∞]

   W = {3}, delay=39:
   1 -> [-∞; +∞]
   2 -> [0; +∞]
   3 -> [0; +∞]
   4 -> [40; +∞]

   W = WTO, delay=41:
   1 -> [-∞; +∞]
   2 -> [0; 40]
   3 -> [0; 39]
   4 -> [40; 40]

*)

(* Trivial language, only one variable *)

module Operator = struct
  type expr =
    | Var
    | Int of int
    | Sum of expr * expr

  type test =
    | Le
    | Ge

  type t =
    | Affect of expr
    | Test of test * int

  let compare = compare

  let default = Affect Var
end

open Operator

(* Basic interval domain *)

module Interval = struct

  type num =
    | MinusInfinity
    | Int of int
    | PlusInfinity

  let print_num = function
    | MinusInfinity -> print_string "-∞"
    | Int n -> print_int n
    | PlusInfinity -> print_string "+∞"

  let ( <% ) n1 n2 = match n1, n2 with
    | MinusInfinity, MinusInfinity
    | PlusInfinity, PlusInfinity -> failwith "<%"
    | MinusInfinity, _ -> true
    | _, PlusInfinity -> true
    | Int a, Int b -> a < b
    | _, _ -> false

  let ( >=% ) n1 n2 =
    not (n1 <% n2)

  let ( <=% ) n1 n2 =
    n1 <% n2 || n1 = n2

  let ( >% ) n1 n2 =
    n1 >=% n2 && n1 <> n2

  let min_ n1 n2 =
    if n1 <=% n2 then n1 else n2

  let max_ n1 n2 =
    if n1 >=% n2 then n1 else n2

  type t =
    | Bottom
    | Bounded of num * num

  let top =
    Bounded (MinusInfinity, PlusInfinity)

  let equal = ( = )

  let print = function
    | Bottom -> print_string "⊥"
    | Bounded (a, b) ->
      (* Printf is for the weak *)
      print_string "[";
      print_num a;
      print_string "; ";
      print_num b;
      print_string "]"

  let join i1 i2 = match i1, i2 with
    | Bottom, _ -> i2
    | _, Bottom -> i1
    | Bounded (a, b), Bounded (c, d) -> Bounded (min_ a c, max_ b d)

  let singleton n =
    Bounded (Int n, Int n)

  let ( +% ) x y = match x, y with
    | MinusInfinity, PlusInfinity
    | PlusInfinity, MinusInfinity -> failwith "+%"
    | MinusInfinity, _
    | _, MinusInfinity -> MinusInfinity
    | PlusInfinity, _
    | _, PlusInfinity -> PlusInfinity
    | Int a, Int b -> Int (a + b)

  let rec abstr_expr interval = function
    | Var -> interval
    | Int n -> singleton n
    | Sum (e1, e2) ->
      match abstr_expr interval e1, abstr_expr interval e2 with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | Bounded (a, b), Bounded (c, d) -> Bounded (a +% c, b +% d)

  let abstr_test interval test c = match interval with
    | Bottom -> Bottom
    | Bounded (a, b) ->
      match test with
      | Le -> if a >% c then Bottom else Bounded (a, min_ b c)
      | Ge -> if b <% c then Bottom else Bounded (max_ a c, b)

  let analyze (v, op, w) interval = match op with
    | Affect e -> abstr_expr interval e
    | Test (test, n) -> abstr_test interval test (Int n)

  let widening i1 i2 = match i1, i2 with
    | Bottom, _ -> i2
    | Bounded _, Bottom -> failwith "widening"
    | Bounded (a, b), Bounded (c, d) ->
      Bounded (
        (if a <=% c then a else MinusInfinity),
        (if b >=% d then b else PlusInfinity)
      )

end

module Int = struct
  type t = int
  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Data = struct
  include Interval
  type edge = int * Operator.t * int
end

module G = Persistent.Digraph.ConcreteLabeled (Int) (Operator)
module Wto = WeakTopological.Make (G)
module Chaotic = ChaoticIteration.Make (G) (Data)

let edges = [
  1, 2, Affect (Int 0);
  2, 3, Test (Le, 39);
  3, 2, Affect (Sum (Var, Int 1));
  2, 4, Test (Ge, 40);
]

let g =
  List.fold_left
    (fun acc (v, w, op) -> G.add_edge_e acc (G.E.create v op w))
    G.empty edges

let strategy = Wto.recursive_scc g 1

let print_vertex_data vertex interval =
  print_int vertex;
  print_string " -> ";
  Interval.print interval;
  print_newline ()

let init v =
  if v = 1 then Interval.top else Interval.Bottom

let widening_delay1 = 40
let widening_set1 = ChaoticIteration.FromWto

let widening_delay2 = 39
let widening_set2 = ChaoticIteration.Predicate (( = ) 3)

let widening_delay3 = 41
let widening_set3 = ChaoticIteration.FromWto

let result1 = Chaotic.recurse g strategy init widening_set1 widening_delay1
let result2 = Chaotic.recurse g strategy init widening_set2 widening_delay2
let result3 = Chaotic.recurse g strategy init widening_set3 widening_delay3

let () =
  print_endline "W = WTO, delay=40:";
  Chaotic.M.iter print_vertex_data result1;
  print_newline ();

  print_endline "W = {3}, delay=39:";
  Chaotic.M.iter print_vertex_data result2;
  print_newline ();

  print_endline "W = WTO, delay=41:";
  Chaotic.M.iter print_vertex_data result3;
  print_newline ();
