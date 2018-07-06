open! Import

(* Invariants:

   - [Append (x, y)] must have both [x] and [y] non-empty (complexity analysis
     of [to_string] relies on it).
   - Overall length is less than [String.max_length] (so [to_string] can work, at least in
     principle). *)
type tree =
  | Base of string
  | Append of tree * tree

type t = { len : int; tree : tree }

let of_string s = { len = String.length s; tree = Base s }

let empty = of_string ""

let length t = t.len
let is_empty t = length t = 0

let%test _ = is_empty empty
let%test _ = not (is_empty (of_string "non-empty"))

let to_string { len; tree } =
  match tree with
  | Base s -> s
  | Append (s1, s2) ->
    let buf = Bytes.create len in
    (* [todo] avoids stack overflow (some usage patterns result in highly unbalanced
       trees, so the naive recursive approach doesn't work) *)
    let rec go todo start = function
      | Base s ->
        Bytes.From_string.blit ~src:s ~src_pos:0 ~dst:buf ~dst_pos:start ~len:(String.length s);
        let start = start + String.length s in
        (match todo with
         | [] -> assert (start = len)
         | x :: xs ->
           go xs start x)
      | Append (s1, s2) ->
        go (s2 :: todo) start s1
    in
    go [s2] 0 s1;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
;;

(* We could imagine loosening the [String.max_length] length restriction if we gave
   another way of getting data out of [Rope]s other than [to_string]. However, we may then
   want to consider swapping it with [Int.max_length] to avoid potential overflow. *)
let (^) a b =
  if is_empty a then b else if is_empty b then a
  else if String.max_length - a.len < b.len
  then Error.raise_s [%message
         "Rope.(a ^ b) would be longer than String.max_length"
           (length a : int) (length b : int) (String.max_length : int)]
  else { len = a.len + b.len; tree = Append (a.tree, b.tree) }

(* Note that it's possible to hit the length restriction while only using logarithmic
   amounts of memory. *)
let%test "length overflow" =
  let x = of_string "x" in
  Exn.does_raise (fun () -> Fn.apply_n_times ~n:Int.num_bits (fun x -> x ^ x) x)

let concat ?(sep=empty) ts =
  List.reduce ts ~f:(fun x y -> x ^ sep ^ y)
  |> Option.value ~default:empty

let%test_unit _ =
  [%test_result: string] ~expect:""
    (concat ~sep:(of_string ", ") []
     |> to_string);
  [%test_result: string] ~expect:"one, two, three"
    (concat ~sep:(of_string ", ") [of_string "one"; of_string "two"; of_string "three"]
     |> to_string)

let concat_array ?(sep=empty) ts =
  Array.reduce ts ~f:(fun x y -> x ^ sep ^ y)
  |> Option.value ~default:empty

let rec add_to_buffer_internal buffer todo = function
  | Append (s1, s2) -> add_to_buffer_internal buffer (s2::todo) s1
  | Base s ->
    Buffer.add_string buffer s;
    begin match todo with
    | [] -> ()
    | x :: xs -> add_to_buffer_internal buffer xs x
    end
;;

let add_to_buffer { len = _; tree } buffer = add_to_buffer_internal buffer [] tree

let%test_unit _ =
  let r = (of_string "abc" ^ of_string "def") ^ (of_string "ghi" ^ of_string "jkl") in
  let buffer = Buffer.create 12 in
  add_to_buffer r buffer;
  [%test_result: String.t] ~expect:"abcdefghijkl" (Buffer.contents buffer)
;;

let%test_unit "no stack overflow" =
  [%test_result: string]
    (to_string (
       List.fold_left ~init:(of_string "") ~f:(^) (
         List.init 1000000 ~f:(fun _x -> of_string "x"))))
    ~expect:(String.make 1000000 'x')
;;

let%test_unit _ = [%test_result: string] (to_string (of_string "")) ~expect:""
let%test_unit _ = [%test_result: string] (to_string (of_string "x")) ~expect:"x"
let%test_unit _ = [%test_result: string]
                    (to_string (of_string "ab" ^ of_string "cd" ^ of_string "efg"))
                    ~expect:"abcdefg"
let%test_unit _ =
  let rec go = function
    | 0 -> of_string "0"
    | n -> go (n - 1) ^ of_string (string_of_int n) ^ go (n - 1)
  in
  [%test_result: string]
    (to_string (go 4))
    ~expect:"0102010301020104010201030102010"
;;
