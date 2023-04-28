open Xapi_blobstore_core

let is_small s = String.length s < 6

(** [truncated_str s] prints first 6 characters of [s], escaped if necessary.
      Longer strings are truncated with an ellipsis and full length printed.

      This is useful to get a quick overview of the input without making test results unreadable in quickcheck.
  *)
let truncated_str s =
  if String.length s < 6 then
    String.escaped s
  else
    Printf.sprintf "%s…(len:%d)"
      (String.sub s 0 6 |> String.escaped)
      (String.length s)

let all_bytes = String.init 256 Char.chr

(** [repeat n] repeats [all_bytes] string [n] times.
      Useful to exercise bugs related to char values (e.g. \0 or non-ASCII, invalid UTF-8, etc.),
      and bugs related to string length (e.g. reaching Key or Value limits).
  *)
let repeat n = List.init n (fun _ -> all_bytes) |> String.concat ""

(** [shrink_string_length s] is a QCheck shrinker that iterates on substrings of [s].
      The substrings of [s] are a depth-limited binary search to find a smaller counter-example.
      (it reuses {!val:QCheck.Shrink.int} which already does this depth limited binary search)
  *)
let shrink_string_length str =
  let open QCheck in
  str |> String.length |> Shrink.int |> QCheck.Iter.map (String.sub str 0)

(** [shrink_all-chars str] shrinks [str] by pretending all characters are 'a'.

  In this case shrinking refers to something more humanly readable: the length stays the same.
  Shrinking each char individually would yield too many possibilities
  on long strings if shrinking doesn't reproduce the bug anymore.
  Instead shrink all chars at once, with a single result.
  It will eventually converge on 'aa...a' anyway in our case.
*)
let shrink_all_chars str yield =
  let next = String.make (String.length str) 'a' in
  (* avoid infinite iteration: stop when target reached *)
  if not (String.equal next str) then
    yield next

(** [shrink_string str] shrinks a string by first attempting to shrink its length and then its contents,
      but more efficiently than {!val:QCheck.Shrink.string} would do which starts by always converting the string to a list of characters.
       *)
let shrink_string str yield =
  shrink_string_length str yield ;
  shrink_all_chars str yield

(** [bounded_string_arb n] is a {!type:QCheck.arbitary} for strings of maximum length [n].

    This can be used to generate inputs for testcases and shrink counter-examples.
*)
let bounded_string_arb n =
  (* QCheck.Shrink.string is very slow: first converts to list of chars.
      In our case bugs come from 3 sources:
       * the byte values (e.g. ASCII vs non-ascii)
       * the length of the string (whether it hits various limits or not)
       * the uniqueness of the string (e.g. duplicate inserts)

     Generate a small fully random string of fixed size, and concatenate it with a random length substring of a static string.
     Shrinking will take care of producing smaller and more readable results as needed,
     even below the fixed size
  *)
  assert (n > 0) ;

  let n = n - 4 in
  let long = ref [] in
  let () =
    if n > 0 then
      (* pregenerate all the long strings by running the shrinker on a static string *)
      let max_repetitions = n / String.length all_bytes in
      let max_str =
        repeat max_repetitions
        ^ String.sub all_bytes 0 (n mod String.length all_bytes)
      in
      shrink_string_length max_str (fun s -> long := s :: !long)
  in
  (* the generator will need to index into the static "list" many times,
     so using an array is more efficient here, we're not using array for its mutability.
  *)
  let gen_long = QCheck.Gen.oneofa @@ Array.of_list !long in
  let gen_string =
    let open QCheck.Gen in
    let* small = string_size @@ return 4 in
    let+ long = gen_long in
    small ^ long
  in
  QCheck.(
    make ~print:truncated_str ~small:String.length ~shrink:shrink_string
      gen_string
  )

module type Config = sig
  type config

  val make_test_config : unit -> config
end

module MakeSTM (KV : Types.KVDirect) (C : Config with type config = KV.config) :
  STM.Spec = struct
  let key_to_string' v = v |> KV.Key.to_string |> truncated_str

  let value_to_string' v = v |> KV.Value.to_string |> truncated_str

  let value_equal v1 v2 =
    String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)

  open STM

  type _ ty += Key : KV.Key.t ty | Value : KV.Value.t ty

  let value = (Value, value_to_string')

  let key = (Key, key_to_string')

  type sut = KV.t

  let init_sut () = KV.connect @@ C.make_test_config ()

  let cleanup = KV.disconnect

  type cmd =
    | Get of KV.Key.t
    | Put of KV.Key.t * KV.Value.t
    | Delete of KV.Key.t
    | List

  let show_cmd = function
    | Get k ->
        Printf.sprintf "GET(%S)" @@ key_to_string' k
    | Put (k, v) ->
        Printf.sprintf "PUT(%S, %S)" (key_to_string' k) (value_to_string' v)
    | Delete k ->
        Printf.sprintf "DELETE(%S)" (key_to_string' k)
    | List ->
        "LIST"

  let run cmd sut =
    match cmd with
    | Get g ->
        Res (option value, KV.get sut g)
    | Put (k, v) ->
        Res (result unit exn, protect (KV.put sut k) v)
    | Delete k ->
        Res (unit, KV.delete sut k)
    | List ->
        Res (list key, KV.list sut)

  (* pure model *)

  module KeyMap = Map.Make (struct
    type t = KV.Key.t

    let compare k1 k2 =
      String.compare (KV.Key.to_string k1) (KV.Key.to_string k2)
  end)

  let kv_length k v =
    let open KV in
    String.length (Key.to_string k) + String.length (Value.to_string v)

  (* TODO: test this on its own that it matches the invariant *)
  module SizedMap = struct
    type t = {map: KV.Value.t KeyMap.t; size: int}

    let empty = {map= KeyMap.empty; size= 0}

    let remove k t =
      match KeyMap.find_opt k t.map with
      | None ->
          t
      | Some old ->
          {map= KeyMap.remove k t.map; size= t.size - kv_length k old}

    let add k v t =
      let t = remove k t in
      {map= KeyMap.add k v t.map; size= t.size + kv_length k v}

    let find_opt k t = KeyMap.find_opt k t.map

    let mem k t = KeyMap.mem k t.map

    let is_empty t = KeyMap.is_empty t.map

    let size t = t.size

    let cardinal t = KeyMap.cardinal t.map

    let to_seq t = KeyMap.to_seq t.map
  end

  type state = int * SizedMap.t

  let () =
    (* the backend must be able to store something,
       this implements the 2 'ensures' constraints from Types.mli *)
    assert (KV.max_data_size > 0) ;
    assert (KV.max_key_count > 0)

  let invariant (_, t) =
    (* implements the invariants on type t *)
    SizedMap.cardinal t <= KV.max_key_count
    && SizedMap.size t <= KV.max_data_size

  let init_state = (0, SizedMap.empty)

  let next_state cmd (i, state) =
    ( i + 1
    , match cmd with
      | Get _ | List ->
          state (* queries do not change state *)
      | Put (k, v) ->
          SizedMap.add k v state
      | Delete k ->
          SizedMap.remove k state
    )

  let precond _cmd (i, _state) =
    i <= 8 (* small number of commands due to potential exponential blowup *)

  let postcond cmd ((i, state) : state) res =
    match (cmd, res) with
    | Get k, Res ((Option Value, _), v) ->
        Option.equal value_equal v (SizedMap.find_opt k state)
    | (Put (_, _) as cmd), Res ((Result (Unit, Exn), _), Ok ()) ->
        (* postcond gets the previous state passed in *)
        invariant (next_state cmd (i, state))
    | (Delete _ as cmd), Res ((Unit, _), ()) ->
        (* postcond gets the previous state passed in *)
        invariant (next_state cmd (i, state))
    | ( (Put (_, _) as cmd)
      , Res ((Result (Unit, Exn), _), Error (Invalid_argument _)) ) ->
        not @@ invariant (next_state cmd (i, state))
    | List, Res ((List Key, _), l) ->
        List.length l = SizedMap.cardinal state
        && List.for_all (fun k -> SizedMap.mem k state) l
    | _ ->
        false

  let key_arb =
    QCheck.map ~rev:KV.Key.to_string KV.Key.of_string_exn
    @@ bounded_string_arb KV.Key.max_length

  let shrink_key = key_arb.QCheck.shrink |> Option.get

  let val_arb =
    QCheck.map ~rev:KV.Value.to_string KV.Value.of_string_exn
    @@ bounded_string_arb KV.Value.max_length

  let shrink_val = val_arb.QCheck.shrink |> Option.get

  let shrink_cmd =
    let open QCheck in
    function
    | Get k ->
        Iter.map (fun k -> Get k) @@ shrink_key k
    | Put (k, v) ->
        Iter.map2 (fun k v -> Put (k, v)) (shrink_key k) (shrink_val v)
    | Delete k ->
        Iter.map (fun k -> Delete k) (shrink_key k)
    | List ->
        Iter.empty

  let small_cmd =
    let open KV in
    function
    | Get k | Delete k ->
        String.length (Key.to_string k)
    | Put (k, v) ->
        String.length (Key.to_string k) + String.length (Value.to_string v)
    | List ->
        1

  let arb_cmd (_, state) =
    let open QCheck in
    let gen_new_key = gen key_arb in
    let value = gen val_arb in
    let key =
      if SizedMap.is_empty state then
        gen_new_key
      else (* TODO: cache for faster access *)
        let keys = state |> SizedMap.to_seq |> Seq.map fst |> Array.of_seq in
        (* generate brand new keys or keys from the existing map:
           this ensures that delete actually exercises the code that deletes an existing key,
           with random generation there would be a very low chance of that otherwise
        *)
        Gen.(oneof [oneofa keys; gen_new_key])
    in
    make ~print:show_cmd ~shrink:shrink_cmd ~small:small_cmd
    @@ Gen.oneof
         [
           Gen.return List
         ; Gen.map (fun k -> Get k) key
         ; Gen.map (fun k -> Delete k) key
         ; Gen.map2 (fun k v -> Put (k, v)) key value
         ]
end

module MakeLin (KV : Types.KVDirect) (C : Config with type config = KV.config) :
  Lin.Spec = struct
  open Lin

  type t = KV.t

  let init () = KV.connect @@ C.make_test_config ()

  let cleanup = KV.disconnect

  let key_to_string' v = v |> KV.Key.to_string |> truncated_str

  let value_to_string' v = v |> KV.Value.to_string |> truncated_str

  let value_equal v1 v2 =
    String.equal (KV.Value.to_string v1) (KV.Value.to_string v2)

  let key_equal v1 v2 = String.equal (KV.Key.to_string v1) (KV.Key.to_string v2)

  let api =
    let key_arb =
      QCheck.map ~rev:KV.Key.to_string KV.Key.of_string_exn
      @@ bounded_string_arb KV.Key.max_length
    in
    let value_arb =
      QCheck.map ~rev:KV.Value.to_string KV.Value.of_string_exn
      @@ bounded_string_arb KV.Value.max_length
    in
    (* needs to be (), otherwise the constructible/deconstructible type parameter will be weak,
       instead of forall 'a and won't work both as parameter and return value *)
    let key () = Lin.gen_deconstructible key_arb key_to_string' key_equal in
    let value () =
      Lin.gen_deconstructible value_arb value_to_string' value_equal
    in
    [
      val_ "GET" KV.get (t @-> key () @-> returning @@ option @@ value ())
    ; val_ "PUT" KV.put (t @-> key () @-> value () @-> returning_or_exc unit)
    ; val_ "DELETE" KV.delete (t @-> key () @-> returning unit)
    ; val_ "LIST" KV.list (t @-> returning @@ list @@ key ())
    ]
end

(* TODO: make the tests one below take an io monad? *)
(*
module MakeDirect
    (KV : Types.S with type 'a io = 'a Lwt.t and type config = unit) :
  KVDirect with type t = KV.t = struct
  type t = KV.t

  type 'a io = 'a

  type config = KV.config

  let max_key_count = KV.max_key_count

  let max_data_size = KV.max_data_size

  module Key = KV.Key
  module Value = KV.Value

  let name = KV.name ^ " (run_in_main)"

  let lwt f x = Lwt_preemptive.run_in_main (fun () -> f x)

  let lwt2 f x y = Lwt_preemptive.run_in_main (fun () -> f x y)

  let lwt3 f x y z = Lwt_preemptive.run_in_main (fun () -> f x y z)

  let connect = lwt KV.connect

  let disconnect = lwt KV.disconnect

  let get = lwt2 KV.get

  let put = lwt3 KV.put

  let delete = lwt2 KV.delete

  let list = lwt KV.list
end
*)

let tests (type config) ~count
    ((module KV : Types.KVDirect with type config = config), make_test_config) =
  let module Config = struct
    type config = KV.config

    let make_test_config = make_test_config
  end in
  let module SpecSTM = MakeSTM (KV) (Config) in
  let module KV_seq = STM_sequential.Make (SpecSTM) in
  let module SpecLin = MakeLin (KV) (Config) in
  let module Conc = Spec_concurrent.Make (SpecSTM) (SpecLin) in
  ( KV.name
  , (* KV_seq.agree_test ~count ~name:(KV.name ^ " STM sequential") :: *)
    Conc.tests ~count ~name:KV.name
  )
