open! Core_kernel
open! Import
open! Unpack_buffer

let is_dead t = Result.is_error (is_empty t)

let is_empty_exn t = ok_exn (is_empty t)

let unpack t =
  let q = Queue.create () in
  match unpack_into t q with
  | Ok () -> Ok q
  | Error _ as err ->
    (* If we *have* unpacked values, we first want to return them,
       and then on the next call we will return the error (because [t.state = Dead]) *)
    assert (is_dead t);
    if Queue.is_empty q then err else Ok q
;;

module type Value = sig
  type t [@@deriving sexp_of]
  include Equal.S with type t := t

  val pack : t list -> string
  val unpack_one : t Unpack_one.t
end

let test (type value) (module V : Value with type t = value) values =
  let input = Bigstring.of_string (V.pack values) in
  let input_size = Bigstring.length input in
  let chunk_sizes =
    List.filter ~f:(fun n -> 1 <= n && n <= input_size)
      [ 1
      ; 2
      ; 13
      ; input_size / 4
      ; input_size / 2 - 1
      ; input_size / 2
      ; input_size / 2 + 1
      ; input_size * 3  / 4
      ; input_size - 1
      ; input_size
      ]
  in
  List.iter chunk_sizes ~f:(fun chunk_size ->
    let t = create V.unpack_one in
    try
      assert (is_empty_exn t);
      let output = Queue.create () in
      let rec loop pos =
        if pos < input_size then begin
          let len = min chunk_size (input_size - pos) in
          assert (feed t input ~pos ~len = Ok ());
          assert (not (is_empty_exn t));
          let unpack_result = ok_exn (unpack t) in
          Queue.blit_transfer ~src:unpack_result ~dst:output ();
          loop (pos + len);
        end
      in
      loop 0;
      assert (is_empty_exn t);
      let output = Queue.to_list output in
      if not (List.equal ~equal:V.equal values output) then
        failwiths "mismatch" (values, output) [%sexp_of: V.t list * V.t list];
    with exn ->
      failwiths "failure"
        (exn, `chunk_size chunk_size, `input input, values, t)
        [%sexp_of: (exn
                    * [ `chunk_size of int ]
                    * [ `input of Bigstring.t ]
                    * V.t list
                    * V.t t)]);
;;

let%test_unit _ =
  debug := true;
  for value_size = 1 to 5 do
    let module Value = struct
      let pack ts = String.concat ts
      let unpack_one =
        Unpack_one.create
          (fun ?partial_unpack:_ buf ~pos ~len ->
             if len < value_size then
               `Not_enough_data ((), 0)
             else
               let bytes = Bytes.create value_size in
               Bigstring.To_bytes.blito ~src:buf ~src_pos:pos ~src_len:value_size
                 ~dst:bytes ();
               `Ok (
                 Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes,
                 value_size
               ))
      include String
    end in
    let values =
      List.init 10 ~f:(fun i ->
        String.init value_size ~f:(fun j ->
          Char.of_int_exn ((i * value_size + j) land 0xFF)))
    in
    test (module Value) values
  done
;;

(* [Unpack_one.sexp] *)
let%test_unit _ =
  let module Value = struct
    let pack ts = String.concat ~sep:" " (List.map ts ~f:Sexp.to_string)
    let unpack_one = Unpack_one.sexp
    include Sexp
  end in
  let sexps =
    Sexp.(
      let e = Atom "" in
      let a = Atom "a" in
      let abc = Atom "abc" in
      [ e; a; abc
      ; List []
      ; List [ a ]
      ; List [ e; a ]
      ; List [ List [] ]
      ; List [ List []
             ; List [ a ]
             ; List [ a; abc ]
             ]])
  in
  let test sexps = test (module Value) sexps in
  let terminator = Sexp.List [] in (* used to ensure unparsing succeeds *)
  List.iter sexps ~f:(fun sexp ->
    test [ sexp; terminator ];
    test [ sexp; sexp; terminator ]);
  test sexps
;;

(* [Unpack_one.sexp] *)
let%test_unit _ =
  debug := true;
  (* Error case. *)
  begin
    let Unpack_one.T unpack_sexp = Unpack_one.sexp in
    match unpack_sexp ~pos:0 ~len:1 (Bigstring.of_string ")") with
    | `Invalid_data _ -> ()
    | `Ok _
    | `Not_enough_data _ -> assert false
  end;
  (* Simple, case where we parse a complete sexp in one pass:
     - starts in the middle of the buffer
     - doesn't consume the whole buffer *)
  begin
    let Unpack_one.T unpack_sexp = Unpack_one.sexp in
    match unpack_sexp ~pos:1 ~len:9 (Bigstring.of_string ")(foo)(x y") with
    | `Ok (Sexp.List [Sexp.Atom "foo"], 5) -> ()
    | `Ok result ->
      Error.raise
        (Error.create "Unexpected result" result [%sexp_of: Sexp.t * int])
    | `Not_enough_data _
    | `Invalid_data _ -> assert false
  end;
  (* Partial sexp case, requries two passes to parse the sexp. *)
  begin
    let Unpack_one.T unpack_sexp = Unpack_one.sexp in
    match unpack_sexp ~pos:6 ~len:4 (Bigstring.of_string ")(foo)(x y") with
    | `Not_enough_data (k, 4) ->
      begin
        match
          unpack_sexp ~partial_unpack:k ~pos:0 ~len:3 (Bigstring.of_string " z)")
        with
        | `Ok (Sexp.List [Sexp.Atom "x"; Sexp.Atom "y"; Sexp.Atom "z"], 3) -> ()
        | `Ok _
        | `Not_enough_data _
        | `Invalid_data _ -> assert false
      end
    | `Not_enough_data (_, n) -> failwithf "Consumed %d bytes" n ()
    | `Ok result ->
      Error.raise
        (Error.create "Unexpected result" result [%sexp_of: Sexp.t * int])
    | `Invalid_data error -> Error.raise error
  end
;;

(* [Unpack_one.ch] *)
let%test_unit _ =
  debug := true;
  let succeeded_correctly = function
    | `Ok ((), 1) -> true
    | `Ok _ | `Not_enough_data _ | `Invalid_data _ -> false
  in
  let failed_correctly = function
    | `Invalid_data _ -> true
    | `Ok _ | `Not_enough_data _ -> false
  in
  let open Unpack_one in
  let T expect_a = expect_char 'a' in
  (* basic *)
  assert (succeeded_correctly (expect_a ~pos:0 ~len:1 (Bigstring.of_string "a")));
  assert (failed_correctly    (expect_a ~pos:0 ~len:1 (Bigstring.of_string "b")));
  (* middle of buffer *)
  assert (succeeded_correctly (expect_a ~pos:3 ~len:1 (Bigstring.of_string "bcda")));
  assert (failed_correctly    (expect_a ~pos:3 ~len:1 (Bigstring.of_string "abcd")));
  (* Need more data *)
  match expect_a ~pos:0 ~len:0 (Bigstring.of_string "") with
  | `Not_enough_data (_, 0) -> ()
  | `Not_enough_data _ | `Ok _ | `Invalid_data _ -> assert false
;;

(* [Unpack_one.bind] *)
let%test_unit _ =
  debug := true;
  let module Value = struct
    include Sexp
    let pack ts =
      List.map ts ~f:(fun sexp -> Sexp.to_string sexp ^ "\n")
      |> String.concat
    let unpack_one =
      let open Unpack_one.Monad_infix in
      Unpack_one.sexp
      >>= fun sexp ->
      Unpack_one.newline
      >>| fun () ->
      sexp
  end
  in
  test (module Value) [];
  test (module Value) [ List [] ];
  test (module Value) [ Atom "one" ];
  test (module Value) [ Atom "one"; Atom "two" ];
  test (module Value) [ Atom "one"; List [Atom "two"] ];
  test (module Value) [ List [Atom "one"] ; Atom "two" ]
;;

(* [Unpack_one.create_bin_prot] *)
let%test_unit _ =
  debug := true;
  let module Value = struct
    type t =
      { foo : bool
      ; bar : int
      ; baz : string list
      }
    [@@deriving bin_io, compare, sexp]

    let equal t t' = compare t t' = 0

    let pack ts =
      let size =
        List.fold ts ~init:0 ~f:(fun acc t ->
          acc + bin_size_t t + Bin_prot.Utils.size_header_length)
      in
      let buffer = Bigstring.create size in
      let final_pos =
        List.fold ts ~init:0 ~f:(fun pos t ->
          Bigstring.write_bin_prot buffer bin_writer_t t ~pos)
      in
      assert (final_pos = size);
      Bigstring.to_string buffer
    ;;
  end
  in
  let module Value_direct = struct
    include Value
    let unpack_one = Unpack_one.create_bin_prot bin_reader_t
  end
  in
  let module Value_blob = struct
    include Value
    let unpack_one =
      Unpack_one.bin_blob
      |> Unpack_one.map ~f:(fun thing ->
        Bin_prot.Blob.Opaque.Bigstring.of_opaque_exn thing bin_reader_t)
  end
  in
  let a = { Value. foo = true; bar = 4; baz = [ "qux"; "quux" ] } in
  let b = { Value. foo = false; bar = -3289; baz = [] } in
  let c = { Value. foo = false; bar = 0; baz = List.init 1000 ~f:(fun _i -> "spam") } in
  List.iter
    [(module Value_direct : Value with type t = Value.t);
     (module Value_blob   : Value with type t = Value.t)]
    ~f:(fun value_module ->
      test value_module [];
      test value_module [ a ];
      test value_module [ a; b; a; a; a; b; b; c; c; b; a; a; b ];
      test value_module (List.init 1000 ~f:(fun i -> if i % 2 = 0 then a else b)))
;;

let%test_unit _ = (* [unpack_iter] when [f] raises *)
  let t =
    create (Unpack_one.create (fun ?partial_unpack:_ _ ~pos:_ ~len ->
      if len = 0
      then assert false
      else `Ok ((), 1)))
  in
  ok_exn (feed_string t "hello");
  assert (is_error (unpack_iter t ~f:(fun _ -> failwith "f raised")))
;;
