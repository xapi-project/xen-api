open! Import
open Std_internal

let debug = ref false

module Unpack_one = struct
  type ('a, 'partial_unpack) unpack_result =
    [ `Ok              of 'a * int
    | `Not_enough_data of 'partial_unpack * int
    | `Invalid_data    of Error.t
    ]

  type ('value, 'partial_unpack) unpacked
    =  ?partial_unpack : 'partial_unpack
    -> ?pos            : int
    -> ?len            : int
    -> Bigstring.t
    -> ('value, 'partial_unpack) unpack_result

  type 'a t = T : ('a, _) unpacked -> 'a t

  let create_unpacked unpack_one =
    fun ?partial_unpack ?pos ?len buf ->
      let (pos, len) =
        Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(Bigstring.length buf)
      in
      unpack_one ?partial_unpack buf ~pos ~len
  ;;

  let create unpack_one = T (create_unpacked unpack_one)

  include Monad.Make (struct

      type nonrec 'a t = 'a t

      let return v = T (fun ?partial_unpack:_ ?pos:_ ?len:_ _ -> `Ok (v, 0))

      let map' (t : 'a t) ~f =
        let T t = t in
        T (fun ?partial_unpack ?pos ?len buf ->
          match t ?partial_unpack ?pos ?len buf with
          | `Invalid_data _ | `Not_enough_data _ as x -> x
          | `Ok (a, pos) -> `Ok (f a, pos))
      ;;

      let map = `Custom map'

      let bind =
        let module Partial_unpack = struct
          type ('pa, 'b) t =
            | A : 'pa -> ('pa, _) t
            | B : 'pb * ('b, 'pb) unpacked -> (_, 'b) t
        end in
        let open Partial_unpack in
        let do_b ~na partial_unpack (ub : (_, _) unpacked) buf ~pos ~len =
          match ub ?partial_unpack ~pos ~len buf with
          | `Invalid_data _ as x -> x
          | `Not_enough_data (pb, nb) -> `Not_enough_data (B (pb, ub), nb + na)
          | `Ok (b, nb) -> `Ok (b, na + nb)
        in
        fun (T ua) ~f ->
          let do_a partial_unpack buf ~pos ~len =
            match ua ?partial_unpack ~pos ~len buf with
            | `Invalid_data _ as x -> x
            | `Not_enough_data (pa, n) -> `Not_enough_data (A pa, n)
            | `Ok (a, na) ->
              let T ub = f a in
              do_b ~na None ub buf ~pos:(pos + na) ~len:(len - na)
          in
          create (fun ?partial_unpack buf ~pos ~len ->
            match partial_unpack with
            | None              -> do_a       None         buf ~pos ~len
            | Some (A pa)       -> do_a       (Some pa)    buf ~pos ~len
            | Some (B (pb, ub)) -> do_b ~na:0 (Some pb) ub buf ~pos ~len)
      ;;
    end)

  (* [create_bin_prot] doesn't use [Bigstring.read_bin_prot] for performance reasons.  It
     was written prior to [Bigstring.read_bin_prot], and it's not clear whether switching
     to use it would cause too much of a performance hit. *)
  let create_bin_prot_unpacked bin_prot_reader ~reader_expects_size_header =
    let header_length = Bin_prot.Utils.size_header_length in
    let not_enough_data = `Not_enough_data ((), 0) in
    let pos_ref = ref 0 in
    let invalid_data message a sexp_of_a =
      `Invalid_data (Error.create message a sexp_of_a)
    in
    let read bin_reader buf ~pos ~len =
      pos_ref := pos;
      let result = bin_reader buf ~pos_ref in
      if !pos_ref <> pos + len then
        invalid_data "pos_ref <> pos + len" (!pos_ref, pos, len)
          ([%sexp_of: int * int * int])
      else
        `Ok result
    in
    create_unpacked
      (fun ?partial_unpack:_ buf ~pos ~len ->
         if header_length > len then
           not_enough_data
         else begin
           match read Bin_prot.Utils.bin_read_size_header buf ~pos ~len:header_length with
           | `Invalid_data _ as x -> x
           | `Ok element_length ->
             if element_length < 0 then
               invalid_data "negative element length %d" element_length [%sexp_of: int]
             else begin
               if element_length > len - header_length then
                 not_enough_data
               else begin
                 let pos =
                   match reader_expects_size_header with
                   | true -> pos
                   | false -> pos + header_length
                 in
                 let len =
                   match reader_expects_size_header with
                   | true -> header_length + element_length
                   | false -> element_length
                 in
                 match read bin_prot_reader.Bin_prot.Type_class.read buf ~pos ~len with
                 | `Invalid_data _ as x -> x
                 | `Ok result -> `Ok (result, header_length + element_length)
               end
             end
         end)
  ;;

  let create_bin_prot bin_prot_reader =
    T (create_bin_prot_unpacked bin_prot_reader ~reader_expects_size_header:false)
  ;;

  let bin_blob =
    T (create_bin_prot_unpacked
         Bin_prot.Blob.Opaque.Bigstring.bin_reader_t
         ~reader_expects_size_header:true)
  ;;

  let sexp =
    let module Parse_pos = Sexp.Parse_pos in
    let partial_unpack_init ~pos ~len buf =
      Sexp.parse_bigstring buf ~len ~parse_pos:(Parse_pos.create ~buf_pos:pos ())
    in
    create
      (fun ?(partial_unpack = partial_unpack_init) buf ~pos ~len ->
         try
           begin match partial_unpack ~pos ~len buf with
           | Cont (_state, k)       -> `Not_enough_data (k, len)
           | Done (sexp, parse_pos) -> `Ok (sexp, parse_pos.Parse_pos.buf_pos - pos)
           end
         with exn -> `Invalid_data (Error.of_exn exn))
  ;;

  let char =
    create (fun ?partial_unpack:_ buf ~pos ~len ->
      if len < 1 then
        `Not_enough_data ((), 0)
      else
        `Ok (Bigstring.get buf pos, 1))
  ;;

  module type Equal = sig
    type t [@@deriving sexp_of]
    val equal : t -> t -> bool
  end

  let expect (type a) (T u) (module E : Equal with type t = a) expected =
    T (fun ?partial_unpack ?pos ?len buf ->
      match u ?partial_unpack ?pos ?len buf with
      | `Invalid_data _ | `Not_enough_data _ as x -> x
      | `Ok (parsed, n) ->
        if E.equal expected parsed then
          `Ok ((), n)
        else
          `Invalid_data
            (Error.create "parsed does not match expected" () (fun () ->
               [%sexp
                 { parsed =   (parsed   : E.t)
                 ; expected = (expected : E.t)
                 }])))
  ;;

  let expect_char = expect char (module Char)

  let newline = expect_char '\n'
end

type ('a, 'b) alive =
  { mutable partial_unpack : 'b option
  ; unpack_one             : ('a, 'b) Unpack_one.unpacked sexp_opaque
  (* [buf] holds unconsumed chars*)
  ; mutable buf            : Bigstring.t
  (* [pos] is the start of unconsumed data in[buf] *)
  ; mutable pos            : int
  (* [len] is the length of unconsumed data in[buf] *)
  ; mutable len            : int
  }
[@@deriving sexp_of]

type 'a state =
  | Alive  : ('a, _) alive -> 'a state
  | Dead  of Error.t
[@@deriving sexp_of]

type 'a t =
  { mutable state : 'a state
  }
[@@deriving sexp_of]

let invariant _ t =
  try
    match t.state with
    | Dead _ -> ()
    | Alive t ->
      assert (t.pos >= 0);
      assert (t.len >= 0);
      if t.len = 0 then assert (t.pos = 0);
      assert (t.pos + t.len <= Bigstring.length t.buf);
  with exn ->
    failwiths "invariant failed" (exn, t) [%sexp_of: exn * _ t]
;;

let create_unpacked ?partial_unpack unpack_one =
  { state =
      Alive { partial_unpack
            ; unpack_one
            ; buf = Bigstring.create 1
            ; pos = 0
            ; len = 0
            };
  }
;;

let create (Unpack_one.T unpack_one) = create_unpacked unpack_one

let create_bin_prot bin_prot_reader =
  create (Unpack_one.create_bin_prot bin_prot_reader)
;;

let is_empty t =
  match t.state with
  | Dead error -> Error error
  | Alive t -> Ok (is_none t.partial_unpack && t.len = 0)
;;

let is_available t len =
  let input_start = t.pos + t.len in
  let available = Bigstring.length t.buf - input_start in
  available >= len
;;

let ensure_available t len =
  if not (is_available t len) then begin
    (* Grow the buffer, and shift the unconsumed bytes to the front. *)
    let new_buf = Bigstring.create (max (t.len + len) (2 * Bigstring.length t.buf)) in
    Bigstring.blito ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:new_buf ();
    t.pos <- 0;
    t.buf <- new_buf;
    assert (is_available t len);
  end;
;;

let feed_gen buf_length (blit_buf_to_bigstring : (_, _) Blit.blito)
      ?pos ?len t buf =
  if !debug then invariant ignore t;
  match t.state with
  | Dead e -> Error e
  | Alive t ->
    let (src_pos, src_len) =
      Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(buf_length buf)
    in
    ensure_available t src_len;
    blit_buf_to_bigstring
      ~src:buf ~src_pos ~src_len
      ~dst:t.buf ~dst_pos:(t.pos + t.len) ();
    t.len <- t.len + src_len;
    Ok ();
;;

let feed ?pos ?len t buf =
  feed_gen Bigstring.length Bigstring.blito             ?pos ?len t buf
;;

let feed_string ?pos ?len t buf =
  feed_gen    String.length Bigstring.From_string.blito ?pos ?len t buf
;;

let feed_bytes ?pos ?len t buf =
  feed_gen    Bytes.length Bigstring.From_bytes.blito ?pos ?len t buf
;;

let unpack_iter t ~f =
  if !debug then invariant ignore t;
  match t.state with
  | Dead e -> Error e
  | Alive alive ->
    let error e =
      t.state <- Dead e;
      Error e
    in
    let t = alive in
    let consume ~num_bytes =
      t.pos <- t.pos + num_bytes;
      t.len <- t.len - num_bytes;
    in
    let rec loop () =
      if t.len = 0 then begin
        t.pos <- 0;
        Ok ();
      end else begin
        match
          t.unpack_one t.buf ~pos:t.pos ~len:t.len ?partial_unpack:t.partial_unpack
        with
        | exception exn -> error (Error.create "unpack error" exn [%sexp_of: Exn.t])
        | unpack_result ->
          match unpack_result with
          | `Invalid_data e -> error (Error.tag e ~tag:"invalid data")
          | `Ok (one, num_bytes) ->
            (* In order to get a value we either need to consume some bytes or have
               partially unpacked data, otherwise it is a bug in [unpack_one].  The case
               of [num_bytes = 0] comes up when parsing sexp atoms where we don't know
               where atom ends until we hit parenthesis, e.g. "abc(". *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "unpack consumed invalid amount" num_bytes
                       [%sexp_of: int])
            else if num_bytes = 0 && Option.is_none t.partial_unpack then
              error (Error.of_string "\
unpack returned a value but consumed 0 bytes without partially unpacked data")
            else begin
              consume ~num_bytes;
              t.partial_unpack <- None;
              match f one with
              | exception exn ->
                error (Error.create "~f supplied to Unpack_buffer.unpack_iter raised" exn
                         [%sexp_of: exn])
              | _ -> loop ();
            end;
          | `Not_enough_data (partial_unpack, num_bytes) ->
            (* Partial unpacking need not have consumed any bytes, and cannot have
               consumed more bytes than were available. *)
            if num_bytes < 0 || num_bytes > t.len then
              error (Error.create "partial unpack consumed invalid amount" num_bytes
                       [%sexp_of: int])
            else begin
              consume ~num_bytes;
              t.partial_unpack <- Some partial_unpack;
              (* Put unconsumed bytes at the front.  We assume that unpacking is
                 deterministic, which ensures that every input byte is shifted at most
                 once.  Once a byte has been shifted, it will remain where it is until it
                 is consumed. *)
              if t.len > 0 then
                Bigstring.blito ~src:t.buf ~src_pos:t.pos ~src_len:t.len ~dst:t.buf ();
              t.pos <- 0;
              Ok ();
            end
      end
    in
    loop ()
;;

let unpack_into t q = unpack_iter t ~f:(Queue.enqueue q)
