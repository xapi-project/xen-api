open Crowbar

let create x =
  match Cstruct.create x with
  | c -> assert (x >= 0); c
  | exception Invalid_argument _ -> assert (x <= 0); bad_test ()

let create_sub x start len =
  try
    let c = Cstruct.create x in
    Cstruct.sub c start len
  with Invalid_argument _ -> bad_test ()

let cstruct = Choose [
    Map ([int8], create);
    Map ([range 0x10000; int; int], create_sub);
  ]

let buffer = Map ([uint8], Bigarray.(Array1.create Char c_layout))

let pp_cstruct f c = Format.pp_print_string f (Cstruct.debug c)

let () =
  assert (Array.length Sys.argv = 2);   (* Prevent accidentally running in quickcheck mode *)
  add_test ~name:"blit" [cstruct; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      try Cstruct.blit src srcoff dst dstoff len; Ok ()
      with Invalid_argument _ -> Ok ()
    );
  add_test ~name:"sexp" [buffer] (fun b ->
      b |> Cstruct.sexp_of_buffer |> Cstruct.buffer_of_sexp
      |> check_eq
        ~cmp:(fun x y -> Cstruct.compare (Cstruct.of_bigarray x) (Cstruct.of_bigarray y))
        b
    );
  add_test ~name:"of_bigarray" [buffer; Option int; Option int] (fun b off len ->
      match Cstruct.of_bigarray b ?off ?len with
      | c -> check (Cstruct.len c <= Bigarray.Array1.dim b)
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"get_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c in
      match Cstruct.get_char c off with
      | _ -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"set_char" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c in
      match Cstruct.set_char c off 'x' with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"sub" [cstruct; int; int] (fun c off len ->
      Fmt.pr "sub %d %d\n%!" off len;
      match Cstruct.sub c off len with
      | sub -> check (Cstruct.len sub <= Cstruct.len c);
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"shift" [cstruct; int] (fun c off ->
      match Cstruct.shift c off with
      | sub -> check (Cstruct.len sub <= Cstruct.len c);
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"copy" [cstruct; int; int] (fun c off len ->
      match Cstruct.copy c off len with
      | sub -> check (String.length sub <= Cstruct.len c);
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"blit_from_bytes" [bytes; int; cstruct; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_from_bytes src srcoff dst dstoff len with
      | () -> Ok ()
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"blit_to_bytes" [cstruct; int; bytes; int; int] (fun src srcoff dst dstoff len ->
      match Cstruct.blit_to_bytes src srcoff dst dstoff len with
      | () -> Ok ()
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"memset" [cstruct; int] (fun c x ->
      Cstruct.memset c x; Ok ()
    );
  add_test ~name:"set_len" [cstruct; int] (fun c x ->
      match Cstruct.set_len c x with
      | c2 -> check (Cstruct.len c2 <= Cstruct.len c)
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"add_len" [cstruct; int] (fun c x ->
      match Cstruct.add_len c x with
      | c2 -> check (Cstruct.len c2 <= Cstruct.len c)
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"split" [cstruct; Option int; int] (fun c start len ->
      match Cstruct.split ?start c len  with
      | c1, c2 -> check (Cstruct.len c2 <= Cstruct.len c && Cstruct.len c1 <= Cstruct.len c)
      | exception Invalid_argument _ -> Ok ()
    );
  add_test ~name:"BE.set_uint64" [cstruct; int] (fun c off ->
      let in_range = off >= 0 && off < Cstruct.len c - 7 in
      match Cstruct.BE.set_uint64 c off 42L with
      | () -> check in_range
      | exception Invalid_argument _ -> check (not in_range)
    );
  add_test ~name:"lenv" [List cstruct] (fun cs ->
      check (Cstruct.lenv cs >= 0)
    );
  add_test ~name:"copyv" [List cstruct] (fun cs ->
      ignore (Cstruct.copyv cs); Ok ()
    );
  add_test ~name:"fillv" [List cstruct; cstruct] (fun src dst ->
      let copied, rest = Cstruct.fillv ~src ~dst in
      check (copied + Cstruct.lenv rest = Cstruct.lenv src);
    );
  add_test ~name:"concat" [List cstruct] (fun cs ->
      let len = Cstruct.len (Cstruct.concat cs) in
      check (len >= 0 && len >= Cstruct.lenv cs);
    );
