(* Examples from the documentation, this code is in public domain. *)

(* Sequential processing *)

let id ic oc =
  let i = Xmlm.make_input (`Channel ic) in
  let o = Xmlm.make_output (`Channel oc) in
  let rec pull i o depth =
    Xmlm.output o (Xmlm.peek i);
    match Xmlm.input i with
    | `El_start _ -> pull i o (depth + 1)
    | `El_end -> if depth = 1 then () else pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false
  in
  Xmlm.output o (Xmlm.input i); (* `Dtd *)
  pull i o 0;
  if not (Xmlm.eoi i) then invalid_arg "document not well-formed"

let id_seq ic oc =
  let i = Xmlm.make_input (`Channel ic) in
  let o = Xmlm.make_output ~nl:true (`Channel oc) in
  while not (Xmlm.eoi i) do Xmlm.output o (Xmlm.input i) done

let prune_docs prune_list ic oc =
  let i = Xmlm.make_input (`Channel ic) in
  let o = Xmlm.make_output ~nl:true (`Channel oc) in
  let copy i o = Xmlm.output o (Xmlm.input i) in
  let prune (name, _) = List.mem name prune_list in
  let rec process i o d =
    let rec skip i d = match Xmlm.input i with
    | `El_start _ -> skip i (d + 1)
    | `El_end -> if d = 1 then () else skip i (d - 1)
    | s -> skip i d
    in
    match Xmlm.peek i with
    | `El_start tag when prune tag -> skip i 0; process i o d
    | `El_start _ -> copy i o; process i o (d + 1)
    | `El_end -> copy i o; if d = 0 then () else process i o (d - 1)
    | `Data _ -> copy i o; process i o d
    | `Dtd _ -> assert false
  in
  let rec docs i o =
    copy i o; (* `Dtd *)
    copy i o; (* root start *)
    process i o 0;
    if Xmlm.eoi i then () else docs i o
  in
  docs i o

(* Tree processing *)

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i =
  let el tag childs = E (tag, childs)  in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let frag = function
  | E (tag, childs) -> `El (tag, childs)
  | D d -> `Data d
  in
  Xmlm.output_doc_tree frag o t

(* Tabular data processing. *)

type w3c_bureaucrat =
  { name : string;
    surname : string;
    honest : bool;
    obfuscation_level : float;
    trs : string list; }

let in_w3c_bureaucrats src =
  let i = Xmlm.make_input ~strip:true src in
  let tag n = ("", n), [] in
  let error () = invalid_arg "parse error" in
  let accept s i = if Xmlm.input i = s then () else error () in
  let rec i_seq el acc i = match Xmlm.peek i with
  | `El_start _ -> i_seq el ((el i) :: acc) i
  | `El_end -> List.rev acc
  | _ -> error ()
  in
  let i_el n i =
    accept (`El_start (tag n)) i;
    let d = match Xmlm.peek i with
    | `Data d -> ignore (Xmlm.input i); d
    | `El_end -> ""
    | _ -> error ()
    in
    accept (`El_end) i;
    d
  in
  let i_bureaucrat i =
    try
      accept (`El_start (tag "bureaucrat")) i;
      let name = i_el "name" i in
      let surname = i_el "surname" i in
      let honest = match Xmlm.peek i with
      | `El_start (("", "honest"), []) -> ignore (i_el "honest" i); true
      | _ -> false
      in
      let obf = float_of_string (i_el "obfuscation_level" i) in
      let trs = i_seq (i_el "tr") [] i in
      accept (`El_end) i;
      { name = name; surname = surname; honest = honest;
        obfuscation_level = obf; trs = trs }
    with
    | Failure _ -> error () (* float_of_string *)
  in
  accept (`Dtd None) i;
  accept (`El_start (tag "list")) i;
  let bl = i_seq i_bureaucrat [] i in
  accept (`El_end) i;
  if not (Xmlm.eoi i) then invalid_arg "more than one document";
  bl

let out_w3c_bureaucrats dst bl =
  let tag n = ("", n), [] in
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let out = Xmlm.output o in
  let o_el n d =
    out (`El_start (tag n));
    if d <> "" then out (`Data d);
    out `El_end
  in
  let o_bureaucrat b =
    out (`El_start (tag "bureaucrat"));
    o_el "name" b.name;
    o_el "surname" b.surname;
    if b.honest then o_el "honest" "";
    o_el "obfuscation_level" (string_of_float b.obfuscation_level);
    List.iter (o_el "tr") b.trs;
    out `El_end
  in
  out (`Dtd None);
  out (`El_start (tag "list"));
  List.iter o_bureaucrat bl;
  out (`El_end)
