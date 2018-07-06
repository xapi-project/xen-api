open Format

exception Include_loop_detected of string
exception Of_sexp_error = Pre_sexp.Of_sexp_error
exception Macro_conv_error of exn * Sexp.t * [`expanded of Sexp.t]

let () =
  let open Sexp in
  Conv.Exn_converter.add ~finalise:false
    [%extension_constructor Macro_conv_error]
    (function
      | Macro_conv_error (exn, unexpanded, `expanded expanded) ->
        List [Atom "Sexplib.Macro.Macro_conv_error";
              List [Conv.sexp_of_exn exn;
                    unexpanded;
                    List [Atom "expanded"; expanded]]]
      | _ -> assert false)

let macro_error err t =
  Of_sexp_error (Failure (sprintf "Error evaluating macros: %s" err), t)

type 'a conv =
  [ `Result of 'a | `Error of exn * Sexp.t ]
type 'a annot_conv = (* 'a Sexp.Annotated.conv = *)
  [ `Result of 'a | `Error of exn * Sexp.Annotated.t ]

let sexp_of_conv sexp_of_a = function
  | `Result a -> Sexp.List [ Atom "Result"; a |> sexp_of_a ]
  | `Error (exn, sexp) ->
    List [ Atom "Error"; List [ Sexplib0.Sexp_conv.sexp_of_exn exn; sexp ] ]

let sexp_of_annot_conv sexp_of_a = function
  | `Result a -> Sexp.List [ Atom "Result"; a |> sexp_of_a ]
  | `Error (exn, annotated_sexp) ->
    List
      [ Atom "Error"
      ; List
          [ Sexplib0.Sexp_conv.sexp_of_exn exn
          ; annotated_sexp |> Sexp.Annotated.get_sexp ] ]

module List = struct

  (* Think about tail recursion when adding more list functions in here. *)

  let length = List.length
  let fold_left = List.fold_left
  let mem = List.mem
  let assq = List.assq
  let iter x ~f = List.iter f x
  let rev_append = List.rev_append
  let rev = List.rev
  let assoc = List.assoc

  let map l ~f =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl -> aux ((f hd) :: acc) tl
    in
    aux [] l

  let concat_map l ~f =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl -> aux (List.rev_append (f hd) acc) tl
    in
    aux [] l

  let rec find_map ~f xs =
    match xs with
    | [] -> None
    | x :: xs ->
      match f x with
      | Some x -> Some x
      | None -> find_map ~f xs

  let exists ~f xs = List.exists f xs

  let rec find_a_dup = function
    | [] -> None
    | x :: xs ->
      if List.mem x xs then Some x else find_a_dup xs
end
let (@) = `redefine_a_tail_rec_append_if_you_need_it
let _ = (@)

module Vars = struct
  include Set.Make (String)
  let add_list set xs =
    List.fold_left (fun vars v -> add v vars) set xs
  let of_list xs =
    add_list empty xs
end
(* Map from template names to template argument lists and bodies.  The argument
   lists are not necessary for the formal evaluation rules, but are useful to
   catch errors early. *)
module Bindings = Map.Make (String)

(* A physical association list mapping sexps after :include are inlined to sexps
   that they originate from.  This map allows us to recover the original sexp
   that gave rise to an error and to give a precise error location. *)
type trail = (Sexp.t * Sexp.t) list

let rec find_arg result trail =
  try find_arg (List.assq result trail) trail
  with Not_found -> result

let atom = function
  | Sexp.Atom str -> str
  | Sexp.List _ as t -> raise (macro_error "Atom expected" t)

let atoms = function
  | Sexp.Atom _ as t -> raise (macro_error "Atom list expected" t)
  | Sexp.List ts -> List.map ~f:atom ts

(* If [~raise_if_any:true], raise an error if a free variable is encountered. *)
let free_variables_gen ~raise_if_any ts =
  (* Tail-recursive w.r.t the number of sexps in a list, but not sexp depth. *)
  let rec free_in_list bound ts acc =
    match ts with
    | Sexp.List (Sexp.Atom ":let" :: v :: vs :: def) :: ts ->
      let acc = free_in_list (Vars.add_list bound (atoms vs)) def acc in
      free_in_list (Vars.add (atom v) bound) ts acc
    | t :: ts ->
      let acc = free bound t acc in
      free_in_list bound ts acc
    | [] -> acc
  and free bound t acc =
    match t with
    | Sexp.List (Sexp.Atom ":use" :: v :: args) ->
      let acc =
        if Vars.mem (atom v) bound
        then acc
        else if raise_if_any
        then
          let msg = "Undefined variable (included files cannot reference variables from outside)" in
          raise (macro_error msg v)
        else Vars.add (atom v) acc
      in
      List.fold_left (fun acc t -> free bound t acc) acc args
    | Sexp.List ts -> free_in_list bound ts acc
    | Sexp.Atom _ -> acc
  in
  free_in_list Vars.empty ts Vars.empty

let check_no_free_variables ts = ignore (free_variables_gen ~raise_if_any:true ts)
let free_variables ts = free_variables_gen ~raise_if_any:false ts

let expand_local_macros_exn ~trail ts =
  let add_result =
    match trail with
    | None -> fun ~arg:_ ~result:_ -> ()
    | Some ref -> fun ~arg ~result -> ref := (result, arg) :: !ref
  in
  (* tail-recursive *)
  let rec expand_list defs ts acc =
    match ts with
    | Sexp.List (Sexp.Atom ":let" :: v :: args :: def) as t :: ts ->
      if def = []
      then raise (macro_error "Empty let bodies not allowed" t);
      let v = atom v in
      let args = atoms args in
      let free = free_variables def in
      let args_set = Vars.of_list args in
      let unused = Vars.diff args_set free in
      if not (Vars.is_empty unused)
      then raise
             (macro_error (sprintf "Unused variables: %s"
                             (String.concat ", " (Vars.elements unused))) t);
      let undeclared = Vars.diff free args_set in
      if not (Vars.is_empty undeclared)
      then raise
             (macro_error (sprintf "Undeclared arguments in let: %s"
                             (String.concat ", " (Vars.elements undeclared))) t);
      begin match List.find_a_dup args with
      | None -> ()
      | Some dup ->
        raise (macro_error (sprintf "Duplicated let argument: %s" dup) t)
      end;
      expand_list (Bindings.add v (args, def) defs) ts acc
    | t :: ts ->
      expand_list defs ts (List.rev_append (expand defs t) acc)
    | [] -> List.rev acc
  and expand defs t =
    match t with
    | Sexp.Atom (":use" | ":let" | ":include" | ":concat" as s) ->
      raise (macro_error ("Unexpected " ^ s) t)
    | Sexp.Atom _ as t -> [t]
    | Sexp.List (Sexp.Atom ":use" :: v :: args) ->
      let split_arg = function
        | Sexp.List (Sexp.Atom v :: def) -> v, def
        | arg -> raise (macro_error "Malformed argument" arg)
      in
      let evaluate_and_bind arg_defs (v, def) =
        (* It is important we evaluate with respect to defs here, to avoid one
           argument shadowing the next one. *)
        let def = expand_list defs def [] in
        Bindings.add v ([], def) arg_defs
      in
      let formal_args, body =
        try Bindings.find (atom v) defs
        with Not_found -> raise (macro_error "Undefined variable" v)
      in
      let args = List.map ~f:split_arg args in
      let arg_names = List.map ~f:(fun (v, _) -> v) args in
      if arg_names <> formal_args then
        raise (macro_error
                 (sprintf ("Formal args of %s differ from supplied args,"
                           ^^ " formal args are [%s]")
                    (atom v)
                    (String.concat ", " formal_args))
                 t);
      let defs = List.fold_left evaluate_and_bind Bindings.empty args in
      expand_list defs body []
    | Sexp.List (Sexp.Atom ":concat" :: ts) as t ->
      let ts = expand_list defs ts [] in
      let ts =
        try List.map ~f:atom ts
        with _ ->
          let error =
            let appl = Sexp.List (Sexp.Atom ":concat" :: ts) in
            sprintf "Malformed concat application: %s" (Sexp.to_string appl)
          in
          raise (macro_error error t)
      in
      let result = Sexp.Atom (String.concat "" ts) in
      add_result ~arg:t ~result;
      [result]
    | Sexp.List ts ->
      let ts = expand_list defs ts [] in
      let result = Sexp.List ts in
      add_result ~arg:t ~result;
      [result]
  in
  expand_list Bindings.empty ts []

let expand_local_macros ts =
  try `Result (expand_local_macros_exn ts ~trail:None)
  with Of_sexp_error (e, t) -> `Error (e, t)

module type Sexp_loader = sig
  module Monad : sig
    type 'a t
    val return : 'a -> 'a t
    module Monad_infix : sig
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    end
    module List : sig
      val iter : 'a list -> f:('a -> unit t) -> unit t
      val map : 'a list -> f:('a -> 'b t) -> 'b list t
    end
  end
  val load_sexps           : string -> Sexp.t           list Monad.t
  val load_annotated_sexps : string -> Sexp.Annotated.t list Monad.t
end

module Loader (S : Sexp_loader) = struct
  module M = S.Monad
  open M.Monad_infix

  type 'a file_contents = (string * 'a) list
  type mode =
    [ `Fast of Sexp.t list file_contents
    | `Find_error of Sexp.Annotated.t list file_contents ]

  let make_absolute_path ~with_respect_to file =
    if Filename.is_relative file
    then Filename.concat (Filename.dirname with_respect_to) file
    else file

  let load_all_includes file : Sexp.t list file_contents M.t =
    let file_contents = ref [] in
    let rec load visited file =
      if List.mem file visited
      then raise (Include_loop_detected file);
      if List.mem file (List.map ~f:fst !file_contents)
      then M.return ()
      else begin
        S.load_sexps file
        >>= fun ts ->
        file_contents := (file, ts) :: !file_contents;
        M.List.iter ts ~f:(load_includes (file :: visited) file)
      end
    and load_includes visited file = function
      | Sexp.List [Sexp.Atom ":include"; Sexp.Atom include_file] ->
        let include_file = make_absolute_path ~with_respect_to:file include_file in
        load visited include_file
      | Sexp.List ts -> M.List.iter ts ~f:(load_includes visited file)
      | Sexp.Atom _ -> M.return ()
    in
    load [] file >>= fun () -> M.return !file_contents

  let load_all_annotated_includes file_contents
    : Sexp.Annotated.t list file_contents M.t =
    M.List.map file_contents ~f:(fun (file, _) ->
      S.load_annotated_sexps file >>= fun ts -> M.return (file, ts))

  let find_annotated bad_sexp annot_file_contents =
    List.find_map annot_file_contents ~f:(fun (file, annot_sexps) ->
      List.find_map annot_sexps ~f:(fun annot_sexp ->
        match Sexp.Annotated.find_sexp annot_sexp bad_sexp with
        | None -> None
        | Some annot_sexp -> Some (file, annot_sexp)))

  (* This function has to compute a transformation trail even though all of the returned
     errors are of the form [Of_sexp_error (_, t)] where [t] is a physical subexpression of
     the input, in the event where an error happens not during macro expansion but during
     conversion to ocaml values. *)
  let expand_and_convert ~multiple (mode : mode) file f =
    let trail = ref ([] : trail) in
    let add_result ~arg ~result =
      match mode with
      | `Fast _ -> ()
      | `Find_error _ -> trail := (result, arg) :: !trail
    in
    let file_contents =
      match mode with
      | `Fast file_contents -> file_contents
      | `Find_error annot_file_contents ->
        List.map ~f:(fun (file, annot_sexps) ->
          (file, List.map ~f:Sexp.Annotated.get_sexp annot_sexps))
          annot_file_contents
    in
    let rec inline_includes current_file = function
      | Sexp.Atom _ as t -> [t]
      (* We expand an :include in list context, because that corresponds to
         the naive string substitution semantics. *)
      | Sexp.List [Sexp.Atom ":include"; Sexp.Atom include_file] ->
        load_and_inline (make_absolute_path ~with_respect_to:current_file include_file)
      | Sexp.List ts as t ->
        let ts = List.concat_map ts ~f:(inline_includes current_file) in
        let t' = Sexp.List ts in
        add_result ~arg:t ~result:t';
        [t']
    and load_and_inline file =
      (* The lookup always succeeds, because [file_contents] is a result of
         [load_all_includes]. *)
      let ts =
        List.concat_map (List.assoc file file_contents) ~f:(inline_includes file)
      in
      (* This checks that, after expanding the includes of file1, file1 doesn't
         have any free variables. So if file1 is included in file2, it won't use
         any of the variable of file2 in scope where file1 is included.
         However, the inclusion of file1 may shadow variables from file2. *)
      check_no_free_variables ts;
      ts
    in
    let map_results ts ~f =
      if multiple then List.map ~f ts
      else match ts with
        | [t]-> [f t]
        | ts ->
          failwith (sprintf "wrong number of sexps in %s, expecting 1, got %d"
                      file (List.length ts))
    in
    match mode with
    | `Fast _ ->
      let ts = expand_local_macros_exn ~trail:None (load_and_inline file) in
      map_results ts ~f:(fun t -> `Result (f t))
    | `Find_error annot_file_contents ->
      let locate_error f =
        try `Result (f ())
        with Of_sexp_error (exc, bad_sexp) as e ->
          (* Find the original sexp that caused the error. *)
          let unexpanded_bad_sexp = find_arg bad_sexp !trail in
          match find_annotated unexpanded_bad_sexp annot_file_contents with
          | Some (file, unexpanded_bad_annot_sexp) ->
            let exc =
              match Sexp.Annotated.get_conv_exn ~file ~exc unexpanded_bad_annot_sexp with
              | Of_sexp_error (inner_exc, unexpanded_bad_sexp) as exc ->
                if bad_sexp = unexpanded_bad_sexp then exc
                else Macro_conv_error (inner_exc, unexpanded_bad_sexp, `expanded bad_sexp)
              | exc -> exc
            in
            `Error (exc, unexpanded_bad_annot_sexp)
          (* This case should never happen. *)
          | None -> raise e
      in
      let inline_and_expand () =
        expand_local_macros_exn ~trail:(Some trail) (load_and_inline file)
      in
      match locate_error inline_and_expand with
      | `Error _ as e -> [e]
      | `Result ts ->
        map_results ts ~f:(fun t -> locate_error (fun () -> f t))

  let load ~multiple file f =
    load_all_includes file
    >>= fun file_contents ->
    try M.return (expand_and_convert ~multiple (`Fast file_contents) file f)
    with Of_sexp_error _ as original_exn ->
      begin
        load_all_annotated_includes file_contents
        >>= fun annotated_file_contents ->
        let result =
          (expand_and_convert
             ~multiple (`Find_error annotated_file_contents) file f)
        in
        if List.exists result ~f:(function
          | `Result _ -> false
          | `Error _ -> true)
        then
          M.return result
        else
          (* Avoid returning success in the case there was an error.
             This can be bad e.g. when reading the input from a pipe. *)
          raise original_exn
      end

  let load_sexps_conv file f = load ~multiple:true file f

  let load_sexp_conv file f =
    load ~multiple:false file f
    >>= function
    | [a] -> M.return a
    | _ -> assert false
end

exception Error_in_file of string * exn
let () =
  Conv.Exn_converter.add ~finalise:false
    [%extension_constructor Error_in_file]
    (function
      | Error_in_file (file, exn) ->
        Sexp.List [Sexp.Atom ("Error in file " ^ file); Conv.sexp_of_exn exn]
      | _ -> assert false)

let add_error_location file = function
  | Sexp.Parse_error e ->
    let err_msg = sprintf "%s: %s" file e.Sexp.err_msg in
    Sexp.Parse_error { e with Sexp.err_msg }
  | Failure e ->
    Failure (sprintf "%s: %s" file e)
  | error -> Error_in_file (file, error)

module Simple_sexp_loader = struct
  module Monad = struct
    type 'a t = 'a
    let return a = a
    module Monad_infix = struct
      let ( >>= ) a f = f a
    end
    module List = List
  end

  let load_sexps file =
    try Sexp.load_sexps file
    with e -> raise (add_error_location file e)

  let load_annotated_sexps file =
    try Sexp.Annotated.load_sexps file
    with e -> raise (add_error_location file e)
end

module Simple_loader = Loader (Simple_sexp_loader)

let id a = a

let load_sexp_conv = Simple_loader.load_sexp_conv

let load_sexp_conv_exn file f =
  match load_sexp_conv file f with
  | `Result a -> a
  | `Error (exn, _) -> raise exn

let load_sexp file =
  load_sexp_conv_exn file id

let load_sexps_conv = Simple_loader.load_sexps_conv

let load_sexps_conv_exn file f =
  let results = load_sexps_conv file f in
  List.map results ~f:(function
    | `Error (exn, _) -> raise exn
    | `Result a -> a)

let load_sexps file =
  load_sexps_conv_exn file id
