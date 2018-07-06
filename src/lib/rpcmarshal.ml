(* Basic type definitions *)
open Rpc.Types

type err = [ `Msg of string ]

let tailrec_map f l = List.rev_map f l |> List.rev

let rec unmarshal : type a. a typ -> Rpc.t -> (a, err) Result.result  = fun t v ->
  let open Rpc in
  let open Result in
  let open Rresult.R in
  let open Rpc.ResultUnmarshallers in
  let list_helper typ l =
    List.fold_left (fun acc v ->
        match acc, unmarshal typ v with
        | (Ok a), (Ok v) -> Ok (v::a)
        | _, (Error (`Msg s)) -> Error (Rresult.R.msg (Printf.sprintf "Failed to unmarshal array: %s (when unmarshalling: %s)" s (Rpc.to_string v)))
        | x, _ -> x) (Ok []) l >>| List.rev
  in
  match t with
  | Basic Int -> int_of_rpc v
  | Basic Int32 -> int32_of_rpc v
  | Basic Int64 -> int64_of_rpc v
  | Basic Bool -> bool_of_rpc v
  | Basic Float -> float_of_rpc v
  | Basic String -> string_of_rpc v
  | Basic Char -> (int_of_rpc v) >>| Char.chr
  | DateTime -> dateTime_of_rpc v
  | Array typ -> begin
      match v with
      | Enum xs -> (list_helper typ xs) >>| Array.of_list
      | _ -> Rresult.R.error_msg "Expecting Array"
    end
  | List (Tuple (Basic String, typ)) -> begin
      match v with
      | Dict xs -> begin
          let keys = tailrec_map fst xs in
          let vs = tailrec_map snd xs in
          list_helper typ vs >>= fun vs ->
          return (List.combine keys vs)
        end
      | _ ->
        Rresult.R.error_msg "Unhandled"
    end
  | Dict (basic, typ) -> begin
      match v with
      | Dict xs -> begin
          match basic with
          | String ->
            let keys = tailrec_map fst xs in
            let vs = tailrec_map snd xs in
            list_helper typ vs >>= fun vs ->
            return (List.combine keys vs)
          | _ ->
            Rresult.R.error_msg "Expecting something other than a Dict type"
        end
      | _ ->
        Rresult.R.error_msg "Unhandled"
    end
  | List typ -> begin
      match v with
      | Enum xs -> list_helper typ xs
      | _ -> Rresult.R.error_msg "Expecting array"
    end
  | Unit -> unit_of_rpc v
  | Option t -> begin
      match v with
      | Enum [x] -> unmarshal t x >>= fun x -> return (Some x)
      | Enum [] -> return None
      | y -> Rresult.R.error_msg (Printf.sprintf "Expecting an Enum value, got '%s'" (Rpc.to_string y))
    end
  | Tuple (t1, t2) -> begin
      match v, t2 with
      | Rpc.Enum list, Tuple (_, _) ->
        unmarshal t1 (List.hd list) >>= fun v1 ->
        unmarshal t2 (Rpc.Enum (List.tl list)) >>= fun v2 ->
        Ok (v1, v2)
      | Rpc.Enum ([x;y]), _ ->
        unmarshal t1 x >>= fun v1 ->
        unmarshal t2 y >>= fun v2 ->
        Ok (v1,v2)
      | Rpc.Enum list, _ ->
        Rresult.R.error_msg "Too many items in a tuple!"
      | _, _ ->
        error_msg "Expecting Rpc.Enum when unmarshalling a tuple"
    end
  | Struct { constructor; sname } -> begin
      match v with
      | Rpc.Dict keys' ->
        let keys = List.map (fun (s,v) -> (String.lowercase_ascii s, v)) keys' in
        constructor { fget = (
            let x : type a. string -> a typ -> (a, Rresult.R.msg) Result.result  = fun s ty ->
              let s = String.lowercase_ascii s in
              match ty with
              | Option x -> begin try List.assoc s keys |> unmarshal x >>= fun o -> return (Some o) with _ -> return None end
              | y ->
                try List.assoc s keys |> unmarshal y
                with Not_found ->
                  error_msg
                    (Printf.sprintf
                       "No value found for key: '%s' when unmarshalling '%s'" s sname)
            in x
          ) }
      | _ ->
        error_msg (Printf.sprintf "Expecting Rpc.Dict when unmarshalling a '%s'" sname)
    end
  | Variant { vconstructor; variants } ->
    (match v with
     | Rpc.String name -> ok (name, Rpc.Null)
     | Rpc.Enum [ Rpc.String name; contents ] -> ok (name, contents)
     | _ -> error_msg "Expecting String or Enum when unmarshalling a variant")
    >>= fun (name, contents) ->
    let constr = { tget = fun typ -> unmarshal typ contents } in
    vconstructor name constr
  | Abstract { of_rpc } ->
    of_rpc v


let rec marshal : type a. a typ -> a -> Rpc.t = fun t v ->
  let open Rpc in
  let open Result in
  let rpc_of_basic : type a. a basic -> a -> Rpc.t = fun t v ->
    match t with
    | Int -> rpc_of_int v
    | Int32 -> rpc_of_int32 v
    | Int64 -> rpc_of_int64 v
    | Bool -> rpc_of_bool v
    | Float -> rpc_of_float v
    | String -> rpc_of_string v
    | Char -> rpc_of_int (Char.code v)
  in
  match t with
  | Basic t -> rpc_of_basic t v
  | DateTime -> rpc_of_dateTime v
  | Array typ -> Enum (tailrec_map (marshal typ) (Array.to_list v))
  | List (Tuple (Basic String, typ)) ->
    Dict (tailrec_map (fun (x,y) -> (x, marshal typ y)) v)
  | List typ -> Enum (tailrec_map (marshal typ) v)
  | Dict (String, typ) ->
    Rpc.Dict (tailrec_map (fun (k,v) -> (k, marshal typ v)) v)
  | Dict (basic, typ) ->
    Rpc.Enum (tailrec_map (fun (k,v) -> Rpc.Enum [ rpc_of_basic basic k; marshal typ v ]) v)
  | Unit -> rpc_of_unit v
  | Option ty ->
    Rpc.Enum (match v with | Some x -> [marshal ty x] | None -> [])
  | Tuple (x, (Tuple (_,_) as y)) -> begin
      match marshal y (snd v) with
      | Rpc.Enum xs -> Rpc.Enum ((marshal x (fst v))::xs)
      | _ -> failwith "Marshalling a tuple should always give an Enum"
    end
  | Tuple (x, y) ->
    Rpc.Enum [marshal x (fst v); marshal y (snd v)]
  | Struct { fields } -> begin
      let fields = List.fold_left (fun acc f ->
          match f with
          | BoxedField f -> begin
              let value = marshal f.field (f.fget v) in
              match f.field, value with
              | Option _, Rpc.Enum [] -> acc
              | Option _, Rpc.Enum [x] -> (f.fname, x)::acc
              | _, _ -> (f.fname, value)::acc
            end
        ) [] fields
      in Rpc.Dict fields
    end
  | Variant { variants } -> begin
      List.fold_left (fun acc t ->
          match t with
          | BoxedTag t ->
            match t.tpreview v with
            | Some x -> begin
                match marshal t.tcontents x with
                | Rpc.Null ->
                  Rpc.String t.tname
                | y ->
                  Rpc.Enum [ Rpc.String t.tname; y ]
              end
            | None -> acc) Rpc.Null variants
    end
  | Abstract { rpc_of } -> begin
      rpc_of v
    end


let ocaml_of_basic : type a. a basic -> string = function
  | Int64 -> "int64"
  | Int32 -> "int32"
  | Int -> "int"
  | String -> "string"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"

let rec ocaml_of_t : type a. a typ -> string = function
  | Basic b -> ocaml_of_basic b
  | DateTime -> "string"
  | Array t -> ocaml_of_t t ^ " list"
  | List t -> ocaml_of_t t ^ " list"
  | Dict (b,t) -> Printf.sprintf "(%s * %s) list" (ocaml_of_basic b) (ocaml_of_t t)
  | Unit -> "unit"
  | Option t -> ocaml_of_t t ^ " option"
  | Tuple (a,b) -> Printf.sprintf "(%s * %s)" (ocaml_of_t a) (ocaml_of_t b)
  | Struct { sname; fields } ->
    let fields = List.map (function
        | BoxedField f ->
          Printf.sprintf "%s: %s;" f.fname (ocaml_of_t f.field)) fields in
    Printf.sprintf "{ %s }" (String.concat " " fields)
  | Variant { variants } ->
    let tags = List.map (function
        | BoxedTag t ->
          Printf.sprintf "| %s (%s) (** %s *)" t.tname (ocaml_of_t t.tcontents) (String.concat " " t.tdescription)) variants in
    String.concat " " tags
  | Abstract _ ->
    "<abstract>"
