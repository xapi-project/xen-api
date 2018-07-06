open Rpc.Types

type _ outerfn =
  | Function : 'a Idl.Param.t * 'b outerfn -> ('a -> 'b) outerfn
  | Returning : ('a Idl.Param.t * 'b Idl.Error.t) -> ('a, 'b) Result.result outerfn

module Method = struct
  type 'a t = {
    name : string;
    description : string list;
    ty : 'a outerfn
  }

  let rec find_inputs : type a. a outerfn -> Idl.Param.boxed list = fun m ->
    match m with
    | Function (x,y) -> (Idl.Param.Boxed x) :: find_inputs y
    | Returning _ -> []

  let rec find_output : type a. a outerfn -> Idl.Param.boxed = fun m ->
    match m with
    | Returning (x,_y) -> Idl.Param.Boxed x
    | Function (_x,y) -> find_output y

  let rec find_errors : type a. a outerfn -> Rpc.Types.boxed_def = fun m ->
    match m with
    | Returning (_x,y) -> Rpc.Types.BoxedDef (y.Idl.Error.def)
    | Function (_x,y) -> find_errors y
end

type boxed_fn =
  | BoxedFunction : 'a Method.t -> boxed_fn

module Interface = struct
  include Idl.Interface

  type t = {
    details : Idl.Interface.description;
    methods : boxed_fn list;
  }

  let prepend_arg : t -> 'a Idl.Param.t -> t = fun interface param ->
    let prepend : type b. b outerfn -> ('a -> b) outerfn = fun arg ->
      Function (param, arg)
    in
    {interface with methods = List.map (fun (BoxedFunction m) ->
         BoxedFunction Method.({ name = m.name; description = m.description; ty = prepend m.ty}))
         interface.methods}

  let rec all_types : t -> boxed_def list = fun i ->
    let all_inputs = List.map (function BoxedFunction f -> Method.(find_inputs f.ty)) i.methods in
    let all_outputs = List.map (function BoxedFunction f -> Method.(find_output f.ty)) i.methods in
    let all = List.concat (all_inputs @ [all_outputs]) in
    let types = List.map (fun (Idl.Param.Boxed p) -> BoxedDef p.Idl.Param.typedef) all in
    let rec setify = function
      | [] -> []
      | (x::xs) -> if List.mem x xs then setify xs else x::(setify xs)
    in setify types
end


module Interfaces = struct
  type t = {
    name : string;
    title : string;
    description : string list;
    type_decls : boxed_def list;
    interfaces : Interface.t list;
  }

  let empty name title description =
    { name; title; description; type_decls=[]; interfaces=[] }

  let add_interface i is =
    let typedefs = Interface.all_types i in
    let new_typedefs = List.filter
        (fun def -> not
            (List.exists
               (fun (BoxedDef def') ->
                  match def with
                  | BoxedDef d -> def'.name = d.name) is.type_decls)) typedefs in

    { is with type_decls = new_typedefs @ is.type_decls; interfaces = i :: is.interfaces }

  let create ~name ~title ~description ~interfaces =
    let i = empty name title description in
    List.fold_right add_interface interfaces i

end

exception Interface_not_described

module Gen () = struct
  type ('a,'b) comp = ('a,'b) Result.result
  type 'a fn = 'a outerfn
  type 'a res = unit
  type implementation = unit -> Interface.t

  let methods = ref []

  let implement i () =
    let n = i.Interface.name in
    if String.capitalize_ascii n <> n then failwith "Interface names must be capitalized";
    let i = Interface.({details=i; methods=(List.rev !methods)}) in
    i

  let returning a b  = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  let declare : string -> string list -> 'a fn -> 'a res = fun name description ty ->
    let m = BoxedFunction Method.({name; description; ty}) in
    methods := m :: !methods
end
