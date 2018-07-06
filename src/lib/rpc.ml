(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2006-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

let debug = ref false
let set_debug x = debug := x
let get_debug () = !debug

type t =
  | Int of int64
  | Int32 of int32
  | Bool of bool
  | Float of float
  | String of string
  | DateTime of string
  | Enum of t list
  | Dict of (string * t) list
  | Null

module Version = struct
  type t = int * int * int

  let compare (x,y,z) (x',y',z') =
    let cmp a b fn () =
      let c = compare a b in
      if c<>0 then c else fn ()
    in cmp x x' (cmp y y' (cmp z z' (fun () -> 0))) ()

end

module Types = struct
  type _ basic =
    | Int : int basic
    | Int32 : int32 basic
    | Int64 : int64 basic
    | Bool : bool basic
    | Float : float basic
    | String : string basic
    | Char : char basic

  type _ typ =
    | Basic : 'a basic -> 'a typ
    | DateTime : string typ
    | Array : 'a typ -> 'a array typ
    | List : 'a typ -> 'a list typ
    | Dict : 'a basic * 'b typ -> ('a * 'b) list typ
    | Unit : unit typ
    | Option : 'a typ -> 'a option typ
    | Tuple : 'a typ * 'b typ -> ('a * 'b) typ
    | Struct : 'a structure -> 'a typ
    | Variant : 'a variant -> 'a typ
    | Abstract : 'a abstract -> 'a typ

  (* A type definition has a name and description *)
  and 'a def = { name: string; description: string list; ty: 'a typ; }

  and boxed_def = BoxedDef : 'a def -> boxed_def

  and ('a, 's) field = {
    fname : string;
    fdescription : string list;
    fversion : Version.t option;
    field : 'a typ;
    fdefault : 'a option;
    fget : 's -> 'a; (* Lenses *)
    fset : 'a -> 's -> 's;
  }
  and 'a boxed_field = BoxedField : ('a, 's) field -> 's boxed_field
  and field_getter = {
    fget : 'a. string -> 'a typ -> ('a, Rresult.R.msg) Result.result;
  }
  and 'a structure = {
    sname : string;
    fields: 'a boxed_field list;
    version : Version.t option;
    constructor : field_getter -> ('a, Rresult.R.msg) Result.result;
  }
  and ('a, 's) tag = {
    tname : string;
    tdescription : string list;
    tversion : Version.t option;
    tcontents : 'a typ;
    tpreview : 's -> 'a option;
    treview : 'a -> 's;
  }
  and 'a boxed_tag = BoxedTag : ('a, 's) tag -> 's boxed_tag
  and tag_getter = {
    tget : 'a. 'a typ -> ('a, Rresult.R.msg) Result.result;
  }
  and 'a variant = {
    vname    : string;
    variants : 'a boxed_tag list;
    vdefault : 'a option;
    vversion : Version.t option;
    vconstructor : string -> tag_getter -> ('a, Rresult.R.msg) Result.result;
  }
  and 'a abstract = {
    aname : string;
    test_data : 'a list;
    rpc_of : 'a -> t;
    of_rpc : t -> ('a, Rresult.R.msg) Result.result;
  }

  let int    = { name="int";    ty=Basic Int;    description=["Native integer"]}
  let int32  = { name="int32";  ty=Basic Int32;  description=["32-bit integer"]}
  let int64  = { name="int64";  ty=Basic Int64;  description=["64-bit integer"]}
  let bool   = { name="bool";   ty=Basic Bool;   description=["Boolean"]}
  let float  = { name="float";  ty=Basic Float;  description=["Floating-point number"]}
  let string = { name="string"; ty=Basic String; description=["String"]}
  let char   = { name="char";   ty=Basic Char;   description=["Char"]}
  let unit   = { name="unit";   ty=Unit;         description=["Unit"]}
  let default_types = [BoxedDef int; BoxedDef int32; BoxedDef int64;
                       BoxedDef bool; BoxedDef float; BoxedDef string;
                       BoxedDef char; BoxedDef unit]
end

exception Runtime_error of string * t
exception Runtime_exception of string * string

let map_strings sep fn l = String.concat sep (List.map fn l)
let rec to_string t =
  let open Printf in
  match t with
  | Int i      -> sprintf "I(%Li)" i
  | Int32 i    -> sprintf "I32(%li)" i
  | Bool b     -> sprintf "B(%b)" b
  | Float f    -> sprintf "F(%g)" f
  | String s   -> sprintf "S(%s)" s
  | DateTime s -> sprintf "D(%s)" s
  | Enum ts    -> sprintf "[%s]" (map_strings ";" to_string ts)
  | Dict ts    -> sprintf "{%s}" (map_strings ";" (fun (s,t) -> sprintf "%s:%s" s (to_string t)) ts)
  | Null       -> "N"


let rpc_of_t x = x
let rpc_of_int64 i = Int i
let rpc_of_int32 i = Int (Int64.of_int32 i)
let rpc_of_int i = Int (Int64.of_int i)
let rpc_of_bool b = Bool b
let rpc_of_float f = Float f
let rpc_of_string s = String s
let rpc_of_dateTime s = DateTime s
let rpc_of_unit () = Null
let rpc_of_char x = Int (Int64.of_int (Char.code x))

let int64_of_rpc = function
  | Int i    -> i
  | String s -> Int64.of_string s
  | x -> failwith (Printf.sprintf "Expected int64, got '%s'" (to_string x))
let int32_of_rpc = function
  | Int i    -> Int64.to_int32 i
  | String s -> Int32.of_string s
  | x -> failwith (Printf.sprintf "Expected int32, got '%s'" (to_string x))
let int_of_rpc = function
  | Int i    -> Int64.to_int i
  | String s -> int_of_string s
  | x -> failwith (Printf.sprintf "Expected int, got '%s'" (to_string x))
let bool_of_rpc = function
  | Bool b -> b
  | x -> failwith (Printf.sprintf "Expected bool, got '%s'" (to_string x))
let float_of_rpc = function
  | Float f  -> f
  | Int i    -> Int64.to_float i
  | Int32 i  -> Int32.to_float i
  | String s -> float_of_string s
  | x -> failwith (Printf.sprintf "Expected float, got '%s'" (to_string x))
let string_of_rpc = function
  | String s -> s
  | x -> failwith (Printf.sprintf "Expected string, got '%s'" (to_string x))
let dateTime_of_rpc = function
  | DateTime s -> s
  | x -> failwith (Printf.sprintf "Expected DateTime, got '%s'" (to_string x))
let unit_of_rpc = function
  | Null -> ()
  | x -> failwith (Printf.sprintf "Expected unit, got '%s'" (to_string x))
let char_of_rpc x =
  let x = int_of_rpc x in
  if x < 0 || x > 255
  then failwith (Printf.sprintf "Char out of range (%d)" x)
  else Char.chr x
let t_of_rpc t = t
let lowerfn = function | String s -> String (String.lowercase_ascii s) | Enum (String s::ss) -> Enum ((String (String.lowercase_ascii s))::ss) | x -> x

module ResultUnmarshallers = struct
  open Rresult

  let int64_of_rpc = function
    | Int i    -> R.ok i
    | String s -> begin
        try R.ok (Int64.of_string s)
        with _ -> R.error_msg (Printf.sprintf "Expected int64, got string '%s'" s)
      end
    | x -> R.error_msg (Printf.sprintf "Expected int64, got '%s'" (to_string x))
  let int32_of_rpc = function
    | Int i    -> R.ok (Int64.to_int32 i)
    | String s -> begin
        try R.ok (Int32.of_string s)
        with _ -> R.error_msg (Printf.sprintf "Expected int32, got string '%s'" s)
      end
    | x -> R.error_msg (Printf.sprintf "Expected int32, got '%s'" (to_string x))
  let int_of_rpc = function
    | Int i    -> R.ok (Int64.to_int i)
    | String s -> begin
        try R.ok (int_of_string s)
        with _ -> R.error_msg (Printf.sprintf "Expected int, got string '%s'" s)
      end
    | x -> R.error_msg (Printf.sprintf "Expected int, got '%s'" (to_string x))
  let bool_of_rpc = function
    | Bool b -> R.ok b
    | x -> R.error_msg (Printf.sprintf "Expected bool, got '%s'" (to_string x))
  let float_of_rpc = function
    | Float f  -> R.ok f
    | Int i    -> R.ok (Int64.to_float i)
    | Int32 i  -> R.ok (Int32.to_float i)
    | String s -> begin
        try R.ok (float_of_string s)
        with _ -> R.error_msg (Printf.sprintf "Expected float, got string '%s'" s)
      end
    | x -> R.error_msg (Printf.sprintf "Expected float, got '%s'" (to_string x))
  let string_of_rpc = function
    | String s -> R.ok s
    | x -> R.error_msg (Printf.sprintf "Expected string, got '%s'" (to_string x))
  let dateTime_of_rpc = function
    | DateTime s -> R.ok s
    | x -> R.error_msg (Printf.sprintf "Expected DateTime, got '%s'" (to_string x))
  let unit_of_rpc = function
    | Null -> R.ok ()
    | x -> R.error_msg (Printf.sprintf "Expected unit, got '%s'" (to_string x))
  let char_of_rpc x =
    Rresult.R.bind (int_of_rpc x) (fun x ->
        if x < 0 || x > 255
        then R.error_msg (Printf.sprintf "Char out of range (%d)" x)
        else R.ok (Char.chr x))
  let t_of_rpc t = R.ok t

end

let struct_extend rpc default_rpc =
  match rpc, default_rpc with
  | Dict real, Dict default_fields ->
    Dict (List.fold_left (fun real (f, default) ->
      if List.mem_assoc f real
      then real
      else (f, default) :: real) real default_fields)
  | _, _ -> rpc

type callback = string list -> t -> unit

type call = {
  name: string;
  params: t list;
}

let call name params = { name = name; params = params }

let string_of_call call =
  Printf.sprintf "-> %s(%s)" call.name (String.concat "," (List.map to_string call.params))

type response = {
  success: bool;
  contents: t;
}

let string_of_response response =
  Printf.sprintf "<- %s(%s)" (if response.success then "success" else "failure") (to_string response.contents)

let success v = { success = true; contents = v }
let failure v = { success = false; contents = v }
