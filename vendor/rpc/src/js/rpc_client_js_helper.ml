(* This module uses the browser's native JSON parsing and converts
 * the result back to a Rpc.t type *)

(* This require quite a lot of trial-and-error to make work :-( *)

let keys obj =
  let arr = Js.Unsafe.meth_call (Js.Unsafe.variable "Object") "keys" [| Js.Unsafe.inject obj |] in
  List.map (Js.to_string) (Array.to_list (Js.to_array arr))

(* This is apparently the ECMAscript approved way of checking whether something is an array *)
let is_array obj =
  let str = Js.Unsafe.call (Js.Unsafe.variable "Object.prototype.toString") obj [||] in
  Js.to_string str = "[object Array]"

(* Magic to find out whether something is one of the Js_of_ocaml Javascript string types *)
let mlString_constr = Js.Unsafe.variable "MlString"
let is_string obj =
  Js.instanceof obj mlString_constr

(* Seems to work. I hope there's a better way of doing this! *)
let nullobj = Json.unsafe_input (Js.string "null")
let is_null obj = Json.output obj = Js.string "null"

let rec rpc_of_json json =
  let ty = Js.typeof json in
  match (Js.to_string ty) with
  | "object" ->
    if is_array json then begin
      let l = Array.to_list (Js.to_array json) in
      Rpc.Enum (List.map rpc_of_json l)				
    end else if is_string json then begin
      Rpc.String (Js.to_string (Js.Unsafe.coerce json))
    end else if is_null json then begin
      Rpc.Null
    end else begin
      let okeys = keys json in
      Rpc.Dict (List.map (fun x -> (x, rpc_of_json (Js.Unsafe.get json (Js.string x)))) okeys)
    end
  | "boolean" ->
    Rpc.Bool (Js.to_bool (Obj.magic json))
  | "number" ->
    (* Convert all numbers to strings - the generic Rpc-light layer can deal with this *)
    let str = Js.Unsafe.meth_call json "toString" [| |] in
    Rpc.String (Js.to_string str)
  | _ -> 
    (* Datetime maybe? *)
    Firebug.console##log (Js.string (Printf.sprintf "Ack! got %s" (Js.to_string ty)));
    Rpc.Bool false

let of_string s = rpc_of_json (Json.unsafe_input (Js.string s))

(* Here be lots of magic. This is mostly untested *)
let to_string rpc =
  let rec inner = function 
    | Rpc.Dict kvs ->
      let o = Json.unsafe_input (Js.string "{}") in
      List.iter (fun (x,y) -> Js.Unsafe.set o (Js.string x) (inner y)) kvs;
      o
    | Rpc.Int x -> Obj.magic (Js.string (Int64.to_string x))
    | Rpc.Int32 x -> Obj.magic x
    | Rpc.Float x -> Obj.magic (Js.string (string_of_float x))
    | Rpc.String x -> Obj.magic (Js.string x)
    | Rpc.Bool x -> Obj.magic (if x then Js._true else Js._false)
    | Rpc.DateTime x -> Obj.magic (Js.string x)
    | Rpc.Enum l -> Obj.magic (Js.array (Array.of_list (List.map inner l)))
    | Rpc.Null -> Obj.magic (Js.null)
  in Json.output (inner rpc)

let new_id =
  let count = ref 0l in
  (fun () -> count := Int32.add 1l !count; !count)

let string_of_call call =
  let json = Rpc.Dict [
      "method", Rpc.String call.Rpc.name;
      "params", Rpc.Enum call.Rpc.params;
      "id", Rpc.Int32 (new_id ());
    ] in
  Js.to_string (to_string json)

exception Malformed_method_response of string

let get name dict =
  if List.mem_assoc name dict then
    List.assoc name dict
  else begin
    if Rpc.get_debug ()
    then Printf.eprintf "%s was not found in the dictionary\n" name;
    let str = List.map (fun (n,_) -> Printf.sprintf "%s=..." n) dict in
    let str = Printf.sprintf "{%s}" (String.concat "," str) in
    raise (Malformed_method_response str)
  end

let response_of_string str =
  match of_string str with
  | Rpc.Dict d ->
    let result = get "result" d in
    let error = get "error" d in
    let (_:int64) = try match get "id" d with Rpc.Int i -> i | Rpc.String s -> Int64.of_string s | _ -> failwith "inconsistent input" with _ -> 
      Firebug.console##log (Js.string (Printf.sprintf "Weirdness: %s" (Rpc.to_string (get "id" d))));
      raise (Malformed_method_response "id") in
    begin match result, error with
      | v, Rpc.Null    -> Rpc.success v
      | Rpc.Null, v    -> Rpc.failure v
      | x,y        -> raise (Malformed_method_response (Printf.sprintf "<result=%s><error=%s>" (Rpc.to_string x) (Rpc.to_string y)))
    end
  | rpc -> 
    Firebug.console##log (Js.string (Rpc.to_string rpc));
    failwith "Bah"
