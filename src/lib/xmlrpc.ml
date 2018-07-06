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

open Printf
open Rpc

let debug = ref false
let debug (fmt: ('a, unit, string, unit) format4) : 'a =
  kprintf (fun s -> if !debug then begin print_string s; print_newline (); flush stdout end) fmt

(* marshalling/unmarshalling code *)

(* The XML-RPC is not very clear about what characters can be in a string value ... *)
let encode =
  let translate = function
    | '>'    -> Some "&gt;"
    | '<'    -> Some "&lt;"
    | '&'    -> Some "&amp;"
    | '"'    -> Some "&quot;"
    | c when (c >= '\x20' && c <= '\xff') || c = '\x09' || c = '\x0a' || c = '\x0d'
      -> None
    | _      -> Some "" in
  Internals.encode translate

let rec add_value f = function
  | Null ->
    f "<value><nil/></value>"

  | Int i  ->
    f "<value>";
    f (Int64.to_string i);
    f "</value>"

  | Int32 i ->
    f "<value><i4>";
    f (Int32.to_string i);
    f "</i4></value>"

  | Bool b ->
    f "<value><boolean>";
    f (if b then "1" else "0");
    f "</boolean></value>"

  | Float d ->
    f "<value><double>";
    (* NB: "%g" loses a lot of precision (e.g. resulting in "1.32621e+09") *)
    f (Printf.sprintf "%.16g" d);
    f "</double></value>"

  | String s ->
    f "<value>";
    f (encode s);
    f "</value>"

  | DateTime s ->
    f "<value><dateTime.iso8601>";
    f s;
    f "</dateTime.iso8601></value>"

  | Enum l ->
    f "<value><array><data>";
    List.iter (add_value f) l;
    f "</data></array></value>"

  | Dict d ->
    let add_member (name, value) =
      f "<member><name>";
      f name;
      f "</name>";
      add_value f value;
      f "</member>"
    in
    f "<value><struct>";
    List.iter add_member d;
    f "</struct></value>"

let to_string x =
  let buf = Buffer.create 128 in
  add_value (Buffer.add_string buf) x;
  Buffer.contents buf

let to_a ~empty ~append x =
  let buf = empty () in
  add_value (fun s -> append buf s) x;
  buf

let string_of_call call =
  let module B = Buffer in
  let buf = B.create 1024 in
  let add = B.add_string buf in
  add "<?xml version=\"1.0\"?>";
  add "<methodCall><methodName>";
  add (encode call.name);
  add "</methodName><params>";
  List.iter (fun p ->
      add "<param>";
      add (to_string p);
      add "</param>"
    ) call.params;
  add "</params></methodCall>";
  B.contents buf

let add_response add response =
  let v = if response.success then
      Dict [ "Status", String "Success"; "Value", response.contents ]
    else
      Dict [ "Status", String "Failure"; "ErrorDescription", response.contents ] in
  add "<?xml version=\"1.0\"?><methodResponse><params><param>";
  to_a ~empty:(fun () -> ()) ~append:(fun _ s -> add s) v;
  add "</param></params></methodResponse>"

let string_of_response response =
  let module B = Buffer in
  let buf = B.create 256 in
  let add = B.add_string buf in
  add_response add response;
  B.contents buf

let a_of_response ~empty ~append response =
  let buf = empty () in
  let add s = append buf s in
  add_response add response;
  buf

exception Parse_error of string * string * Xmlm.input

let debug_input input =
  let buf = Buffer.create 1024 in
  let rec aux tags =
    if not (Xmlm.eoi input) then begin
      match Xmlm.input input with
      | `El_start ((_,tag),_) ->
        Buffer.add_string buf "<";
        Buffer.add_string buf tag;
        Buffer.add_string buf ">";
        aux (tag :: tags)
      | `El_end ->
        begin match tags with
          | []     ->
            Buffer.add_string buf "<?/>";
            aux tags
          | h :: t ->
            Buffer.add_string buf "</";
            Buffer.add_string buf h;
            Buffer.add_string buf ">";
            aux t
        end
      | `Data d ->
        Buffer.add_string buf d;
        aux tags
      | `Dtd _ ->
        aux tags end
  in
  aux [];
  Buffer.contents buf

let pretty_string_of_error got expected input =
  sprintf "Error: got '%s' while '%s' was expected when processing '%s'\n" got expected (debug_input input)

let parse_error got expected input =
  raise (Parse_error (got, expected, input))

module Parser = struct

  let is_empty s =
    let is_empty = ref true in
    for i = 0 to (String.length s - 1)
    do
      if s.[i] <> '\n' && s.[i] <> ' ' && s.[i] <> '\t' then is_empty := false
    done;
    !is_empty

  let rec skip_empty input =
    match Xmlm.peek input with
    | `Data d when is_empty d -> let _ = Xmlm.input input in skip_empty input
    | _                       -> ()

  (* Helpers *)
  let get_data input =
    match Xmlm.input input with
    | `Dtd _                -> parse_error "dtd" "data" input
    | `Data d               -> d
    | `El_start ((_,tag),_) -> parse_error (sprintf "open_tag(%s)" tag) "data" input
    | `El_end               -> ""

  let rec open_tag input =
    match Xmlm.input input with
    | `Dtd _                  -> open_tag input
    | `El_start ((_,tag),_)   -> tag
    | `Data d when is_empty d -> open_tag input
    | `Data d                 -> parse_error (sprintf "data(%s)" (String.escaped d)) "open_tag" input
    | `El_end                 -> parse_error "close_tag" "open_tag" input

  let rec close_tag tag input =
    match Xmlm.input input with
    | `Dtd _                  -> parse_error "dtd" (sprintf "close_tag(%s)" tag) input
    | `El_end                 -> ()
    | `El_start ((_,t),_)     -> parse_error (sprintf "open_tag(%s)" t) (sprintf "close_tag(%s)" tag) input
    | `Data d when is_empty d -> close_tag tag input
    | `Data d                 -> parse_error (sprintf "data(%s)" (String.escaped d)) (sprintf "close_tag(%s)" tag) input

  let empty_tag input = function
    | "string" -> String ""
    | "array"  -> Enum []
    | "struct" -> Dict []
    | "nil"    -> Null
    | "value"  -> String ""
    | tag      -> parse_error (sprintf "empty_%s" tag) tag input

  let map_tags f input =
    let tag = open_tag input in
    let r =
      if Xmlm.peek input = `El_end then
        empty_tag input tag
      else
        f input tag in
    close_tag tag input;
    r

  let map_tag tag f input =
    let t = open_tag input in
    if t = tag then begin
      let r = f input in
      close_tag tag input;
      r
    end else
      parse_error (sprintf "open_tag(%s)" t) (sprintf "open_tag(%s)" tag) input

  let name   input   = map_tag "name" get_data input
  let data   f input = map_tag "data" f input
  let value  f input =
    let t = open_tag input in
    if t = "value" then begin
      let r =
        match Xmlm.peek input with
        | `El_end -> Rpc.String ""
        | `Data d ->
          let _ = Xmlm.input input in
          if is_empty d && match Xmlm.peek input with `El_start _ -> true | _ -> false then
            f input
          else
            Rpc.String d
        | _       -> f input in
      close_tag "value" input;
      r
    end else
      parse_error "open_tag(value)" (sprintf "open_tag(%s)" t) input

  let members f input =
    let g input =
      let name  = name input in
      let value = f name input in
      (name, value) in
    let r = ref [] in
    skip_empty input;
    while Xmlm.peek input <> `El_end do
      r := map_tag "member" g input :: !r;
      skip_empty input;
    done;
    List.rev !r

  (* Constructors *)
  let make fn ?callback accu data =
    let r = fn data in
    match callback with
    | Some f -> f (List.rev accu) r; r
    | None   -> r

  let make_null   = make (fun ()   -> Null)
  let make_int    = make (fun data -> Int (Int64.of_string data))
  let make_bool   = make (fun data -> Bool (if data = "1" then true else false))
  let make_float  = make (fun data -> Float (float_of_string data))
  let make_string = make (fun data -> String data)
  let make_dateTime = make (fun data -> DateTime data)
  let make_enum   = make (fun data -> Enum data)
  let make_dict   = make (fun data -> Dict data)

  (* General parser functions *)
  let rec of_xml ?callback accu input =
    try value (map_tags (basic_types ?callback accu)) input
    with
    | Xmlm.Error ((a,b), e) as exn->
      eprintf "Characters %i--%i: %s\n%!" a b (Xmlm.error_message e);
      raise exn
    | e ->
      eprintf "%s\n%!" (Printexc.to_string e);
      raise e

  and basic_types ?callback accu input = function
    | "int"
    | "i8"
    | "i4"     -> make_int    ?callback accu (get_data input)
    | "boolean"-> make_bool   ?callback accu (get_data input)
    | "double" -> make_float  ?callback accu (get_data input)
    | "string" -> make_string ?callback accu (get_data input)
    | "dateTime.iso8601" -> make_dateTime ?callback accu (get_data input)
    | "array"  -> make_enum   ?callback accu (data (of_xmls ?callback accu) input)
    | "struct" -> make_dict   ?callback accu (members (fun name -> of_xml ?callback (name::accu)) input)
    | "nil"    -> make_null   ?callback accu ()
    | tag      -> parse_error (sprintf "open_tag(%s)" tag) "open_tag(int/i8/i4/boolean/double/string/dateTime.iso8601/array/struct/nil)" input

  and of_xmls ?callback accu input =
    let r = ref [] in
    skip_empty input;
    while Xmlm.peek input <> `El_end do
      r := of_xml ?callback accu input :: !r;
      skip_empty input;
    done;
    List.rev !r
end

let of_string ?callback str =
  let input = Xmlm.make_input (`String (0, str)) in
  begin match Xmlm.peek input with
    | `Dtd _ -> ignore (Xmlm.input input)
    | _      -> () end;
  Parser.of_xml ?callback [] input

let of_a ?callback ~next_char b =
  let aux () =
    match next_char b with
    | Some c -> int_of_char c
    | None   -> raise End_of_file
  in
  let input = Xmlm.make_input (`Fun aux) in
  Parser.of_xml ?callback [] input

let call_of_string ?callback str =
  let input = Xmlm.make_input (`String (0, str)) in
  begin match Xmlm.peek input with
    | `Dtd _ -> ignore (Xmlm.input input)
    | _      -> () end;
  let name = ref "" in
  let params = ref [] in
  Parser.map_tag "methodCall" (fun input ->
      name := Parser.map_tag "methodName" Parser.get_data input;
      Parser.map_tag "params" (fun input ->
          Parser.skip_empty input;
          while Xmlm.peek input <> `El_end do
            Parser.map_tag "param" (fun input -> params := (Parser.of_xml ?callback [] input) :: !params) input;
            Parser.skip_empty input;
          done;
        ) input
    ) input;
  call !name (List.rev !params)

let response_of_fault ?callback input =
  Parser.map_tag "fault" (fun input ->
      match Parser.of_xml ?callback [] input with
      | Dict d ->
        let fault_code = List.assoc "faultCode" d in
        let fault_string = List.assoc "faultString" d in
        failure ( Rpc.Enum [ String "fault"; fault_code; fault_string ] )
      | r      -> parse_error (to_string r) "fault" input
    ) input

let response_of_success ?callback input =
  Parser.map_tag "params" (fun input ->
      Parser.map_tag "param" (fun input ->
          match Parser.of_xml ?callback [] input with
          | Dict d ->
            if List.mem_assoc "Status" d && List.assoc "Status" d = String "Success" && List.mem_assoc "Value" d then
              success (List.assoc "Value" d)
            else if List.mem_assoc "Status" d && List.assoc "Status" d = String "Failure" && List.mem_assoc "ErrorDescription" d then
              failure (List.assoc "ErrorDescription" d)
            else
              success (Dict d)
          | v  -> success v
        ) input
    ) input

let response_of_input ?callback input =
  begin match Xmlm.peek input with
    | `Dtd _ -> ignore (Xmlm.input input)
    | _      -> () end;
  Parser.map_tag "methodResponse" (fun input ->
      Parser.skip_empty input;
      match Xmlm.peek input with
      | `El_start ((_,"params"),_) -> response_of_success ?callback input
      | `El_start ((_,"fault"),_)  -> response_of_fault ?callback input
      | `El_start ((_,tag),_)      -> parse_error (sprintf "open_tag(%s)" tag) "open_tag(fault/params)" input
      | `Data d                    -> parse_error (String.escaped d) "open_tag(fault/params)" input
      | `El_end                    -> parse_error "close_tag" "open_tag(fault/params)" input
      | `Dtd _                     -> parse_error "dtd" "open_tag(fault/params)" input
    ) input

let response_of_string ?callback str =
  let input = Xmlm.make_input (`String (0, str)) in
  response_of_input ?callback input

let response_of_in_channel ?callback chan =
  let input = Xmlm.make_input (`Channel chan) in
  response_of_input ?callback input
