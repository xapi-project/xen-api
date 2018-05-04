(*
 * Copyright (C) 2018 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let write_str filename str =
  let oc = open_out filename in
  Printf.fprintf oc "%s" str;
  close_out oc

let read_str filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  s

open Idl

module type CONFIG = sig
  val test_data_path : string
  (** Path under which we look for or generate requests and responses. For example,
      if test_data_path = 'foo', this module will search for or generate requests
      matching 'foo/requests/<RPC name>.request.<n>' and responses matching
      'foo/responses/<RPC name>.response.<n>' *)
end

module type MARSHALLER = sig
  val string_of_call : Rpc.call -> string
  val call_of_string : string -> Rpc.call
  val string_of_response : Rpc.response -> string
  val response_of_string : string -> Rpc.response
  val to_string : Rpc.t -> string
  val of_string : string -> Rpc.t
end

(* Slightly annoyingly, both RPC modules have a slightly different signature. Fix it here *)
module TJsonrpc : MARSHALLER = struct
  include Jsonrpc
  let string_of_call call = string_of_call call
  let string_of_response response = string_of_response response
end

module TXmlrpc : MARSHALLER = struct
  include Xmlrpc
  let call_of_string s = call_of_string s
  let response_of_string s = response_of_string s
  let of_string s = of_string s
end



(** The following module implements test cases that write test
    RPC requests and responses in JSON that can be used to
    verify that subsequent versions of an API can still parse
    them.

    The test cases are obtained by obtaining the implementation
    of the module generated when applying the API functor to
    this module.

    The test data will be written to the path specified in the
    CONFIG module passed in *)
module GenTestData (C:CONFIG) (M:MARSHALLER) = struct
  type implementation = unit Alcotest.test_case list ref

  let tests : unit Alcotest.test_case list ref = ref []
  let description = ref None

  let implement x = description := Some x; tests

  type ('a,'b) comp = 'a
  type 'a res = unit
  type _ fn =
    | Function : 'a Idl.Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Idl.Param.t * 'b Idl.Error.t) -> ('a, _) comp fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  open M

  let declare name _ ty =
    let rec inner : type b. (((string * Rpc.t) list * Rpc.t list) list) -> b fn -> unit = fun params ->
      function
      | Function (t, f) -> begin
        let vs = Rpc_genfake.genall 2 (match t.Param.name with Some n -> n | None -> t.Param.typedef.Rpc.Types.name) t.Param.typedef.Rpc.Types.ty in
        let marshalled = List.map (fun v -> Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v) vs in
        match t.Param.name with
        | Some n ->
          inner
            (List.flatten
              (List.map
                (fun marshalled ->
                  match marshalled, t.Param.typedef.Rpc.Types.ty with
                  | Rpc.Enum [], Rpc.Types.Option _ ->
                    params
                  | Rpc.Enum [x], Rpc.Types.Option _ ->
                    List.map
                      (fun (named,unnamed) ->
                        (((n, x)::named),unnamed))
                      params
                  | _, _ ->
                    List.map
                      (fun (named,unnamed) ->
                        (((n,marshalled)::named),unnamed))
                      params
                ) marshalled
              )
            ) f
        | None -> inner (List.flatten (List.map (fun marshalled -> List.map (fun (named,unnamed) -> (named,(marshalled::unnamed))) params) marshalled)) f
      end
      | Returning (t, e) ->
        let wire_name = Idl.get_wire_name !description name in
        let calls = List.map
          (fun (named,unnamed) ->
            let args =
            match named with
            | [] -> List.rev unnamed
            | _ -> (Rpc.Dict named) :: List.rev unnamed
          in
          let call = Rpc.call wire_name args in
          call) params in
        List.iteri (fun i call ->
          let request_str = string_of_call call in
          write_str
            (Printf.sprintf "%s/requests/%s.request.%d" C.test_data_path wire_name i)
            request_str) calls;
        let vs = Rpc_genfake.genall 2 (match t.Param.name with Some n -> n | None -> t.Param.typedef.Rpc.Types.name) t.Param.typedef.Rpc.Types.ty in
        let marshalled_vs = List.map (fun v -> Rpc.success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v)) vs in
        let errs = Rpc_genfake.genall 2 "error" e.Error.def.Rpc.Types.ty in
        let marshalled_errs = List.map (fun err -> Rpc.failure (Rpcmarshal.marshal e.Error.def.Rpc.Types.ty err)) errs in
        List.iteri (fun i response ->
          let response_str = string_of_response response in
          write_str
            (Printf.sprintf "%s/responses/%s.response.%d" C.test_data_path wire_name i)
            response_str) (marshalled_vs @ marshalled_errs)
    in
    let test_fn () =
      let mkdir_safe p = begin try Unix.mkdir p 0o755 with Unix.Unix_error (EEXIST, _, _) -> () end in
      mkdir_safe C.test_data_path;
      mkdir_safe (Printf.sprintf "%s/requests" C.test_data_path);
      mkdir_safe (Printf.sprintf "%s/responses" C.test_data_path);
      inner [[],[]] ty in
    tests := (Printf.sprintf "Generate test data for '%s'" (Idl.get_wire_name !description name), `Quick, test_fn) :: !tests
end

let get_arg call has_named name is_opt =
  match has_named, name, call.Rpc.params with
  | true, Some n, (Rpc.Dict named)::unnamed -> begin
      match List.partition (fun (x,_) -> x = n) named with
      | (_,arg)::dups,others when is_opt ->
        Result.Ok (Rpc.Enum [arg], {call with Rpc.params = (Rpc.Dict (dups @ others))::unnamed })
      | [], _others when is_opt -> Result.Ok (Rpc.Enum [], call)
      | (_,arg)::dups,others ->
        Result.Ok (arg, {call with Rpc.params = (Rpc.Dict (dups @ others))::unnamed })
      | _,_ -> Result.Error (`Msg (Printf.sprintf "Expecting named argument '%s'" n))
    end
  | true, None, (Rpc.Dict named)::unnamed -> begin
      match unnamed with
      | head::tail -> Result.Ok (head, {call with Rpc.params = (Rpc.Dict named)::tail})
      | _ -> Result.Error (`Msg "Incorrect number of arguments")
    end
  | true, _, _ -> begin
      Result.Error (`Msg "Marshalling error: Expecting dict as first argument when named parameters exist")
    end
  | false, None, head::tail -> begin
      Result.Ok (head, {call with Rpc.params = tail})
    end
  | false, None, [] ->
    Result.Error (`Msg "Incorrect number of arguments")
  | false, Some _, _ ->
    failwith "Can't happen by construction"

exception NoDescription
exception MarshalError of string


(** The following module will generate alcotest test cases to verify
    that a set of requests and responses can be successfully parsed.

    The CONFIG module specifies the location for the test data as
    `test_data_path`. Requests and responses will be looked up in
    this location in the subdirectories `requests` and `responses`.
    The actual data must be in files following the naming convention
    <wire_name>.request.<n> and <wire_name>.response.<n>.

    The code here closely follows that of the GenServer module to
    ensure it accurately represents how the server would parse the
    json.
    *)
module TestOldRpcs (C : CONFIG) (M : MARSHALLER) = struct
  open Rpc
  type implementation = unit Alcotest.test_case list ref

  let tests : implementation = ref []
  let description = ref None

  let implement x = description := Some x; tests

  type ('a,'b) comp = unit
  type 'a res = unit

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Error.t) -> (_, _) comp fn

  let returning a b = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  open M

  let rec has_named_args : type a. a fn -> bool =
    function
    | Function (t, f) -> begin
        match t.Param.name with
        | Some _ -> true
        | None -> has_named_args f
      end
    | Returning (_, _) ->
      false

  let declare : string -> string list -> 'a fn -> _ res = fun name _ ty ->
    begin
      (* Sanity check: ensure the description has been set before we declare
         any RPCs *)
      match !description with
      | Some _ -> ()
      | None -> raise NoDescription
    end;

    let wire_name = Idl.get_wire_name !description name in

    let rec read_all path extension i =
      try
        let call =
          read_str (Printf.sprintf "%s/%s/%s.%s.%d" C.test_data_path path wire_name extension i) in
        call :: read_all path extension (i+1)
      with _ -> []
    in

    let calls = read_all "requests" "request" 0 |> List.map call_of_string in
    let responses = read_all "responses" "response" 0 |> List.map response_of_string in

    let verify : type a. a Rpc.Types.typ -> Rpc.t -> a = fun typ rpc ->
      let rec sort_dicts ty =
        let open Rpc in
        match ty with
        | Dict kvs' ->
          let kvs = List.map (fun (k,v) -> (k, sort_dicts v)) kvs' in
          Dict (List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) kvs)
        | Enum ts -> Enum (List.map sort_dicts ts)
        | _ -> ty
      in
      match Rpcmarshal.unmarshal typ rpc with
      | Ok x ->
        let check = Rpcmarshal.marshal typ x in
        if (to_string (sort_dicts check)) <> (to_string (sort_dicts rpc)) then begin
          let err = Printf.sprintf "Round-trip failed. Before: '%s' After: '%s'"
            (to_string rpc)
            (to_string check) in
          raise (MarshalError err)
        end;
        x
      | Error (`Msg m) ->
        raise (MarshalError m)
    in

    let testfn call response =
      let has_named = has_named_args ty in
      let rec inner : type a. a fn -> Rpc.call -> unit = fun f call ->
        match f with
        | Function (t, f) -> begin
            let (arg_rpc, call') =
              let is_opt = match t.Param.typedef.Rpc.Types.ty with Rpc.Types.Option _ -> true | _ -> false in
              match get_arg call has_named t.Param.name is_opt with
              | Result.Ok (x,y) -> (x,y)
              | Result.Error (`Msg m) -> raise (MarshalError m)
            in
            verify t.Param.typedef.Rpc.Types.ty arg_rpc |> ignore;
            inner f call'
          end
        | Returning (t,e) -> begin
          match response.success with
          | true ->
            verify t.Param.typedef.Rpc.Types.ty response.contents |> ignore
          | false ->
            verify e.Error.def.Rpc.Types.ty response.contents |> ignore
          end
      in inner ty call
    in
    (* Check all calls *)
    let request_tests =
      List.mapi (fun i call ->
        let response = List.hd responses in
        let name = Printf.sprintf "Check old request for '%s': %d" wire_name i in
        (name, `Quick, fun () -> testfn call response)) calls in
    (* Now check all responses *)
    let response_tests =
      List.mapi (fun i response ->
      let call = List.hd calls in
      let name = Printf.sprintf "Check old response for '%s': %d" wire_name i in
      (name, `Quick, fun () -> testfn call response)) responses in

    tests := !tests @ request_tests @ response_tests

end
