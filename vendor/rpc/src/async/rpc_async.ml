open Idl
module Deferred = Async.Deferred

type async_rpcfn = Rpc.call -> Rpc.response Deferred.t

(* Construct a helper monad to hide the nasty 'a comp type *)
module M = struct
  type 'a async = { async: 'a Deferred.t }
  type ('a, 'b) t = ('a, 'b) Result.result async

  let return x = { async=Deferred.return (Result.Ok x) }
  let return_err e = { async=Deferred.return (Result.Error e)}
  let checked_bind x f f1 = { async=Deferred.bind x.async (function | Result.Ok x -> (f x).async | Result.Error x -> (f1 x).async) }
  let bind x f = checked_bind x f return_err
  let (>>=) x f = bind x f
  let deferred x = x.async
end

module GenClient () = struct
  type implementation = unit
  let description : Idl.Interface.description option ref = ref None

  let implement x = description := Some x

  exception MarshalError of string

  type ('a,'b) comp = ('a,'b) Result.result M.async
  type rpcfn = Rpc.call -> Rpc.response Deferred.t
  type 'a res = rpcfn -> 'a

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn

  let returning a err = Returning (a, err)
  let (@->) = fun t f -> Function (t, f)

  let declare name _ ty (rpc : rpcfn) =
    let open Result in
    let rec inner : type b. ((string * Rpc.t) list * Rpc.t list) -> b fn -> b = fun (named,unnamed) ->
      function
      | Function (t, f) -> begin
          fun v ->
            match t.Param.name with
            | Some n -> begin
                match t.Param.typedef.Rpc.Types.ty, v with
                | Rpc.Types.Option t1, None ->
                  inner (named, unnamed) f
                | Rpc.Types.Option t1, Some v' ->
                  let marshalled = Rpcmarshal.marshal t1 v' in
                  inner ((n, marshalled)::named,unnamed) f
                | ty, v ->
                  let marshalled = Rpcmarshal.marshal ty v in
                  inner ((n, marshalled)::named,unnamed) f
              end
            | None ->
              let marshalled = Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty v in
              inner (named,(marshalled::unnamed)) f
        end
      | Returning (t, e) ->
        let wire_name = get_wire_name !description name in
        let args =
          match named with
          | [] -> List.rev unnamed
          | _ -> (Rpc.Dict named) :: List.rev unnamed
        in
        let call = Rpc.call wire_name args in
        let res = Deferred.bind (rpc call) (fun r ->
            if r.Rpc.success
            then match Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty r.Rpc.contents with Ok x -> Deferred.return (Ok x) | Error (`Msg x) -> raise (MarshalError x)
            else match Rpcmarshal.unmarshal e.Idl.Error.def.Rpc.Types.ty r.Rpc.contents with Ok x -> Deferred.return (Error x) | Error (`Msg x) -> raise (MarshalError x)) in
        {M.async=res}
    in inner ([],[]) ty
end

exception MarshalError of string
exception UnknownMethod of string
exception UnboundImplementation of string list

type server_implementation = (string, async_rpcfn option) Hashtbl.t
let server hashtbl =
  let impl = Hashtbl.create (Hashtbl.length hashtbl) in
  let unbound_impls = Hashtbl.fold (fun key fn acc ->
      match fn with
      | None -> key::acc
      | Some fn -> Hashtbl.add impl key fn; acc
    ) hashtbl [] in
  if unbound_impls <> [] then
    raise (UnboundImplementation unbound_impls);
  fun call ->
    let fn = try Hashtbl.find impl call.Rpc.name with Not_found -> raise (UnknownMethod call.Rpc.name) in
    fn call

let combine hashtbls =
  let result = Hashtbl.create 16 in
  List.iter (Hashtbl.iter (fun k v -> Hashtbl.add result k v)) hashtbls;
  result

module GenServer () = struct
  open Rpc

  let funcs = Hashtbl.create 20
  type implementation = server_implementation
  let description : Idl.Interface.description option ref = ref None
  let implement x = description := Some x; funcs

  type ('a,'b) comp = ('a,'b) Result.result M.async
  type rpcfn = Rpc.call -> Rpc.response Deferred.t
  type funcs = (string, rpcfn option) Hashtbl.t
  type 'a res = 'a -> unit

  type _ fn =
    | Function : 'a Param.t * 'b fn -> ('a -> 'b) fn
    | Returning : ('a Param.t * 'b Idl.Error.t) -> ('a, 'b) M.t fn

  let returning a b = Returning (a,b)
  let (@->) = fun t f -> Function (t, f)

  let rec has_named_args : type a. a fn -> bool =
    function
    | Function (t, f) -> begin
        match t.Param.name with
        | Some n -> true
        | None -> has_named_args f
      end
    | Returning (t, e) ->
      false

  let declare : string -> string list -> 'a fn -> 'a res = fun name _ ty ->
    let open Rresult.R in
    (* We do not know the wire name yet as the description may still be unset *)
    Hashtbl.add funcs name None;
    fun impl ->
      begin
        (* Sanity check: ensure the description has been set before we declare
           any RPCs *)
        match !description with
        | Some _ -> ()
        | None -> raise NoDescription
      end;
      let rpcfn =
        let has_named = has_named_args ty in
        let rec inner : type a. a fn -> a -> call -> response Deferred.t = fun f impl call ->
          match f with
          | Function (t, f) -> begin
              let is_opt = match t.Param.typedef.Rpc.Types.ty with | Rpc.Types.Option _ -> true | _ -> false in
              let (arg_rpc, call') =
                match get_arg call has_named t.Param.name is_opt with
                | Result.Ok (x,y) -> (x,y)
                | Result.Error (`Msg m) -> raise (MarshalError m)
              in
              let z = Rpcmarshal.unmarshal t.Param.typedef.Rpc.Types.ty arg_rpc in
              match z with
              | Result.Ok arg -> inner f (impl arg) call'
              | Result.Error (`Msg m) -> raise (MarshalError m)
            end
          | Returning (t,e) -> begin
              Deferred.bind impl.M.async (function
                  | Result.Ok x -> Deferred.return (success (Rpcmarshal.marshal t.Param.typedef.Rpc.Types.ty x))
                  | Result.Error y -> Deferred.return (failure (Rpcmarshal.marshal e.Idl.Error.def.Rpc.Types.ty y)))
            end
        in inner ty impl
      in

      Hashtbl.remove funcs name;
      (* The wire name might be different from the name *)
      let wire_name = get_wire_name !description name in
      Hashtbl.add funcs wire_name (Some rpcfn)

end
